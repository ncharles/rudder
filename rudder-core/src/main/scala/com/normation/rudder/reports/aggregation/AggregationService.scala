/*
*************************************************************************************
* Copyright 2013 Normation SAS
*************************************************************************************
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU Affero GPL v3, the copyright holders add the following
* Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU Affero GPL v3
* licence, when you create a Related Module, this Related Module is
* not considered as a part of the work and may be distributed under the
* license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>.
*
*************************************************************************************
*/

package com.normation.rudder.reports.aggregation

import com.normation.rudder.repository.RuleExpectedReportsRepository
import com.normation.rudder.repository.ReportsRepository
import org.joda.time.DateTime
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.reports.RuleExpectedReports
import com.normation.inventory.domain.NodeId
import scala.collection.mutable.Buffer
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import org.joda.time.Duration
import java.sql.Timestamp
import net.liftweb.common.Loggable
import com.normation.rudder.domain.reports.bean.Reports._
import com.normation.utils.HashcodeCaching
import org.joda.time.Interval
import net.liftweb.common._
import com.normation.rudder.services.reports.ReportingService
import com.normation.rudder.domain.reports.bean.ReportType._

/**
 * That service contains most of the logic to merge
 * interval of same criticity together.
 */
class AggregationService(
    expectedReportRepository : RuleExpectedReportsRepository
  , reportsRepository        : ReportsRepository
  , reportingService         : ReportingService
  , aggregatedReportsRepository: AggregatedReportsRepository
  , updatesEntriesRepository : UpdatesEntriesRepository
  , reportDelay : Int // in hours
  , maxDays : Int // in days
) extends Loggable {
  import AggregationConstants._

  def getTimeInterval() : Option[Interval] = {

    val intervalStartDateTime = (updatesEntriesRepository.getAgregationUpdateTime) getOrElse {
      val expectedReports = expectedReportRepository.findExpectedReports(new DateTime(0), DateTime.now())
      //val oldestReportDate = reportsRepository.getOldestReports.getOrElse(None).map(_.executionDate).getOrElse(DateTime.now())
      expectedReports match {
        case Full(seq) => seq.sortWith((x,y)=> x.beginDate.isBefore(y.beginDate)).headOption.map(_.beginDate).getOrElse(DateTime.now)
        case _ => DateTime.now
      }
    }
    val reportLimit = DateTime.now.minusHours(reportDelay)
    val maxReportDate = intervalStartDateTime.plusDays(maxDays)

    if (intervalStartDateTime.isAfter(reportLimit))
      None
    else {
      val end = if (reportLimit.isAfter(maxReportDate)) maxReportDate else reportLimit
      Some(new Interval(intervalStartDateTime,end))
    }
  }




  def aggregateReports() : Unit = {

    def timer() : DateTime = DateTime.now
    def timeDiff(date:DateTime) = timer().getMillis() - date.getMillis()
    val beginTime = timer()

    logger.info("Starting the aggregation of the reports")
    getTimeInterval() match {
      case None =>
        logger.warn(s"Last aggregation happened less than ${reportDelay} hours ago, don't launch aggregation" )
      case Some(interval) =>
        val start = interval.getStart
        val end = interval.getEnd
        logger.debug(s"Fetching entries from ${start} to ${end}")

        // fetch the expected repo for this time
        val expectedReports = expectedReportRepository.findExpectedReports(
              start
            , end
            ) match {
          case Full(seq) => seq.sortWith((r1,r2) => r1.ruleId.value<r2.ruleId.value && r1.serial<r2.serial)
          case eb: EmptyBox =>
            val fail = eb ?~! "could not fetch expected reports"
            logger.error(s"Could not fetch expected reports from ${start} to ${end}, reason: ${fail.messageChain}")
            Seq()
        }

        logger.debug(s"${expectedReports.length} expected reports in the period")

        for (expectedReport <- expectedReports) {  // expected reports are grouped by CR
          val serial    = expectedReport.serial
          val beginDate = expectedReport.beginDate
          val endDate   = expectedReport.endDate.getOrElse("now")
          val ruleId    = expectedReport.ruleId.value
          logger.debug(s"Handling expected reports serial ${serial} from ${beginDate} to ${endDate}")

          // For the moment (2.6), the DirectivesOnNodes of the RuleExpectedReports is constrained
          // to have the same DirectiveId on all the Nodes (but each directive may have differents component/componentValue)
          // so we can simply fetch all the nodes there to iterate over here
          // CAUTION : with Rudder 2.7 or later, this may NOT be applicable anymore
          val nodes = expectedReport.directivesOnNodes.flatMap(x => x.nodeIds).toSet
            for (node <- nodes) {

              logger.debug(s"Fetching the already existant agregated reports for node ${node.value}")
              val now = timer()
              val aggregatedReports = aggregatedReportsRepository.getAggregatedReports(
                                         expectedReport.ruleId
                                       , expectedReport.serial
                                       , node
                                     ) match {
                                       case Full(seq) if seq.size > 0 =>
                                         seq
                                        // if there is no report, look for the latest we could have (maybe we always want the latest)
                                       case _ =>
                                         aggregatedReportsRepository.getLatestAggregatedReports(
                                             expectedReport.ruleId
                                           , node
                                         ).getOrElse(Seq())
                                     }
              logger.debug(s"Fetched ${aggregatedReports.length} aggregated reports for node ${node.value} on rule ${ruleId} for serial ${serial} in ${timeDiff(now)} milliseconds")

              // define the begin date from where we search
              val beginInterval = if (start.isAfter(beginDate)) start else beginDate

              val endInterval = expectedReport.endDate match {
                case None => end
                case Some(date) => if (date.isAfter(end)) end else date
              }

              logger.debug(s"Fetching the reports for node ${node.value} between ${beginInterval} and ${endInterval}")
              val now2 = DateTime.now
              val linearisedBatches : Seq[LinearisedNodeStatusReport] = for {
                batch <- reportingService.findReportsByRule(expectedReport.ruleId, Some(beginInterval), Some(endInterval))
                linearisedBatch <- LinearisedNodeStatusReport(batch)
              } yield linearisedBatch

              logger.debug(s"Fetched ${linearisedBatches.size} reports for node ${node.value} on rule ${ruleId} for serial ${serial} in ${timeDiff(now2)} milliseconds")

              logger.trace("looking for complete execution")
              val completeExecutions =  reportsRepository.findExecutionTimeByNode(node, beginInterval, Some(endInterval))

              // we deal with node/rule/directive/component value execution
              val linearisedExpectedReports = lineariseExpectedReport(node, expectedReport).map(x =>
                                                (KeyOfExecution.apply(x) -> x)
                                              ).toMap
              /*
               * This is a tricky part:
               * I create a mutable map of buffer (mutable) of aggregated reports
               * the KeyOfExecution are the same in the linearisedExpectedReports and in the aggregation report
               * the buffer of aggregated reports must be in ascendant order.
               * i'm wondering if I should order on the endtime or on the id
               */
              val aggregation = scala.collection.mutable.Map() ++
              aggregatedReports.groupBy(KeyOfExecution(_)).
              mapValues(_.sortWith( (x,y) => x.endTime.before(y.endTime)).map((_,false)).toBuffer)

              for ((expectedKey, expected) <- linearisedExpectedReports) {
                // a Seq of (executionTime, Seq(reports))
                // the filtering of the relevant reports (filtering on the serial because they match)
                val allNodeStatus = linearisedBatches.filter(KeyOfExecution(_) == expectedKey)
                .sortWith( (x,y) => x.executionDate.isBefore(y.executionDate))

                // Look for each execution time what we have
                for ( status  <- allNodeStatus) {
                  val isCompleteExecution = completeExecutions.contains(status.executionDate)
                      /* if (!isCompleteExecution)
                    logger.debug("The execution at %s for node %s is incomplete ! complete execution are : %s".format(status.executionDate.toString(), node.value,completeExecutions))*/
                      aggregation += ( expectedKey  ->
                                         aggregate(
                                             Some(status)
                                           , aggregation.get(expectedKey)
                                           , status.executionDate
                                           , isCompleteExecution
                                           , expectedReport.beginDate
                                           , expected)
                                         )
                  }

                // wooops, must look for the execution we _don't_ have also
                // this can be done by running against the expected reports, with a no reports.
                aggregation += ( KeyOfExecution(expected)-> aggregate(
                    None
                  , aggregation.get(expectedKey)
                  , endInterval
                  , true
                  , expectedReport.beginDate
                  , expected))

              }
              logger.debug("Saving the agregated reports")

              // and now we have to save this
              val now3 = DateTime.now
              val reportsToSave = aggregation.values.flatten.filter(_._2).map(_._1).toSeq
              aggregatedReportsRepository.saveAggregatedReports(reportsToSave)
              logger.debug(s"Saved ${reportsToSave.size} reports in ${timeDiff(now3)} milliseconds")
          }
        }
      updatesEntriesRepository.setAgregationUpdateTime(end)
      logger.info(s"Finished the aggregation in ${timeDiff(beginTime)} millisec")
    }

  }

  implicit  def toTimeStamp(d:DateTime) : Timestamp = new Timestamp(d.getMillis)

  /**
   * applicationDate : date at with the expectedreports are created
   * Caution, there is nothing that prevent to agregate with past reports, so chronology might get broken
   */
  def aggregate(
      report : Option[LinearisedNodeStatusReport]
    , agregated : Option[Buffer[(AggregatedReports,Boolean)]]
    , executionTime : DateTime
    , isComplete : Boolean
    , applicationDate : DateTime
    , expected : LinearisedExpectedReport
  ) : Buffer[(AggregatedReports,Boolean)] = {
    report match {
      case None => // no answers !
        agregated match {
          case None =>
            // nothing before ?
            // do we want to have a different behavior if there is incomplete response, with a massive gap ?
            // TODO : isn't it worth waiting the NoAnswerTime ?
            val report = AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                    expected.ruleId.value, expected.serial, expected.serial,
                    expected.component, expected.keyValue, if (isComplete) NoAnswerReportType else ErrorReportType,
                    "", "", applicationDate, executionTime)
            Buffer((report,true))

          case Some(buffer) =>
            val previous = buffer.sortWith((x,y) => x._1.endTime.before(y._1.endTime)).last._1
            // check the consistency of data
            if (new DateTime(previous.endTime).isAfter(executionTime)) {
                // the last report of aggregation for this node/rule finished after the one we are handling ?
                // this is really not what we wish
                logger.warn(s"Trying to agregate a non existent report from ${executionTime} to an agregate report from ${previous.startTime} to ${previous.endTime}")
                buffer
            } else {
              if (new DateTime(previous.endTime).plus(NoAnswerTime).isAfter(executionTime)) {
                if (isComplete) {
                  // ok, we can say that something not that bad happen
                  // will wait for the next message
                  buffer
                } else {
                  // so this is an error
                  addToBuffer(AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                        expected.ruleId.value, expected.serial, expected.serial,
                        expected.component, expected.keyValue, ErrorReportType,
                        "", "", executionTime, executionTime),
                        buffer)
                }
              } else {
                logger.trace("we have a long time without answer, when expecting for %s %s".format(expected.ruleId,expected.component ))
                logger.trace("long is %s to %s".format(previous.endTime, executionTime))
                // a long time without answer
                // add a noanswer from last execution
                 val extension = addToBuffer( AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                        expected.ruleId.value, expected.serial, expected.serial,
                        expected.component, expected.keyValue, NoAnswerReportType,
                        "", "", previous.endTime, executionTime),
                        buffer )

                  if (!isComplete) {
                   // add a no answer from last execution
                    // then add an error
                    addToBuffer( AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                        expected.ruleId.value, expected.serial, expected.serial,
                        expected.component, expected.keyValue,  ErrorReportType,
                        "", "", executionTime, executionTime),
                        extension )
                 } else {
                   extension
                 }
              }
            }
        }
        // if not complete, then it is an error
        // compare with last agregated (if any)

        // if complete
          // compare with last agregated
            // if less than 10 minutes : ok
            // if more than 10 minutes : no answer

      case Some(aReport) => // an answer !
        agregated match {
          case None =>
            // if the executionTime is just after the applicationdate, we say its applied
            if (applicationDate.plus(NoAnswerTime).isAfter(executionTime)) {
              val report = AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                  expected.ruleId.value, expected.serial, aReport.serial,
                  aReport.component, aReport.keyValue, aReport.reportType,
                  aReport.message, "", applicationDate, executionTime)
              Buffer((report,true))
            }
            else {
             val noAnswer = AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                  expected.ruleId.value, expected.serial, expected.serial,
                  aReport.component, aReport.keyValue, NoAnswerReportType,
                  "", "", applicationDate, executionTime)
             val report = AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                  expected.ruleId.value, expected.serial, aReport.serial,
                  aReport.component, aReport.keyValue, aReport.reportType,
                  aReport.message, "", executionTime, executionTime)
              Buffer( (noAnswer,true)// first a no answer
                    , (report,true)) // then the answer
            }

            /*
             *  fetch last aggregated
             * if not
             *   if applicationDate less than 10 minutes
             *     Create a Buffer with this status fromapplicatinDate
             *   if applicationDate more than 10 minutes
             *     Create a buffer with no answer, then this status
             * else
             *   if end time less than 10 minutes
             *     if same severity
             *       copy by changing endTime
             *      else
             *        the end time is the new one, and create a new entry
             *   else
             *     not create a no answer, and the new one
             */
          case Some(buffer) =>
            val previous = buffer.last._1
             // check the consistency of data
            if (new DateTime(previous.endTime).isAfter(executionTime)) {
              /*
               * the last report of aggregation for this node/cr finished after the one we are handling ?
               * this is really not what we wish
               */
              logger.warn(s"Trying to agregate a non existent report from ${executionTime} to an agregate report from ${previous.startTime} to ${previous.endTime}")
              buffer
            } else {
              // when was the last answer ?
              if (new DateTime(previous.endTime).plus(NoAnswerTime).isAfter(executionTime)) {
                /*
                 * either case, the endTime is now
                 * compare the status if the previous is recent
                 */
                val report = AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                    expected.ruleId.value, expected.serial, aReport.serial,
                    aReport.component, aReport.keyValue, aReport.reportType,
                    aReport.message, "", executionTime, executionTime)
                addToBuffer(report, buffer)
              } else {
                // no answer for more than 10 minutes !
                val noAnswer =
                  AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                    expected.ruleId.value, expected.serial, expected.serial,
                    aReport.component, aReport.keyValue, NoAnswerReportType,
                    "", "", previous.endTime, executionTime)
                val report =
                  AggregatedReports(expected.nodeId.value, expected.directiveId.value,
                    expected.ruleId.value, aReport.serial, aReport.serial,
                    aReport.component, aReport.keyValue, aReport.reportType,
                    aReport.message, "", executionTime, executionTime)
                addToBuffer(report, addToBuffer(noAnswer,buffer))
              }
            }
        }
    }

  }
  /**
   * Add the new advanced reports to the existing one
   * If it's the same type as earlier, simply extends one
   * If not, close the previous and add the new one
   */
  def addToBuffer(toAdd : AggregatedReports, buffer :Buffer[(AggregatedReports,Boolean)] ) : Buffer[(AggregatedReports,Boolean)] = {
    buffer.last match {
      case (report : AggregatedReports,_) if report.state == toAdd.state =>
        report.endTime = toAdd.endTime
        report.endSerial = toAdd.endSerial
        buffer.update(buffer.size - 1, (report,true))
        buffer
      case (report : AggregatedReports,_) =>
        report.endTime = toAdd.startTime
        buffer.update(buffer.size-1, (report,true))
        buffer :+ (toAdd,true)
    }
  }

  /**
   * unfold expected reports to have proper lines
   */
  def lineariseExpectedReport(
        nodeId : NodeId
      , aRuleExpectedReports : RuleExpectedReports
  ) : Seq[LinearisedExpectedReport] = {

    for {
      directivesOnNodes <- aRuleExpectedReports.directivesOnNodes
      directive <- directivesOnNodes.directiveExpectedReports
       component <- directive.components
       value <-component.componentsValues
    } yield {
      LinearisedExpectedReport(
          nodeId
        , directive.directiveId
        , aRuleExpectedReports.ruleId
        , aRuleExpectedReports.serial
        , component.componentName
        , value
      )
    }
  }
}

case class LinearisedNodeStatusReport(
    nodeId       : NodeId
  , ruleId       : RuleId
  , serial       : Int
  , directiveId  : DirectiveId
  , component    : String
  , keyValue     : String
  , reportType   : ReportType
  , message      : String
  , executionDate: DateTime
) extends HashcodeCaching

object LinearisedNodeStatusReport {

  def apply(nodeStatusReport :NodeStatusReport, serial:Int, execDate:DateTime) : Seq[LinearisedNodeStatusReport]= {
    for {
      directive <- nodeStatusReport.directives
      component <- directive.components ++ directive.unexpectedComponents
      keyValue  <- component.componentValues ++ component.unexpectedCptValues
    } yield {
      LinearisedNodeStatusReport(
          nodeStatusReport.nodeId
        , nodeStatusReport.ruleId
        , serial
        , directive.directiveId
        , component.component
        , keyValue.componentValue
        , keyValue.cptValueReportType
        , keyValue.message.mkString(";")
        , execDate)
    }
  }
  def apply(executionBatch:ExecutionBatch) : Seq[LinearisedNodeStatusReport] =
    executionBatch.getNodeStatus.flatMap(LinearisedNodeStatusReport(_,executionBatch.serial,executionBatch.executionTime))
}


// a class not unlike the AggregatedReports
case class LinearisedExpectedReport(
      nodeId       : NodeId,
      directiveId  : DirectiveId,
      ruleId       : RuleId,
      serial       : Int,
      component    : String,
      keyValue     : String
) extends HashcodeCaching

// this class that identify the entry
case class KeyOfExecution(
       ruleId      : RuleId
    ,  directiveId : DirectiveId
    ,  component   : String
    ,  keyValue    : String
    ,  nodeId      : NodeId
) extends HashcodeCaching

object KeyOfExecution {
  def apply(entry : LinearisedExpectedReport) : KeyOfExecution =
    KeyOfExecution(entry.ruleId, entry.directiveId, entry.component, entry.keyValue,entry.nodeId)

  def apply(entry : LinearisedNodeStatusReport) : KeyOfExecution =
    KeyOfExecution(entry.ruleId, entry.directiveId, entry.component, entry.keyValue,entry.nodeId)

  def apply(entry : AggregatedReports) : KeyOfExecution =
    KeyOfExecution(
        RuleId(entry.ruleId),
        DirectiveId(entry.directiveId), entry.component, entry.keyValue,NodeId(entry.nodeId))

}