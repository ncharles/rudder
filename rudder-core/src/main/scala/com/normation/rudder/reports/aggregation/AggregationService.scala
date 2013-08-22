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
import com.normation.rudder.domain.reports.RuleExpectedReports

/**
 * That service contains most of the logic to merge
 * interval of same criticity together.
 */
class AggregationService(
    expectedReportRepository : RuleExpectedReportsRepository
  , reportsRepository        : ReportsRepository
  , reportingService         : ReportingService
  , aggregatedReportsRepository: AggregatedReportsRepository
  , updatesEntriesRepository : AggregationStatusRepository
  , reportDelay : Int // in hours
  , maxDays : Int // in days
) extends Loggable {
  import AggregationConstants._


//  def checkReport(report: Reports, expectedReport : RuleExpectedReports) = {
//    if ( report.)

//  }
  val reportInterval : Int = 5

  def newAggregation = {
    updatesEntriesRepository.getAggregationStatus match {
      case Full(Some((lastReportId,lastReportDate))) =>
        val reports = reportsRepository.getReportsfromId(lastReportId, lastReportDate) match {
          case Full(reports) =>
            for {
              (ruleId, reportsByRule) <- reports.map(_._1).groupBy(_.ruleId)
              executionTimeStamps = reportsByRule.map(_.executionTimestamp).distinct
              firstExecutionTimeStamp = executionTimeStamps.minBy(_.getMillis())
              lastExecutionTimeStamp = executionTimeStamps.maxBy(_.getMillis())
              expectedReports <- expectedReportRepository.findExpectedReports(ruleId, Some(firstExecutionTimeStamp), Some(lastExecutionTimeStamp))
              lineariseExpectedReports  = expectedReports.flatMap(lineariseExpectedReport(_)).distinct
           //   aggregatedReports <- aggregatedReportsRepository.getAggregatedReportsByDate(ruleId, firstExecutionTimeStamp, lastExecutionTimeStamp)
              expectedMap = lineariseExpectedReports.groupBy(ReportKey(_))
              reportMap = reportsByRule.groupBy(ReportKey(_))
            //  aggregatedReportMap = aggregatedReports.groupBy(KeyOfExecution(_))
            } yield {
               for {
                 (key,reports) <- reportMap
               } yield {
                 // Check if there is an expected report
                 val ReportsToAdd : Seq[AggregatedReport] = expectedMap.get(key) match {
                   // There is no expected report => Map to UnexpectedReport
                   case None =>
                     reports.groupBy(_.executionTimestamp).map{ case (_,reports) => AggregatedReport(reports.head,UnknownReportType,reports.size,reports.size)}.toSeq
                   case Some(expectedReports) =>
                     reports.groupBy(_.executionTimestamp).flatMap{
                       case (executionTimeStamp,reports) =>
                         val headReport = reports.head
                             expectedReports.find(_.serial == headReport.serial) match {
                         case None => Seq(AggregatedReport(headReport,UnknownReportType,reports.size,reports.size))
                         case Some(expected) =>
                           val res = reports.groupBy(_.severity).map{
                           case(severity,reports) =>
                             val headReport = reports.head
                             AggregatedReport(headReport,ReportType(headReport),reports.size,reports.size)
                          }.toSeq
                          res
                       }
                     }.toSeq
                 }
               }
            }
          case _ =>
        }


      case Full(None) =>
      case eb:EmptyBox =>

    }

  }
/*
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

    , @Column("directiveid") directiveId: String
  , @Column("ruleid") ruleId: String
  , @Column("beginserial") beginSerial: Int
  , @Column("endserial") endSerial: Int
  , @Column("component") component: String
  , @Column("keyvalue") keyValue: String
  , state: DBReportType
  , @Column("message") message: String
  , @Column("starttime") startTime: Timestamp
  , @Column("endtime") endTime: Timestamp // only the endtime is mutable
  , @Column("received") received : Int
  , @Column("expected") expected : Int
*/

  def resolveconflictingReport (conflicting : AggregatedReport, report : AggregatedReport) : (Option[AggregatedReport],Seq[AggregatedReport],Option[AggregatedReport]) = {

    def splitBegin (base: AggregatedReport, newReportBegin: Timestamp) = {
      val splittedEnd =  AggregatedReport (
          base.nodeId
        , base.directiveId
        , base.ruleId
        , base.beginSerial
        , base.endSerial
        , base.component
        , base.keyValue
        , SuccessReportType
        , ""
        , newReportBegin
        , base.endTime
        , 0
        , base.expected
      )
      val splittedBegin = base.copy(endTime = newReportBegin)
      (splittedBegin,splittedEnd)
    }

    def splitEnd (base: AggregatedReport, newReportEnd: Timestamp) = {
      val splittedBegin =  AggregatedReport (
          base.nodeId
        , base.directiveId
        , base.ruleId
        , base.beginSerial
        , base.endSerial
        , base.component
        , base.keyValue
        , SuccessReportType
        , ""
        , base.startTime
        , newReportEnd
        , 0
        , base.expected
      )
      val splittedEnd = base.copy(startTime = newReportEnd)
      (splittedBegin,splittedEnd)
    }

    val (notConflictingBegin, conflictingEnd) = if (conflicting.startDate isBefore report.startDate) {
      val (notConflictingBegin, conflictingEnd) = splitEnd(conflicting,report.startTime)

      (Some(notConflictingBegin), conflictingEnd)
    } else {
      (None,conflicting)
    }

    val (notConflictingEnd, conflictingPart) = if (conflictingEnd.endDate isAfter report.endDate) {
      val ( conflictingPart, notConflictingEnd) = splitEnd(conflicting,report.startTime)

      (Some(notConflictingEnd), conflictingPart)
    } else {
      (None,conflictingEnd)

    }

    // Check if there is to much Report
    val newCount = report.received + conflictingPart.received
    val resolvedConflicts = if (newCount > report.expected) {
      // TOO MUCH PUT UNKNOWN WITH NEWCOUNT RECEIVED
      Seq(conflicting.copy(state = UnknownReportType, received = newCount, expected = newCount))
    } else {
      // Check if same state
      if (report.state == conflictingPart.state) {
        // Update receive Number and return
        Seq(conflictingPart.copy(received = newCount))
      } else {
        // check if both are empty (should not Happen empty reports whoudl already been treated because they should all get Success status (with 0 reports)
        if (newCount == 0){
          // Both empty report, Wrong report type here, repair it
          Seq(conflicting.copy(state = SuccessReportType))
        } else {
          // Still need to remove empty if there exists, There will still be at least one report in it
          Seq(conflictingPart,report).filter(_.received == 0)
        }
      }
    }

   (notConflictingBegin ,resolvedConflicts, notConflictingEnd)
  }

  def mergeOneAggregatedReport ( base : Seq[AggregatedReport], report : AggregatedReport ) : Seq [AggregatedReport] = {
    def createEmptyReport (base: AggregatedReport, begin : Timestamp, end: Timestamp) = {
      AggregatedReport (
          base.nodeId
        , base.directiveId
        , base.ruleId
        , base.beginSerial
        , base.endSerial
        , base.component
        , base.keyValue
        , SuccessReportType
        , ""
        , begin
        , end
        , 0
        , base.expected
      )
    }

  // Is the report after all aggregated reports
  val res : Seq[AggregatedReport] = if (base.forall(_.endDate isBefore report.startDate)) {
    // Get last report from this list
    val maxEndDate : AggregatedReport = base.maxBy(_.endDate.getMillis())
    if (maxEndDate.endDate isAfter (report.startDate minusMinutes (2 * reportInterval))) {
      // Check if same report has been received
      if (maxEndDate.state == report.state && maxEndDate.received == report.received) {
        // Extend actual report
        base.map{baseReport =>
        if (baseReport == maxEndDate) {
          baseReport.copy(endTime = report.endTime)
        } else {
          baseReport
        } }
      } else {
        // Check if this a no answer
        if (report.received == 0) {
          // Nothing to do, waiting for a new report
          base
        } else {
          // extend previous to beginning of new report and and this new report
          base.map{baseReport =>
            if (baseReport == maxEndDate) {
            baseReport.copy(endTime = report.startTime)
            } else {
              baseReport
          } } ++ Seq(report)
        }
      }
    } else {
      base ++ Seq(createEmptyReport(report, maxEndDate.endTime, report.startTime), report)
    }

  } else {
    // ATTENTION NE PAS MERGE/SPLIT SI le rapport est empty
    val (conflictingReports, noConfflictReports) = base.partition(baseReport => baseReport.interval == report.interval || baseReport.interval.overlaps(report.interval) || report.interval.overlaps(baseReport.interval)  )
    val conflicted = conflictingReports.map(resolveconflictingReport(_, report))
    val finalnoCOnficlts =
    for {
      resolvedConflict <- resolvedConflicts
      beginInterval = new Interval(resolvedConflict.startDate minusMinutes( 2 * reportInterval), resolvedConflict.startDate)
      (toMergeBeginWith,notOTmerge) = finalnoCOnficlts.partition(noConflict => beginInterval contains noConflict.endDate )
    } yield {
      val sameState = toMergeBeginWith.filter(toMerge => toMerge.state == resolvedConflict.state && toMerge.received == resolvedConflict.received)
      if (sameState.size == 1) {
        sameState.head.copy(endTime = )
      }
    }
    Seq()
  }
  Seq()

  }

  def mergeAggregatedReports ( base : Seq[AggregatedReport], toMerge : Seq[AggregatedReport]) = {
    val baseMap = base.groupBy(ReportKey(_))
    val mergeMap = toMerge.groupBy(ReportKey(_))
    val mergingMap = (for {
      key <- (baseMap.keys ++ mergeMap.keys).toSeq.distinct
    } yield {
      key -> (baseMap.getOrElse(key, Seq()),mergeMap.getOrElse(key,Seq()))
    }).toMap

    mergingMap.map {
      case (_,(baseReports,Seq())) if baseReports.size > 0 => baseReports
      case (_,(Seq(),mergeReports)) if mergeReports.size > 0 => mergeReports
      // Should not happen
      case (_,(Seq(),Seq())) => Seq()
      case (_,(baseReports,mergeReports)) =>
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
              aggregatedReports.groupBy(ReportKey(_)).
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
    , agregated : Option[Buffer[(AggregatedReport,Boolean)]]
    , executionTime : DateTime
    , isComplete : Boolean
    , applicationDate : DateTime
    , expected : LinearisedExpectedReport
  ) : Buffer[(AggregatedReport,Boolean)] = {
    report match {
      case None => // no answers !
        agregated match {
          case None =>
            // nothing before ?
            // do we want to have a different behavior if there is incomplete response, with a massive gap ?
            // TODO : isn't it worth waiting the NoAnswerTime ?
            val report = AggregatedReport(expected.nodeId.value, expected.directiveId.value,
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
                  addToBuffer(AggregatedReport(expected.nodeId.value, expected.directiveId.value,
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
                 val extension = addToBuffer( AggregatedReport(expected.nodeId.value, expected.directiveId.value,
                        expected.ruleId.value, expected.serial, expected.serial,
                        expected.component, expected.keyValue, NoAnswerReportType,
                        "", "", previous.endTime, executionTime),
                        buffer )

                  if (!isComplete) {
                   // add a no answer from last execution
                    // then add an error
                    addToBuffer( AggregatedReport(expected.nodeId.value, expected.directiveId.value,
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
              val report = AggregatedReport(expected.nodeId.value, expected.directiveId.value,
                  expected.ruleId.value, expected.serial, aReport.serial,
                  aReport.component, aReport.keyValue, aReport.reportType,
                  aReport.message, "", applicationDate, executionTime)
              Buffer((report,true))
            }
            else {
             val noAnswer = AggregatedReport(expected.nodeId.value, expected.directiveId.value,
                  expected.ruleId.value, expected.serial, expected.serial,
                  aReport.component, aReport.keyValue, NoAnswerReportType,
                  "", "", applicationDate, executionTime)
             val report = AggregatedReport(expected.nodeId.value, expected.directiveId.value,
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
                val report = AggregatedReport(expected.nodeId.value, expected.directiveId.value,
                    expected.ruleId.value, expected.serial, aReport.serial,
                    aReport.component, aReport.keyValue, aReport.reportType,
                    aReport.message, "", executionTime, executionTime)
                addToBuffer(report, buffer)
              } else {
                // no answer for more than 10 minutes !
                val noAnswer =
                  AggregatedReport(expected.nodeId.value, expected.directiveId.value,
                    expected.ruleId.value, expected.serial, expected.serial,
                    aReport.component, aReport.keyValue, NoAnswerReportType,
                    "", "", previous.endTime, executionTime)
                val report =
                  AggregatedReport(expected.nodeId.value, expected.directiveId.value,
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
  def addToBuffer(toAdd : AggregatedReport, buffer :Buffer[(AggregatedReport,Boolean)] ) : Buffer[(AggregatedReport,Boolean)] = {
    buffer.last match {
      case (report : AggregatedReport,_) if report.state == toAdd.state =>
        report.endTime = toAdd.endTime
        report.endSerial = toAdd.endSerial
        buffer.update(buffer.size - 1, (report,true))
        buffer
      case (report : AggregatedReport,_) =>
        report.endTime = toAdd.startTime
        buffer.update(buffer.size-1, (report,true))
        buffer :+ (toAdd,true)
    }
  }

  /**
   * unfold expected reports to have proper lines
   */
  def lineariseExpectedReport(
      aRuleExpectedReports : RuleExpectedReports
  ) : Seq[LinearisedExpectedReport] = {

    for {
      directivesOnNodes <- aRuleExpectedReports.directivesOnNodes
      directive <- directivesOnNodes.directiveExpectedReports
      nodeId <- directivesOnNodes.nodeIds
       component <- directive.components
       value <-component.componentsValues
    } yield {
      LinearisedExpectedReport(
          nodeId
        , directive.directiveId
        , aRuleExpectedReports.ruleId
        , aRuleExpectedReports.serial
        , component.componentName
        , component.cardinality
        , value
      )
    }
  }
}

//case class MissingReports (expected : Int, received : Int) => Sérialisé en _received_expected pour stocker le nombre

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
    nodeId       : NodeId
  , directiveId  : DirectiveId
  , ruleId       : RuleId
  , serial       : Int
  , component    : String
  , cardinality  : Int
  , keyValue     : String
) extends HashcodeCaching

// this class that identify the entry
case class ReportKey(
       ruleId      : RuleId
    ,  directiveId : DirectiveId
    ,  component   : String
    ,  keyValue    : String
    ,  nodeId      : NodeId
) extends HashcodeCaching

object ReportKey {
  def apply(entry : LinearisedExpectedReport) : ReportKey =
    ReportKey(entry.ruleId, entry.directiveId, entry.component, entry.keyValue, entry.nodeId)

  def apply(entry : LinearisedNodeStatusReport) : ReportKey =
    ReportKey(entry.ruleId, entry.directiveId, entry.component, entry.keyValue, entry.nodeId)

  def apply(entry : Reports) : ReportKey =
    ReportKey(entry.ruleId, entry.directiveId, entry.component, entry.keyValue, entry.nodeId)

  def apply(entry : AggregatedReport) : ReportKey =
    ReportKey(
        RuleId(entry.ruleId),        DirectiveId(entry.directiveId), entry.component, entry.keyValue, NodeId(entry.nodeId))

}