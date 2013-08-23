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

  def newAggregationfromReports (reports:Seq[Reports]) : (Seq[AggregatedReport],Seq[AggregatedReport])= {

    val res : Seq[(Seq[AggregatedReport],Seq[AggregatedReport])] =  reports.groupBy(_.ruleId).flatMap {
      case (ruleId, reportsByRule) =>
        val  executionTimeStamps = reportsByRule.map(_.executionTimestamp).distinct
        val firstExecutionTimeStamp = executionTimeStamps.minBy(_.getMillis())
        val lastExecutionTimeStamp = executionTimeStamps.maxBy(_.getMillis())
        val res : Seq[(Seq[AggregatedReport],Seq[AggregatedReport])] =  expectedReportRepository.findExpectedReports(ruleId, Some(firstExecutionTimeStamp), Some(lastExecutionTimeStamp)) match {
          case eb:EmptyBox => logger.error("couldn't not fetch expected Reports")
          Seq()
          case Full(expectedReports) =>
            logger.warn(expectedReports)
            val linearisedExpectedReports = expectedReports.flatMap(lineariseExpectedReport(_)).distinct
            val expectedMap = linearisedExpectedReports.groupBy(ReportKey(_))
            reportsByRule.groupBy(ReportKey(_)).map {
              case (key,reports) =>
                logger.info(expectedMap)
                expectedMap.get(key) match {
                  // There is no expected report => Map to UnexpectedReport
                  case None =>
                    (reports.groupBy(_.executionTimestamp).map{ case (_,reports) => AggregatedReport(reports.head,UnknownReportType,reports.size,reports.size)}.toSeq, Seq())
                  case Some(expectedReports) =>
                    val ReportsToAdd =  reports.groupBy(_.executionTimestamp).flatMap{
                      case (executionTimeStamp,reports) =>
                        val headReport = reports.head
                        expectedReports.find(_.serial == headReport.serial) match {
                          case None =>
                            Seq(AggregatedReport(headReport,UnknownReportType,reports.size,reports.size))
                          case Some(expected) =>
                            reports.groupBy(_.severity).map{
                              case(severity,reports) =>
                                val headReport = reports.head
                                AggregatedReport(headReport,ReportType(headReport),reports.size,reports.size)
                            }.toSeq
                        }
                    }.toSeq

                    val linearisedAggregated = linearisedExpectedReports.map{base =>
                      AggregatedReport (
                          base.nodeId.value
                        , base.directiveId.value
                        , base.ruleId.value
                        , base.serial
                        , base.serial
                        , base.component
                        , base.keyValue
                        , SuccessReportType
                        , ""
                        , toTimeStamp(base.startDate)
                        , toTimeStamp(base.endDate)
                        , 0
                        , base.cardinality
                    ) }
                    val merged = mergeAggregatedReports(linearisedAggregated, ReportsToAdd)

                    aggregatedReportsRepository.getAggregatedReportsByDate(ruleId, firstExecutionTimeStamp, lastExecutionTimeStamp) match {
                      case Full(aggregated) =>
                        mergeAggregatedReports(aggregated, merged._1)
                    }
                }
            }.toSeq
        }
     res
     }.toSeq

     (res.flatMap(_._1), res.flatMap(_._2))
  }


  def newAggregation = {
    updatesEntriesRepository.getAggregationStatus match {
      case Full(Some((lastReportId,lastReportDate))) =>
        val reports = reportsRepository.getReportsfromId(lastReportId, lastReportDate plusDays(maxDays)) match {
          case Full(reports) =>
            logger.warn(reports)
            if (!reports.isEmpty) {
            val (toSave,toDelete) = newAggregationfromReports(reports.map(_._1))
            logger.info(toSave.size.toString)
            logger.warn(toDelete.size.toString)
            aggregatedReportsRepository.saveAggregatedReports(toSave)
            logger.info(aggregatedReportsRepository.deleteAggregatedReports(toDelete))
            updatesEntriesRepository.setAggregationStatus(reports.map(_._2).max, reports.maxBy(_._2)._1.executionTimestamp)
            }
            else {
              logger.error("Nothing to do")
            }
          case _ => logger.error("error with reports")
        }


      case Full(None) => reportsRepository.getOldestReportWithId match {
        case Full(Some((report,id))) =>

          logger.info("Just Updating for now")
          updatesEntriesRepository.setAggregationStatus(id, report.executionTimestamp)
        case _ => logger.error("oulalah")
      }
      case eb:EmptyBox =>   reportsRepository.getOldestReportWithId match {
        case Full(Some((report,id))) =>

          logger.info("Just Updating for now")
         logger.warn( updatesEntriesRepository.setAggregationStatus(id, report.executionTimestamp))
        case _ => logger.error("oulalah")
      }

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


  def remergeConflict (baseSeq : Seq[AggregatedReport], reportsToMerge : Seq[AggregatedReport], beforeMerge : Option[AggregatedReport], afterMerge : Option[AggregatedReport]) = {
    def shouldRemoveBound (bound : Option[AggregatedReport] ) : Boolean = {
      bound match {
        case None => true
        case Some(report) => // Need merge only if the bound is empty and less than reportIntervalSize
          report.received == 0
      }
    }

    val reportsToSave = Buffer[AggregatedReport]()
    val reportsToDelete : Buffer[AggregatedReport] = Buffer[AggregatedReport]()
    //Mergebegin
    for {
      reportToMerge <- reportsToMerge

    } yield {
      val updatedReport = baseSeq.filter(baseReport => baseReport.endDate isAfter (reportToMerge.startDate minusMinutes reportInterval)) match {
        case mergeReports =>
        beforeMerge match {
          case Some(beforeReport) =>
            if (beforeReport.received == 0) {
              reportsToSave ++= mergeReports.map(_.copy(endTime = reportToMerge.startTime))
              reportsToDelete += beforeReport
            } else {
              // Nothing to save
            }
            case None =>
              // Nothing to save
        }
        mergeReports.find (mergeReport => mergeReport.state == reportToMerge.state && mergeReport.received == reportToMerge.received ) match {
            case Some(mergeReport) =>
              val reportMerged =  mergeReport.copy(endTime = reportToMerge.endTime)
              if (reportsToSave.contains(mergeReport)) {
                reportsToSave.update(reportsToSave.indexOf(mergeReport), reportMerged)
              } else {
                reportsToSave += reportMerged
              }
              reportsToDelete += reportToMerge
              reportMerged
            case None =>
              reportToMerge

        }
      }

      baseSeq.filter(baseReport => baseReport.startDate isBefore (updatedReport.endDate plusMinutes reportInterval)) match {
        case mergeReports =>
        val updatedReportAgain = afterMerge match {
          case Some(afterReport) =>
            if (afterReport.received == 0) {
              val updatedReportAgain = updatedReport.copy(endTime = afterReport.endTime)
              if (reportsToSave.contains(updatedReport)) {
                reportsToSave.update(reportsToSave.indexOf(updatedReport), updatedReportAgain)
              } else {
                reportsToSave += updatedReportAgain
              }
              reportsToDelete += afterReport
              updatedReportAgain
            } else {
              updatedReport
            }
          case None =>
            updatedReport
        }
        mergeReports.find (mergeReport => mergeReport.state == updatedReportAgain.state && mergeReport.received == updatedReportAgain.received ) match {
            case Some(mergeReport) =>
              val reportMerged =  updatedReportAgain.copy(endTime = mergeReport.endTime)
              if (reportsToSave.contains(updatedReportAgain)) {
                reportsToSave.update(reportsToSave.indexOf(updatedReportAgain), reportMerged)
              } else {
                reportsToSave += reportMerged
              }
              reportsToDelete += mergeReport

            case None =>
              // Nothing to change
        }
      }
    }


    (reportsToSave.distinct, reportsToDelete.distinct)

  }

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

  def mergeOneAggregatedReport ( base : Seq[AggregatedReport], report : AggregatedReport ) : (Seq[AggregatedReport],Seq[AggregatedReport]) = {
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
  if (base.forall(_.endDate isBefore report.startDate)) {
    // Get last report from this list
    val maxEndDate : AggregatedReport = base.maxBy(_.endDate.getMillis())
    val reportsToSave = if (maxEndDate.endDate isAfter (report.startDate minusMinutes (2 * reportInterval))) {
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
      (base ++ Seq(createEmptyReport(report, maxEndDate.endTime, report.startTime), report))
    }
    (reportsToSave,Seq())
  } else {

    val (conflictingReports, noConfflictReports) = base.partition(baseReport => baseReport.interval == report.interval || baseReport.interval.overlaps(report.interval) || report.interval.overlaps(baseReport.interval)  )
    // No conflict, return all reports + the new one
    val (toSave,toRemove) = if (conflictingReports.size == 0) {
      (base ++ Seq(report), Seq())
    } else {
      val conflicted = conflictingReports.map(resolveconflictingReport(_, report))
      val res = conflicted.map(conflict => remergeConflict(base, conflict._2, conflict._1, conflict._3))
      (res.flatMap(_._1), res.flatMap(_._2))
    }
    (toSave,toRemove)
  }
  }

  def mergeAggregatedReports ( base : Seq[AggregatedReport], toMerge : Seq[AggregatedReport])  : (Seq[AggregatedReport], Seq[AggregatedReport])= {
    val baseMap = base.groupBy(ReportKey(_))
    val mergeMap = toMerge.groupBy(ReportKey(_))
    val mergingMap = (for {
      key <- (baseMap.keys ++ mergeMap.keys).toSeq.distinct
    } yield {
      key -> (baseMap.getOrElse(key, Seq()),mergeMap.getOrElse(key,Seq()))
    }).toMap

    val res : Seq[(Seq[AggregatedReport], Seq[AggregatedReport])] = mergingMap.map {
      case (_,(baseReports,Seq())) if baseReports.size > 0 => (Seq[AggregatedReport](),Seq[AggregatedReport]())
      case (_,(Seq(),mergeReports)) if mergeReports.size > 0 => (mergeReports,Seq[AggregatedReport]())
      // Should not happen
      case (_,(Seq(),Seq())) => (Seq[AggregatedReport](),Seq[AggregatedReport]())
      case (_,(baseReports,mergeReports)) =>
        val res = mergeReports.map(mergeOneAggregatedReport(baseReports,_))
        (res.flatMap(_._1).distinct, res.flatMap(_._2).distinct)
    }.toSeq

    (res.flatMap(_._1), res.flatMap(_._2))
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
        , aRuleExpectedReports.beginDate
        , aRuleExpectedReports.endDate.getOrElse(DateTime.now)
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
  , startDate    : DateTime
  , endDate      : DateTime
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