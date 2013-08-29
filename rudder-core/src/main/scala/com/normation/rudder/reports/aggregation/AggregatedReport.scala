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

import com.normation.rudder.domain.reports.bean.ReportType
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.reports.bean.Reports._
import AggregationConstants._
import org.joda.time.DateTime
import org.squeryl.customtypes.TimestampField
import org.joda.time.Interval
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.policies.RuleId
import net.liftweb.common.Loggable



case class AggregatedReport (
    nodeId      : NodeId
  , ruleId      : RuleId
  , beginSerial : Int
  , endSerial   : Int
  , directiveId : DirectiveId
  , component   : String
  , keyValue    : String
  , state       : ReportType
  , message     : String
  , startDate   : DateTime
  , endDate     : DateTime
  , received    : Int
  , expected    : Int
  , id          : Long
) extends Loggable {
  val interval  : Interval = {
    if (startDate .isAfter( endDate))
      logger.error(s"$startDate is over $endDate")

    new Interval(startDate,endDate plus(1))
  }

  def toSquerylEntity : AggregatedSquerylReport = {
    AggregatedSquerylReport(
        nodeId.value
      , ruleId.value
      , beginSerial
      , endSerial
      , directiveId.value
      , component
      , keyValue
      , state.severity
      , message
      , toTimeStamp(startDate)
      , toTimeStamp(endDate)
      , received
      , expected
      , id
    )
  }
}

object AggregatedReport {

  def apply (report : Reports, reportType : ReportType, received:Int, expected:Int) : AggregatedReport = {
    AggregatedReport(
        report.nodeId
      , report.ruleId
      , report.serial
      , report.serial
      , report.directiveId
      , report.component
      , report.keyValue
      , reportType
      , report.message
      , report.executionTimestamp
      , report.executionTimestamp
      , received
      , expected
      , 0
    )
  }

    def apply (report : AggregatedSquerylReport) : AggregatedReport = {
    AggregatedReport(
        NodeId(report.nodeId)
      , RuleId(report.ruleId)
      , report.beginSerial
      , report.endSerial
      , DirectiveId(report.directiveId)
      , report.component
      , report.keyValue
      , ReportType(report.state)
      , report.message
      , new DateTime(report.startTime)
      , new DateTime(report.endTime)
      , report.received
      , report.expected
      , report.id
    )
  }
}