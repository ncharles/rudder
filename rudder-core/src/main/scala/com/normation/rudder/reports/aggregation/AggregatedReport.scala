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

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import java.sql.Timestamp
import org.squeryl.customtypes.CustomType
import com.normation.rudder.domain.reports.bean.ReportType._
import com.normation.rudder.domain.reports.bean.ReportType
import org.squeryl.customtypes.StringField
import com.normation.rudder.domain.reports.bean._
import com.normation.rudder.domain.reports.bean.Reports._
import AggregationConstants._
import org.joda.time.DateTime
import org.squeryl.customtypes.TimestampField
import org.joda.time.Interval


/**
 * An example of trait that can be mixed into CustomType,
 * to add meta data and validation
 */
trait Domain[A] {
  self: CustomType[A] =>

  def label: String
  def validate(a: A): Unit
  def value: A

  validate(value)
}

case class DBReportType(reportType:ReportType) extends StringField(ReportType.getSeverityFromStatus(reportType)) with Domain[String]{
      override def equals(obj: Any) = obj match {
        case DBReportType(report) => reportType eq report
        case report:ReportType => reportType eq report
        case _ => false
    }

   def this(status:String) = this(status match {
    // Use reports kind,
    case ERRORKEY => ErrorReportType
    case NOANSWERKEY => NoAnswerReportType
    case RESULT_SUCCESS => SuccessReportType
    case RESULT_REPAIRED => RepairedReportType
    case _ => ReportType(status)
  })

   val label = "state"
   def validate(s:String):Unit = {}
}

object DBReportType {

  def apply(status:String) = new DBReportType(status)
  implicit def reportTypeToDbReportType (reportType:ReportType) : DBReportType = DBReportType(reportType)
  implicit def dbreportTypeToReportType (reportType:DBReportType) : ReportType = reportType.reportType

  implicit def StringToDbReportType (status:String) : DBReportType = DBReportType(status)
}

case class AggregatedReport (
    @Column("nodeid") nodeId: String
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
) extends KeyedEntity[Long] {
  @Column("id")
  val id = 0L

/*  override def equals (that: Any) = { that match {
    case aggregated:AggregatedReport => true
    case _ => false
  } }*/
  override def hashCode = 0

  val startDate : DateTime = new DateTime(startTime)
  val endDate   : DateTime = new DateTime(endTime)
  val interval  : Interval = new Interval(startDate,endDate)
}

object AggregatedReport {

  def apply (report : Reports, reportType : ReportType, received:Int, expected:Int) : AggregatedReport = {
    val timestamp = toTimeStamp(report.executionTimestamp)
    AggregatedReport(
        report.nodeId.value
      , report.directiveId.value
      , report.ruleId.value
      , report.serial
      , report.serial
      , report.component
      , report.keyValue
      , reportType
      , report.message
      , timestamp
      , timestamp
      , received
      , expected

    )
  }
}