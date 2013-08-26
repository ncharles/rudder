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
*/package com.normation.rudder.reports.aggregation

import net.liftweb.common.Loggable
import org.joda.time.DateTime
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import net.liftweb.common._
import org.squeryl.KeyedEntity
import java.sql.Timestamp
import com.normation.rudder.domain.policies.RuleId
import com.normation.inventory.domain.NodeId
import org.joda.time.base.BaseDateTime
import net.liftweb.util.Helpers.tryo
import com.normation.rudder.repository.jdbc.SquerylConnectionProvider
import com.normation.rudder.domain.reports.bean.ReportType
import org.squeryl.customtypes.StringField
import com.normation.rudder.domain.reports.bean.Reports._
import com.normation.rudder.domain.reports.bean._
import org.squeryl.customtypes.CustomType

class AggregatedReportsJdbcRepository(
    sessionProvider : SquerylConnectionProvider
  , systemRules     : Seq[RuleId]
) extends AggregatedReportsRepository with Loggable {

  def toTimeStamp(d:BaseDateTime) : Timestamp = new Timestamp(d.getMillis)
  //val session = sessionProvider.getSession()
  //session.bindToCurrentThread

  def getAggregatedReports(ruleId : RuleId, serial:Int, nodeId: NodeId) : Box[Seq[AggregatedReport]] = {
    logger.debug("trying to get advanced reports for node %s on rule %s with serial %d".format(nodeId.value,ruleId.value,serial))
    try {sessionProvider.ourTransaction {
      val q = from(Reportings.reports)(entry =>
        where(entry.ruleId === ruleId.value
          and (entry.beginSerial lte serial)
          and (entry.endSerial gte serial)
          and entry.nodeId === nodeId.value)
        select(entry)
      )
      // trick : I really don't want to send a stream, but rather consume the resultset (i need to traverse it several time)
      Full(Seq[AggregatedReport]() ++ q)
    }
    } catch {
      case e:Exception  => logger.error("error while fetching advanced reports %s".format(e))
          Failure("error while fetching advanced reports %s".format(e))
    }
  }

  /**
   * Get the last aggregated report for a given Node/CR
   */
  def getLatestAggregatedReports(ruleId : RuleId, nodeId: NodeId) : Box[Seq[AggregatedReport]] = {
    try { sessionProvider.ourTransaction {
      val result = from(Reportings.reports)(result =>
        where(result.ruleId === ruleId.value
            and result.nodeId === nodeId.value
            and (result.endTime in (
            from(Reportings.reports)(entry =>
              where(entry.ruleId === ruleId.value
                and entry.nodeId === nodeId.value)
                compute(max(entry.endTime))
                )
              )
            )
         )
         select(result)
      )
      // trick : I really don't want to send a stream, but rather consume the resultset (i need to traverse it several time)
      Full(Seq[AggregatedReport]() ++ result)
    }
    } catch {
      case e:Exception  => logger.error("error while fetching advanced reports %s".format(e))
          Failure("error while fetching advanced reports %s".format(e))
    }
  }


  def createAggregatedReports(reports : Seq[AggregatedReport]) : Seq[AggregatedReport] = {
    sessionProvider.ourTransaction {
        reports.map(report => Reportings.reports.insert(report) )

    }
  }

  def deleteAggregatedReports(reports : Seq[AggregatedReport]) : Seq[Int] = {
    val toDelete = reports.filter(_.id != 0)
    sessionProvider.ourTransaction {
        toDelete.map(report => Reportings.reports.deleteWhere(line => line.id === report.id) )

    }
  }
  def updateAggregatedReports(reports : Seq[AggregatedReport]) : Seq[Int] = {
    sessionProvider.ourTransaction {
        reports.map(report => update(Reportings.reports)(entry =>
                where (entry.id === report.id)
                set(
                    entry.endTime 	:= report.endTime
                	, entry.endSerial := report.endSerial
                	, entry.state.value := report.state.value
                	, entry.received := report.received
                	, entry.expected := report.expected
                )
              )

            )

    }
  }

  /**
   * Save the reports :
   * create them if they don't exist
   * update them otherwise
   */
  def saveAggregatedReports(reports : Seq[AggregatedReport]) : Unit = {
    sessionProvider.ourTransaction {
        val updated = updateAggregatedReports(reports.filter(x => x.id != 0))
        val created = createAggregatedReports(reports.filter(x => x.id == 0))

    }
  }


  // Retrieve all the reports for a CR between two dates
  def getAggregatedReportsByDate(
      ruleId               : RuleId
    , beginDate            : DateTime
    , endDate              : DateTime
  ) : Box[Seq[AggregatedReport]] = {

    if(beginDate.getMillis >= endDate.getMillis) Full(Seq())
    else {

      // convert date to the proper format
      val start = toTimeStamp(beginDate.toDateMidnight)
      val end = toTimeStamp(endDate.withTime(23,59,59,999))

      tryo ( sessionProvider.ourTransaction {
        val q = from(Reportings.reports)(entry =>
          where(
              entry.ruleId === ruleId.value and
              entry.startTime < end and
              entry.endTime > start
          )
          select(entry)
        )


          // trick : I really don't want to send a stream, but rather consume the resultset (i need to traverse it several time)
          (Seq[AggregatedReport]() ++ q).map { x =>
                if (x.startTime.getTime() < start.getTime()) x.copy(startTime = start) else x
             }.map { x =>
                if ((x.endTime == null) || (x.endTime.getTime() > end.getTime())) x.copy(endTime = end) else x
             }

      } ) ?~! "Error when trying to get report information for configuration rule '%s' from date '%s' to date '%s'".format(ruleId, beginDate, endDate)
    }
  }

  // Retrieve all the reports for a given Node between two dates
  def getAggregatedReportsByNodeAndDate(
      nodeId      : NodeId
    , beginDate   : DateTime
    , endDate     : DateTime
  ) : Box[Seq[AggregatedReport]] = {

    if(beginDate.getMillis >= endDate.getMillis) Full(Seq())
    else {
      // List the rulesId of system rules
      //val systemRules = Seq("inventory-all", "hasPolicyServer-root", "root-DP")
      // convert date to the proper format
      val start = toTimeStamp(beginDate)
      val end = toTimeStamp(endDate)

      tryo ( sessionProvider.ourTransaction {
        val q = from(Reportings.reports)(entry =>
          where(
              entry.nodeId === nodeId.value and
              entry.startTime < end and
              entry.endTime > start and
              entry.ruleId.notIn(systemRules.map(x => x.value))
          )
          select(entry)
        )
        // trick : I really don't want to send a stream, but rather consume the resultset (i need to traverse it several time)
        (Seq[AggregatedReport]() ++ q).map { x =>
                if (x.startTime.getTime() < start.getTime()) x.copy(startTime = start) else x
           }.map { x =>
              if ((x.endTime == null) || (x.endTime.getTime() > end.getTime())) x.copy(endTime = end) else x
           }

      } ) ?~! "Error when trying to get report information for node '%s' from date '%s' to date '%s'".format(nodeId, beginDate, endDate)
    }
  }

  /**
   * Return true if the rule is applied to other reports passed on argument
   * It means that it returns true if the rule is not applied exactly and only to the group of node
   */
  def isNotAppliedOnlyToThisGroup(
      ruleId    : RuleId
    , reports   : Seq[AggregatedReport]
    , beginDate : DateTime
    , endDate   : DateTime
  ) : Box[Boolean] = {

    if(beginDate.getMillis >= endDate.getMillis) Failure("The End date is after the begin date")
    else {
      // convert date to the proper format
      val start = toTimeStamp(beginDate)
      val end = toTimeStamp(endDate)

      val reportsId = reports.map(x => x.id)

      tryo ( sessionProvider.ourTransaction {
        val q = from(Reportings.reports)(entry =>
          where(
              entry.ruleId === ruleId.value and
              entry.startTime < end and
              entry.endTime > start
          )
          select(entry.id)
        )

        (Seq[Long]() ++ q.toList).exists(id => !reports.map(_.id).contains(id))
      } ) ?~! "Error when trying to get report information for rule '%s' from date '%s' to date '%s'".format(ruleId, beginDate, endDate)
    }
  }

}


object Reportings extends Schema {
  val reports = table[AggregatedReport]("aggregatedreports")

    on(reports)(t => declare(
      t.id.is(autoIncremented("aggregatedreportsid"), primaryKey)
      )
      )
}

case class AggregatedSquerylReport (
    @Column("nodeid")      nodeId      : String
  , @Column("directiveid") directiveId : String
  , @Column("ruleid")      ruleId      : String
  , @Column("beginserial") beginSerial : Int
  , @Column("endserial")   endSerial   : Int
  , @Column("component")   component   : String
  , @Column("keyvalue")    keyValue    : String
  , @Column("state")       state       : String
  , @Column("message")     message     : String
  , @Column("starttime")   startTime   : Timestamp
  , @Column("endtime")     endTime     : Timestamp
  , @Column("received")    received    : Int
  , @Column("expected")    expected    : Int
) extends KeyedEntity[Long] {
  @Column("id")
  val id = 0L
}