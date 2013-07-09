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

package com.normation.rudder.migration


import java.sql._
import java.util.Calendar

import scala.Option.option2Iterable
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.xml._

import org.springframework.jdbc.core.RowMapper
import org.springframework.jdbc.core.BatchPreparedStatementSetter
import org.springframework.jdbc.core.JdbcTemplate
import org.squeryl.annotations.Column
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.KeyedEntity
import org.squeryl.Schema


import com.normation.rudder.domain.Constants
import com.normation.rudder.repository.jdbc.SquerylConnectionProvider
import com.normation.rudder.services.marshalling.TestFileFormat
import com.normation.utils.Control._
import com.normation.utils.XmlUtils
import com.normation.rudder.domain.logger._

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.IterableFunc.itNodeSeq
import net.liftweb.util.StringPromotable.intToStrPromo

case class MigrationChangeRequest(
    id           : Long
  , name         : String
  , content      : Elem
)

object MigrationChangeRequestMapper extends RowMapper[MigrationChangeRequest] {
  override def mapRow(rs : ResultSet, rowNum: Int) : MigrationChangeRequest = {
    MigrationChangeRequest(
        id          = rs.getLong("id")
      , name        = rs.getString("name")
      , content     = XML.load(rs.getSQLXML("content").getBinaryStream)
    )
  }
}

/**
 * The class that handle the processing of the list of all event logs
 * logic.
 * Each individual eventlog is processed in EventLogMigration_10_2
 *
 */
class ChangeRequestsMigration_3_4(
    jdbcTemplate               : JdbcTemplate
  , changeRequestMigration     : ChangeRequestMigration_3_4
  , errorLogger                : Failure => Unit
  , successLogger              : Seq[MigrationChangeRequest] => Unit
  , batchSize                  : Int = 1000
) {
 def logger = MigrationLogger(4)

  /**
   * retrieve all change request to migrate.
   */
  def findAllChangeRequests : Box[Seq[MigrationChangeRequest]] = {

    //check if the CR must be migrated
    def needMigration(xml:NodeSeq) : Boolean = (
      try {
        (xml \\ "@fileFormat" exists { _.text.toFloat == 3 })
      } catch {
        case e:NumberFormatException => false
      }
    )

    val SELECT_SQL_ALL_CHANGEREQUESTS =
      """
      |SELECT id, name, content FROM changerequest
      |""".stripMargin

    tryo(
        jdbcTemplate.query(SELECT_SQL_ALL_CHANGEREQUESTS, MigrationChangeRequestMapper).asScala
       .filter(cr => needMigration(cr.content))
    )
  }

  private[this] def saveChangeRequests(logs:Seq[MigrationChangeRequest]) : Box[Seq[MigrationChangeRequest]] = {
    val UPDATE_SQL = "UPDATE changerequest set content = ? where id = ?"

    val ilogs = logs match {
      case x:IndexedSeq[_] => logs
      case seq => seq.toIndexedSeq
    }

    tryo { jdbcTemplate.batchUpdate(
               UPDATE_SQL
             , new BatchPreparedStatementSetter() {
                 override def setValues(ps: PreparedStatement, i: Int): Unit = {
                   val sqlXml = ps.getConnection.createSQLXML()
                   sqlXml.setString(ilogs(i).content.toString)
                   ps.setSQLXML(1, sqlXml)
                   ps.setLong(2, ilogs(i).id )
                 }

                 override def getBatchSize() = ilogs.size
               }
    ) }.map( _ => ilogs )
  }

  /**
   * General algorithm: get all change request to migrate,
   * then process and save them.
   * Return the number of change request migrated
   */
  def processChangeRequests() : Box[Int] = {
    for {
      crs      <- findAllChangeRequests
      migrated <- saveResults(
                    crs = migrate(crs, errorLogger)
                  , saveChangeRequests = saveChangeRequests
                  , successLogger = successLogger
                  , batchSize = batchSize
                  )
    } yield {
      migrated
    }
  }

  private[this] def migrate(
      crs          : Seq[MigrationChangeRequest]
    , errorLogger  : Failure => Unit
  ) : Seq[MigrationChangeRequest] = {
    crs.flatMap { cr =>
      changeRequestMigration.migrate(cr) match {
        case eb:EmptyBox => errorLogger(eb ?~! "Error when trying to migrate change request with id '%s'".format(cr.id)); None
        case Full(m)     => Some(m)
      }
    }
  }

  /**
   * Actually save the crs in DB by batch of batchSize.
   * The final result is a failure if any batch were in failure.
   */
  private[this] def saveResults(
      crs                : Seq[MigrationChangeRequest]
    , saveChangeRequests : Seq[MigrationChangeRequest] => Box[Seq[MigrationChangeRequest]]
    , successLogger      : Seq[MigrationChangeRequest] => Unit
    , batchSize          : Int
  ) : Box[Int] = {
    (bestEffort(crs.grouped(batchSize).toSeq) { seq =>
      val res = saveChangeRequests(seq) ?~! "Error when saving logs (ids: %s)".format(seq.map( _.id).sorted.mkString(","))
      res.foreach { seq => successLogger(seq) }
      res
    }).map( _.flatten ).map( _.size ) //flatten else we have Box[Seq[Seq]]]
  }
}

/**
 * Migrate an event log from fileFormat 2 to 3
 * Also take care of categories, etc.
 */
class ChangeRequestMigration_3_4(xmlMigration:XmlMigration_3_4) {
  def logger = MigrationLogger(3)

  def migrate(cr:MigrationChangeRequest) : Box[MigrationChangeRequest] = {
    /*
     * We don't use values from
     * com.normation.rudder.domain.eventlog.*EventType
     * so that if they change in the future, the migration
     * from 2.3 to 2.4 is still OK.
     */
    val MigrationChangeRequest(id,name, content) = cr


    //utility to factor common code
    //notice the addition of <entry> tag in the result
    def create(optElem:Box[Elem], name:String) = {
       optElem.map { xml => MigrationChangeRequest(id, name, <entry>{xml}</entry>) }
    }

    for {
      xml      <- TestIsEntry(content)
      migrated <- create(xmlMigration.changeRequest(xml), name)
    } yield {
      migrated
    }
  }
}