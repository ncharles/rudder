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

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import net.liftweb.common._
import com.normation.rudder.repository.jdbc._
import java.sql.Timestamp
import org.squeryl.KeyedEntity
import org.joda.time.DateTime

class UpdatesEntriesJdbcRepository(sessionProvider : SquerylConnectionProvider) extends UpdatesEntriesRepository with  Loggable{
  import AggregationConstants._

  def toTimeStamp(d:DateTime) : Timestamp = new Timestamp(d.getMillis)


  def getNameUpdateTime() : Option[DateTime] = {
    getUpdateTime(NAME_UPDATE_KEY)
  }

  def getAgregationUpdateTime() : Option[DateTime] = {
   getUpdateTime(AGREGATION_UPDATE_KEY)
  }

  private def getUpdateTime(key : String) : Option[DateTime] = {
    sessionProvider.ourTransaction {
      val q = from(Updates.updates)(entry =>
	      where(entry.key === key)
	      select(entry)
	    )
	    val result =
	      q.toList


      result match {
        case Nil => None
        case head :: Nil => Some(new DateTime(head.dateTime))
        case _ =>
          logger.error("Too many entry matching %s in table updates".format(NAME_UPDATE_KEY))
          None
      }
    }


  }


  def setNameUpdateTime(date : DateTime) = {
   setUpdateTime(NAME_UPDATE_KEY, date)
  }

  def setAgregationUpdateTime(date : DateTime) = {
   setUpdateTime(AGREGATION_UPDATE_KEY, date)
  }

  private def setUpdateTime(key : String, date : DateTime) = {
    sessionProvider.ourTransaction {
      	val q = update(Updates.updates)(entry =>
        	where(entry.key === key)
        	set(entry.dateTime := toTimeStamp(date)))

        if (q ==0) // could not update
          Updates.updates.insert(new UpdateEntry(key, toTimeStamp(date)))

    }
  }

}




case class UpdateEntry(
    @Column("key") key: String,
    @Column("value") dateTime: Timestamp
) extends KeyedEntity[String]  {
	def id = key
}

object Updates extends Schema {
  val updates = table[UpdateEntry]("updates")
}