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

package com.normation.rudder.repository.jdbc

import net.liftweb.common.Loggable
import com.normation.rudder.repository.RoChangeRequestRepository
import org.joda.time.DateTime
import net.liftweb.common._
import java.sql.ResultSet
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.Timestamp
import org.springframework.jdbc.core.JdbcTemplate
import com.normation.rudder.domain.workflows.ChangeRequest
import org.springframework.jdbc.core.RowMapper
import com.normation.rudder.domain.workflows.ConfigurationChangeRequest
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.ChangeRequestInfo
import scala.util.{Try, Failure => Catch, Success}
import scala.collection.JavaConversions._
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource
import com.normation.rudder.domain.policies.DirectiveId
import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.rudder.domain.policies.RuleId

class RoChangeRequestJdbcRepository(
    jdbcTemplate : JdbcTemplate
) extends RoChangeRequestRepository with Loggable {

  val INSERT_SQL = "insert into ChangeRequest (name, description, creationTime, content) values (?, ?, ?, ?)"
    
  val SELECT_SQL = "SELECT id, name, description, creationTime, content FROM ChangeRequest"
    
  def getAll() : Box[Seq[ChangeRequest]] = {
    Try {
      jdbcTemplate.query(SELECT_SQL, Array[AnyRef](), ChangeRequestsMapper).toSeq
    } match {
      case Success(x) => Full(x)
      case Catch(error) => Failure(error.toString())
    }
  }
  
  def get(changeRequestId:ChangeRequestId) : Box[Option[ChangeRequest]] = {
    Try {
      jdbcTemplate.query(
          SELECT_SQL + " where id = ?"
        , Array[AnyRef](changeRequestId.value.asInstanceOf[AnyRef])
        , ChangeRequestsMapper).toSeq
    } match {
      case Success(x) => x.size match {
        case 0 => Full(None)
        case 1 => Full(x.headOption)
        case _ => Failure(s"Too many change request have the same id ${changeRequestId.value}")
      }
      case Catch(error) => Failure(error.toString())
    }  
  }
  
  def getByIds(changeRequestId:Seq[ChangeRequestId]) : Box[Seq[ChangeRequest]] = {
    val parameters = new MapSqlParameterSource();
    parameters.addValue("ids", changeRequestId.map(x => x.value))

    Try {
      jdbcTemplate.query(
          SELECT_SQL + " where id in (:ids)"
        , ChangeRequestsMapper
        , parameters).toSeq
    } match {
      case Success(x) => Full(x)
      case Catch(error) => Failure(error.toString())
    }  
  }

  def getByDirective(id : DirectiveId) : Box[Seq[ChangeRequest]] = {
    val directiveQuery = " where cast (xpath('/path/to/directive/text()', content) as text[]) = '{?}'"
    Try {
      jdbcTemplate.query(SELECT_SQL + directiveQuery, Array[AnyRef](id.value), ChangeRequestsMapper).toSeq
    } match {
      case Success(x) => Full(x)
      case Catch(error) => Failure(error.toString())
    }
  }
  
  def getByNodeGroup(id : NodeGroupId) : Box[Seq[ChangeRequest]] = ???
  
  def getByRule(id : RuleId) : Box[Seq[ChangeRequest]] = ???
  
  
}

object ChangeRequestsMapper extends RowMapper[ChangeRequest] with Loggable {
  def mapRow(rs : ResultSet, rowNum: Int) : ChangeRequest = {
    // dummy code
    ConfigurationChangeRequest(
        ChangeRequestId(rs.getInt("id"))
      , ChangeRequestInfo(
            rs.getString("name")
          , rs.getString("description")
        )
      , Map()
      , Map()
      , Map()
    )
  }
  
}