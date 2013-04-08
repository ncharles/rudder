/*
*************************************************************************************
* Copyright 2011-2013 Normation SAS
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

package com.normation.rudder.services.eventlog

import scala.collection.mutable.{ Buffer, Map => MutMap }
import com.normation.rudder.domain.workflows.ChangeRequestId
import net.liftweb.common._
import com.normation.rudder.repository.EventLogRepository
import com.normation.rudder.repository.QueryParameter
import com.normation.rudder.domain.eventlog.ChangeRequestEventLog
import com.normation.rudder.domain.eventlog.ChangeRequestDiff
import com.normation.eventlog._


/**
 * Allow to query relevant information about change request
 * status.
 */
trait ChangeRequestEventLogService {

  //we certainly won't keep that one in the end
  def saveChangeRequestLog(modId: ModificationId, principal: EventActor, diff: ChangeRequestDiff, reason:Option[String]) : Box[EventLog]

  def getChangeRequestHistory(id: ChangeRequestId) : Box[Seq[ChangeRequestEventLog]]

  /**
   * Return the last logged action for the given ChangeRequest.
   * If the change request is not find, a Full(None) is returned.
   * Else, Full(Some(action)) in case of success, and a Failure
   * describing what happened in other cases.
   */
  def getLastLog(id:ChangeRequestId) : Box[Option[ChangeRequestEventLog]]
}

class ChangeRequestEventLogServiceImpl(
   eventLogRepository : EventLogRepository
 ) extends ChangeRequestEventLogService {
  import scala.collection.mutable.{Map => MutMap, Buffer}

  private[this] val repo = MutMap[ChangeRequestId, Buffer[ChangeRequestEventLog]]()


  def saveChangeRequestLog(modId: ModificationId, principal: EventActor, diff: ChangeRequestDiff, reason:Option[String]): Box[EventLog] = {
    eventLogRepository.saveChangeRequest(modId, principal, diff, reason)
  }

  def getChangeRequestHistory(id: ChangeRequestId) : Box[Seq[ChangeRequestEventLog]] = {
    val query =   QueryParameter("cast (xpath('//changeRequest/id/text()', content) as text[]) = '{?}'",Some(id.value.toString))
    eventLogRepository.getEventLogByCriteria(Some(query), None, None)
        Full(Seq())
  }

  /* changed to lastOption as the order is reversed */
  def getLastLog(id:ChangeRequestId) : Box[Option[ChangeRequestEventLog]] = {
    val query =   QueryParameter("cast (xpath('//changeRequest/id/text()', content) as text[]) = '{?}'",Some(id.value.toString))
    eventLogRepository.getEventLogByCriteria(Some(query), Some(1), Some("executionDate DESC"))
    Full(None)
  }
}
