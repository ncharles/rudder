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

package com.normation.rudder.repository.inmemory

import scala.collection.mutable.{ Map => MutMap }
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.workflows.{ ChangeRequest, ChangeRequestId }
import com.normation.rudder.repository.{ RoChangeRequestRepository, RoDraftChangeRequestRepository, WoChangeRequestRepository, WoDraftChangeRequestRepository }
import net.liftweb.common._
import com.normation.rudder.services.eventlog.ChangeRequestEventLogService
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.workflows.ChangeRequestEventLog
import com.normation.rudder.domain.workflows.ChangeRequestEventLog
import org.joda.time.DateTime
import com.normation.rudder.domain.workflows.AddChangeRequestDiff
import com.normation.rudder.domain.workflows.DeleteChangeRequestDiff
import com.normation.rudder.domain.workflows.ModifyToChangeRequestDiff

class InMemoryDraftChangeRequestRepository extends RoDraftChangeRequestRepository with WoDraftChangeRequestRepository {

  private[this] val repo = MutMap[ChangeRequestId, (ChangeRequest, EventActor, Option[String])]()


  def get(changeRequestId:ChangeRequestId) : Box[Option[(ChangeRequest, EventActor, Option[String])]] = {
    Full(repo.get(changeRequestId))
  }

  def getAll() : Box[Seq[(ChangeRequest, EventActor, Option[String])]] = Full(repo.values.toSeq)

  def getAll(actor:EventActor) : Box[Seq[(ChangeRequest, EventActor, Option[String])]] = {
    Full(repo.values.toSeq.filter { case (_,a, _) => actor == a } )
  }

  def deleteDraftChangeRequest(id: ChangeRequestId, actor: EventActor): Box[ChangeRequestId] = {
    repo.get(id) match {
      case None => Full(id)
      case Some((cr,a,reason)) =>
        if(actor != a) Failure("You can only delete you change request drafts")
        else {
          repo -= id
          Full(id)
        }

    }
  }

  def saveDraftChangeRequest(cr: ChangeRequest,actor: EventActor,reason: Option[String]): Box[ChangeRequest] = {
    repo += (cr.id -> (cr,actor,reason))
    Full(cr)
  }
}

class InMemoryChangeRequestRepository(log:ChangeRequestEventLogService) extends RoChangeRequestRepository with WoChangeRequestRepository with Loggable {

  private[this] val repo = MutMap[ChangeRequestId, ChangeRequest]()

  /** As seen from class InMemoryChangeRequestRepository, the missing signatures are as follows.
   *  For convenience, these are usable as stub implementations.
   */
  // Members declared in com.normation.rudder.repository.RoChangeRequestRepository
  def get(changeRequestId: ChangeRequestId): Box[Option[ChangeRequest]] = {
    Full(repo.get(changeRequestId))
  }
  def getAll(): Box[Seq[ChangeRequest]] = {
    Full(repo.values.toSeq)
  }

  def createChangeRequest(changeRequest: ChangeRequest,actor: EventActor,reason: Option[String]): Box[ChangeRequest] = {
      repo.get(changeRequest.id) match {
        case Some(x) => Failure(s"Change request with ID ${changeRequest.id} is already created")
        case None =>
          repo += (changeRequest.id-> changeRequest)
          log.saveChangeRequestLog(
              changeRequest.id
            , ChangeRequestEventLog(actor, DateTime.now, reason, AddChangeRequestDiff(changeRequest))
          ).map( _ => changeRequest)
      }

  }

  def deleteChangeRequest(changeRequest: ChangeRequest,actor: EventActor,reason: Option[String]): Box[ChangeRequest] = {
      repo.get(changeRequest.id) match {
        case None => Full(changeRequest)
        case Some(_) =>
           repo -= changeRequest.id
          log.saveChangeRequestLog(
              changeRequest.id
            , ChangeRequestEventLog(actor, DateTime.now, reason, DeleteChangeRequestDiff(changeRequest))
          ).map( _ => changeRequest)
      }
  }

  def updateChangeRequest(changeRequest: ChangeRequest,actor: EventActor,reason: Option[String]): Box[ChangeRequest] = {
      repo.get(changeRequest.id) match {
        case None => Failure(s"Change request with ID ${changeRequest.id} does not exists")
        case Some(_) =>
          repo += (changeRequest.id-> changeRequest)
          log.saveChangeRequestLog(
              changeRequest.id
            , ChangeRequestEventLog(actor, DateTime.now, reason, AddChangeRequestDiff(changeRequest))
          ).map( _ => changeRequest)
      }

  }

}