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

package com.normation.rudder.repository

import com.normation.rudder.domain.workflows.ChangeRequest
import net.liftweb.common.Box
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.eventlog.EventActor
import com.normation.eventlog.EventActor

/**
 * Before going wild, change request are in Draft
 * Draft are private, by user
 */
trait RoDraftChangeRequestRepository {

  def getAll() : Box[Seq[(ChangeRequest, EventActor, Option[String])]]

  def get(changeRequestId:ChangeRequestId) : Box[Option[(ChangeRequest, EventActor, Option[String])]]

  def getAll(actor:EventActor) : Box[Seq[(ChangeRequest, EventActor, Option[String])]]

}

trait WoDraftChangeRequestRepository {

  /**
   * A draft take care of the actor and the reason: they will be
   * used when the change request is submitted in the wf engine
   * (for the AddChangeRequestEventLog).
   */
  def saveDraftChangeRequest(cr:ChangeRequest, actor:EventActor, reason: Option[String]): Box[ChangeRequest]

  /**
   * A user can do what he wants with it's own draft, and delete them.
   */
  def deleteDraftChangeRequest(id:ChangeRequestId, actor:EventActor) : Box[ChangeRequestId]
}

/**
 * Read access to change request
 */
trait RoChangeRequestRepository {


  def getAll() : Box[Seq[ChangeRequest]]

  def get(changeRequestId:ChangeRequestId) : Box[Option[ChangeRequest]]
}

/**
 * Write access to change request
 */
trait WoChangeRequestRepository {

  /**
   *
   * Save a new change request in the back-end.
   * The idea is ignored, and a new one will be attributed
   * to the change request.
   */
  def createChangeRequest(changeRequest:ChangeRequest, actor:EventActor, reason: Option[String]) : Box[ChangeRequest]

  /**
   * Update a change request. The change request must not
   * be in read-only mode and must exists.
   * The update can not change the read/write mode (such a change
   * will be ignore), an explicit call to setWriteOnly must be
   * done for that.
   */
  def updateChangeRequest(changeRequest:ChangeRequest, actor:EventActor, reason: Option[String]) : Box[ChangeRequest]

  /**
   * Delete a change request.
   * (whatever the read/write mode is).
   */
  def deleteChangeRequest(changeRequest:ChangeRequest, actor:EventActor, reason: Option[String]) : Box[ChangeRequest]

  /**
   * Unlock the Change Request so that subsequent call to updateChangeRequest
   * can be made (and succeed).
   * If the Change Request is already in Read/Write mode, that operation
   * is a no-op.
   */
  def setReadWrite(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]

  /**
   * Lock the Change Request so that subsequent call to updateChangeRequest
   * can not be made (they will return an error).
   * If the Change Request is already in Read Only mode, that operation
   * is a no-op.
   */
  def setReadOnly(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
}
