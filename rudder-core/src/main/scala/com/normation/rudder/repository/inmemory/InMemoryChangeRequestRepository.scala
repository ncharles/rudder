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

import com.normation.rudder.repository.RoChangeRequestRepository
import com.normation.rudder.repository.WoChangeRequestRepository
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.ChangeRequest
import net.liftweb.common._
import scala.collection.mutable.{ Map => MutMap }
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.ConfigurationChangeRequest

class InMemoryChangeRequestRepository extends RoChangeRequestRepository with WoChangeRequestRepository {

  private[this] val repo = MutMap[ChangeRequestId, ChangeRequest]()

  def get(changeRequestId: ChangeRequestId): Box[ChangeRequest] = Box(repo.get(changeRequestId))
  def getAll(): Box[Seq[ChangeRequest]] = Full(repo.values.toSeq)
  def createChangeRequest(changeRequest: ChangeRequest): Box[ChangeRequest] = repo.get(changeRequest.id) match {
    case Some(_) => Failure(s"Error, the change request with ID '${changeRequest.id.value}' already exists, it can not be created")
    case None =>
      repo += (changeRequest.id -> changeRequest)
      Full(changeRequest)
  }
  def updateChangeRequest(changeRequest: ChangeRequest): Box[ChangeRequest] = repo.get(changeRequest.id) match {
    case Some(x) =>
      if(x.status.readOnly) {
        Failure(s"Error, the change request with ID '${changeRequest.id.value}' can not be updated because it is read-only")
      } else {
        repo += (changeRequest.id -> changeRequest)
        Full(changeRequest)
      }

    case None => Failure(s"Error, the change request with ID '${changeRequest.id.value}' does not exist, it can not be updated")
  }
  def setReadOnly(changeRequestId: ChangeRequestId): Box[ChangeRequestId] = repo.get(changeRequestId) match {
    case None => Failure(s"No change request with ID '${changeRequestId.value}' exists, can not set it read-only")
    case Some(cr) => ???
  }
  def deleteChangeRequest(changeRequest: ChangeRequest): Box[ChangeRequest] = {
    repo -= changeRequest.id
    Full(changeRequest)
  }

  def setReadWrite(changeRequestId: ChangeRequestId): Box[ChangeRequestId] = ???
}