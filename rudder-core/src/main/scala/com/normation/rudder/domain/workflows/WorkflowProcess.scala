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

package com.normation.rudder.domain.workflows

import scala.collection.mutable.Buffer
import net.liftweb.common._

case class WorkflowProcessId(value:String)



object WorkflowProcess{

  val steps:Map[WorkflowProcessId,WorkflowProcess] =
    Map(Draft.id     -> Draft
      , Validation.id -> Validation
          , Deployment.id -> Deployment
              , Deployed.id -> Validation
                  , Rejected.id -> Rejected)

}
/**
 * TODO: the datastructure that allows to share
 * information between Rudder and the Workflow engine
 */
trait WorkflowProcess {
import WorkflowProcess._
  def id  : WorkflowProcessId
  def requests: Buffer[ChangeRequestId]
  def nextSteps: List[WorkflowProcessId]
  def backSteps: List[WorkflowProcessId]
  def next(request:ChangeRequestId,next:WorkflowProcessId) = {
    if(nextSteps.contains(next)){
      steps(next).requests += request
      requests -= request
      Full(request)
    } else
      Failure("not a good workflow")
  }

}

case object Draft extends WorkflowProcess {
  val id = WorkflowProcessId("Draft")
  val requests = Buffer[ChangeRequestId]()
  val nextSteps = List(Validation.id)
  val backSteps = List(Rejected.id)
}


case object Validation extends WorkflowProcess {
  val id = WorkflowProcessId("Validation")
  val requests = Buffer[ChangeRequestId]()
  val nextSteps = List(Deployment.id,Deployed.id)
  val backSteps = List(Draft.id,Rejected.id)

}

case object Deployment extends WorkflowProcess {
  val id = WorkflowProcessId("Deployment")
  val requests = Buffer[ChangeRequestId]()
  val nextSteps = List(Deployed.id)
  val backSteps = List(Draft.id,Rejected.id)
}

case object Deployed extends WorkflowProcess {
  val id = WorkflowProcessId("Deployed")
  val requests = Buffer[ChangeRequestId]()
  val nextSteps = List()
  val backSteps = List()
}


case object Rejected extends WorkflowProcess {
  val id = WorkflowProcessId("Rejected")
  val requests = Buffer[ChangeRequestId]()
  val nextSteps = List()
  val backSteps = List()
}