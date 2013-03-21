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

package com.normation.rudder.services.workflows

import com.normation.rudder.domain.workflows.{ ChangeRequestId }
import net.liftweb.common.Box
import com.normation.rudder.domain.workflows.ChangeRequestId
import scala.collection.mutable.Buffer
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.ChangeRequestId
import net.liftweb.common.Failure
import net.liftweb.common.Loggable
import net.liftweb.common.Full
import com.normation.rudder.domain.workflows.WorkflowProcessEventLog
import com.normation.rudder.domain.workflows.StepWorkflowProcessEventLog
import com.normation.eventlog.EventActor
import org.joda.time.DateTime
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.WorkflowNodeId
import com.normation.rudder.domain.workflows.WorkflowNode
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.domain.workflows.WorkflowNode

/**
 * That service allows to glue Rudder with the
 * workflows engine.
 * It allows to send new ChangeRequest to the engine,
 * and to be notified when one of them reach the end.
 */
trait WorkflowService {

  /**
   * Start a new workflow process with the given
   * change request or continue an existing
   * wf for that change request
   * (one change request can not have more than
   * one wf at the same time).
   *
   * So for now, a workflow process id IS a changeRequestId.
   * That abstraction is likelly to leak.
   *
   */
  def startWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]

  /**
   * Notify who wants to know about the successful ending of the workflow
   * process for the given change request
   */
  def onSuccessWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]

  def onFailureWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]

  //allowed workflow steps

  def stepStartToValidation(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepValidationToDeployment(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepValidationToCorrection(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepValidationToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepValidationToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepDeploymentToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepDeploymentToCorrection(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepDeploymentToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepCorrectionToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]
  def stepCorrectionToValidation(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId]

  def getValidation : Box[Seq[ChangeRequestId]]
  def getDeployment : Box[Seq[ChangeRequestId]]
  def getCorrection : Box[Seq[ChangeRequestId]]
  def getDeployed : Box[Seq[ChangeRequestId]]
  def getRejected : Box[Seq[ChangeRequestId]]


  val stepsValue :List[String]

  def findStep(changeRequestId: ChangeRequestId) :String
}


class WorkflowProcessLog extends Loggable {

  def saveEventLog(log:StepWorkflowProcessEventLog) : Box[WorkflowProcessEventLog] = {
    logger.info(s"[${log.date.toString()}] Actor ${log.actor}Workflow step from ${log.from.id.value} to ${log.to.id.value}")
    Full(log)
  }

}

class WorkflowServiceImpl(log:WorkflowProcessLog) extends WorkflowService with Loggable {
  private[this] sealed trait MyWorkflowNode extends WorkflowNode

  //buffers for Workflow process
  private[this] case object Start extends MyWorkflowNode {
    val id = WorkflowNodeId("Start")
    val requests = Buffer[ChangeRequestId]()
  }

  private[this] case object Correction extends MyWorkflowNode {
    val id = WorkflowNodeId("Correction")
    val requests = Buffer[ChangeRequestId]()
  }

  private[this] case object Validation extends MyWorkflowNode {
    val id = WorkflowNodeId("Validation")
    val requests = Buffer[ChangeRequestId]()
  }

  private[this] case object Deployment extends MyWorkflowNode {
    val id = WorkflowNodeId("Deployment")
    val requests = Buffer[ChangeRequestId]()
  }

  private[this] case object Deployed extends MyWorkflowNode {
    val id = WorkflowNodeId("Deployed")
    val requests = Buffer[ChangeRequestId]()
  }

  private[this] case object Rejected extends MyWorkflowNode {
    val id = WorkflowNodeId("Rejected")
    val requests = Buffer[ChangeRequestId]()
  }

  private[this] val steps:List[WorkflowNode] = List(Start,Correction,Validation,Deployment,Deployed,Rejected)

  val stepsValue = steps.map(_.id.value)

  def findStep(changeRequestId: ChangeRequestId) = {
    steps.find(_.requests.contains(changeRequestId)).map(_.id.value).getOrElse("Draft")
  }
  private[this] def changeStep(
      from           : MyWorkflowNode
    , to             : MyWorkflowNode
    , changeRequestId: ChangeRequestId
    , actor          : EventActor
    , reason         : Option[String]
  ) : Box[ChangeRequestId] = {
    //check the presence of the changeRequest in the first state, and migrate
    //it, logging
    if(from.requests.contains(changeRequestId)) {
      from.requests -= changeRequestId
      to.requests += changeRequestId
      log.saveEventLog(StepWorkflowProcessEventLog(actor, DateTime.now, reason, from, to))
      Full(changeRequestId)
    } else {
      Failure(s"The workflow step ${from.id.value} does not contain the change request with ID ${changeRequestId.value}")
    }
  }


  def startWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info("start")
    Start.requests += changeRequestId
    stepStartToValidation(changeRequestId, actor, reason)
  }

  def onSuccessWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info("update")
    ???
  }

  def onFailureWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info("update")
    ???
  }

  //allowed workflow steps

  def stepStartToValidation(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Start, Validation, changeRequestId, actor, reason)
  }

  def stepValidationToDeployment(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Validation, Deployment, changeRequestId, actor, reason)
  }

  def stepValidationToCorrection(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Validation, Correction, changeRequestId, actor, reason)
  }

  def stepValidationToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Validation, Deployed, changeRequestId, actor, reason)
  }

  def stepValidationToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Validation, Rejected, changeRequestId, actor, reason)
  }

  def stepDeploymentToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Deployment, Deployed, changeRequestId, actor, reason)
  }

  def stepDeploymentToCorrection(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Deployment, Correction, changeRequestId, actor, reason)
  }

  def stepDeploymentToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Deployment, Rejected, changeRequestId, actor, reason)
  }

  def stepCorrectionToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Correction, Rejected, changeRequestId, actor, reason)
  }

  def stepCorrectionToValidation(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Start, Validation, changeRequestId, actor, reason)
  }

  def getValidation : Box[Seq[ChangeRequestId]] = Full(Validation.requests)
  def getDeployment : Box[Seq[ChangeRequestId]] = Full(Deployment.requests)
  def getCorrection : Box[Seq[ChangeRequestId]] = Full(Correction.requests)
  def getDeployed : Box[Seq[ChangeRequestId]] = Full(Deployed.requests)
  def getRejected : Box[Seq[ChangeRequestId]] = Full(Rejected.requests)
}