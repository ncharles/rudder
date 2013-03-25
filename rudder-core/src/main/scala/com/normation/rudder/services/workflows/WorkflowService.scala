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

import scala.collection.mutable.Buffer
import scala.collection.mutable.{ Map => MutMap }

import org.joda.time.DateTime

import com.normation.cfclerk.domain.TechniqueName
import com.normation.eventlog.EventActor
import com.normation.eventlog.ModificationId
import com.normation.rudder.batch.AsyncDeploymentAgent
import com.normation.rudder.batch.AutomaticStartDeployment
import com.normation.rudder.domain.eventlog.RudderEventActor
import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.workflows._
import com.normation.rudder.repository.RoChangeRequestRepository
import com.normation.rudder.repository.RoDirectiveRepository
import com.normation.rudder.repository.WoDirectiveRepository
import com.normation.rudder.services.policies.DependencyAndDeletionService
import com.normation.utils.Control._
import com.normation.utils.StringUuidGenerator

import net.liftweb.common._

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


  val stepsValue :List[WorkflowNodeId]

  val nextSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[ChangeRequestId])]]
  val backSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[ChangeRequestId])]]
  def findStep(changeRequestId: ChangeRequestId) : WorkflowNodeId
}




/**
 * A service that will do whatever is needed to commit
 * modification about a change request in LDAP and
 * deploy them
 */
class CommitAndDeployChangeRequest(
    uuidGen             : StringUuidGenerator
  , roChangeRequestRepo : RoChangeRequestRepository
  , roDirectiveRepo     : RoDirectiveRepository
  , woDirectiveRepo     : WoDirectiveRepository
  , asyncDeploymentAgent: AsyncDeploymentAgent
  , dependencyService   : DependencyAndDeletionService
) extends Loggable {

  def save(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info(s"Saving and deploying change request ${changeRequestId}")
    for {
      changeRequest <- roChangeRequestRepo.get(changeRequestId)
      saved         <- changeRequest match {
                         case Some(config:ConfigurationChangeRequest) => saveConfigurationChangeRequest(config)
                         case x => Failure("We don't know how to deploy change request like this one: " + x)
                       }
    } yield {
      saved
    }
  }

  /*
   * So, what to do ?
   */
  private[this] def saveConfigurationChangeRequest(cr:ConfigurationChangeRequest) : Box[ChangeRequestId] = {
    import com.normation.utils.Control.sequence

    def doDirectiveChange(directiveChanges:DirectiveChanges, modId: ModificationId) : Box[DirectiveId] = {
      def save(tn:TechniqueName, d:Directive, change: DirectiveChangeItem) = {
        for {
          activeTechnique <- roDirectiveRepo.getActiveTechnique(tn)
          saved           <- woDirectiveRepo.saveDirective(activeTechnique.id, d, modId, change.actor, change.reason)
        } yield {
          saved
        }
      }

      for {
        change <- directiveChanges.changes.change
        done   <- change.diff match {
                    case DeleteDirectiveDiff(tn,d,rs) =>
                      dependencyService.cascadeDeleteDirective(d.id, modId, change.actor, change.reason).map( _ => d.id)
                    case ModifyToDirectiveDiff(tn,d,rs) => save(tn,d, change).map( _ => d.id )
                    case AddDirectiveDiff(tn,d,rs) => save(tn,d, change).map( _ => d.id )
                  }
      } yield {
        done
      }
    }

    def doNodeGroupChange(change:NodeGroupChanges, modId: ModificationId) : Box[NodeGroupId] = {
      val id = change.changes.initialState.map( _.id.value).getOrElse("new group")
      logger.info(s"Save group with id '${id}'")
      Full(NodeGroupId(id))
    }

    val modId = ModificationId(uuidGen.newUuid)

    /*
     * TODO: we should NOT commit into git each modification,
     * but wait until the last line and then commit.
     */

    for {
      directives <- sequence(cr.directives.values.toSeq) { directiveChange =>
                      doDirectiveChange(directiveChange, modId)
                    }
      groups     <- sequence(cr.nodeGroups.values.toSeq) { nodeGroupChange =>
                      doNodeGroupChange(nodeGroupChange, modId)
                    }
    } yield {
      asyncDeploymentAgent ! AutomaticStartDeployment(modId, RudderEventActor)
      cr.id
    }
  }

}

class WorkflowServiceImpl(
    log   : WorkflowProcessEventLogService
  , commit: CommitAndDeployChangeRequest
) extends WorkflowService with Loggable {
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

  val stepsValue = steps.map(_.id)

  val nextSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[ChangeRequestId])]] =
    steps.map{
    case Start      => Start.id -> Seq((Validation.id,stepStartToValidation _))
    case Validation => Validation.id -> Seq((Deployment.id,stepValidationToDeployment _),(Deployed.id,stepValidationToDeployed _))
    case Correction => Correction.id -> Seq((Validation.id,stepCorrectionToValidation _))
    case Deployment => Deployment.id -> Seq((Deployed.id,stepDeploymentToDeployed _))
    case Deployed   => Deployed.id -> Seq()
    case Rejected   => Rejected.id -> Seq()
   }.toMap

  val backSteps: Map[WorkflowNodeId,Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[ChangeRequestId])]] =
    steps.map{
    case Start      => Start.id -> Seq()
    case Validation => Validation.id -> Seq((Correction.id,stepValidationToCorrection _),(Rejected.id,stepValidationToRejected _))
    case Correction => Correction.id -> Seq((Rejected.id,stepCorrectionToRejected _))
    case Deployment => Deployment.id -> Seq((Correction.id,stepDeploymentToCorrection _),(Rejected.id,stepDeploymentToRejected _))
    case Deployed   => Deployed.id -> Seq()
    case Rejected   => Rejected.id -> Seq()
   }.toMap

  def findStep(changeRequestId: ChangeRequestId) = { logger.warn(s"lookinf for cr $changeRequestId")
    steps.find(_.requests.contains(changeRequestId)).map(_.id).getOrElse(Rejected.id)
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
      log.saveEventLog(StepWorkflowProcessEventLog(actor, DateTime.now, reason, from, to),changeRequestId)
      Full(changeRequestId)
    } else {
      Failure(s"The workflow step ${from.id.value} does not contain the change request with ID ${changeRequestId.value}")
    }
  }

  private[this] def toSuccess(from: MyWorkflowNode, changeRequestId: ChangeRequestId, actor: EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    for {
      success <- changeStep(from, Deployed, changeRequestId, actor, reason)
      saved   <- onSuccessWorkflow(changeRequestId, actor, reason)
    } yield {
      saved
    }
  }

  private[this] def toFailure(from: MyWorkflowNode, changeRequestId: ChangeRequestId, actor: EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    for {
      failure <- changeStep(from, Rejected, changeRequestId, actor, reason)
      failed  <- onFailureWorkflow(changeRequestId, actor, reason)
    } yield {
      failed
    }
  }

  def startWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info("start")
    Start.requests += changeRequestId
    stepStartToValidation(changeRequestId, actor, reason)
  }

  def onSuccessWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    logger.info("update")
    commit.save(changeRequestId, actor, reason)
  }

  def onFailureWorkflow(changeRequestId: ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    //Only log the fact that something happened
    logger.info(s"Change request with ID ${changeRequestId.value} was rejected")
    Full(changeRequestId)
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
    toSuccess(Validation, changeRequestId, actor, reason)
  }

  def stepValidationToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    toFailure(Validation, changeRequestId, actor, reason)
  }

  def stepDeploymentToDeployed(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    toSuccess(Deployment, changeRequestId, actor, reason)
  }

  def stepDeploymentToCorrection(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Deployment, Correction, changeRequestId, actor, reason)
  }

  def stepDeploymentToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    toFailure(Deployment, changeRequestId, actor, reason)
  }

  def stepCorrectionToRejected(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    toFailure(Correction, changeRequestId, actor, reason)
  }

  def stepCorrectionToValidation(changeRequestId:ChangeRequestId, actor:EventActor, reason: Option[String]) : Box[ChangeRequestId] = {
    changeStep(Correction, Validation, changeRequestId, actor, reason)
  }

  def getValidation : Box[Seq[ChangeRequestId]] = Full(Validation.requests)
  def getDeployment : Box[Seq[ChangeRequestId]] = Full(Deployment.requests)
  def getCorrection : Box[Seq[ChangeRequestId]] = Full(Correction.requests)
  def getDeployed : Box[Seq[ChangeRequestId]] = Full(Deployed.requests)
  def getRejected : Box[Seq[ChangeRequestId]] = Full(Rejected.requests)
}


trait WorkflowProcessEventLogService {

  def saveEventLog(log:StepWorkflowProcessEventLog, crId:ChangeRequestId) : Box[WorkflowProcessEventLog]

  def getChangeRequestHistory(id: ChangeRequestId) : Box[Seq[WorkflowProcessEventLog]]

  def getLastLog(id:ChangeRequestId) : Box[Option[WorkflowProcessEventLog]]
}

class InMemoryWorkflowProcessEventLogService extends WorkflowProcessEventLogService with Loggable {
  import scala.collection.mutable.{Map => MutMap, Buffer}
  private[this] val repo = MutMap[ChangeRequestId, Buffer[WorkflowProcessEventLog]]()
  def saveEventLog(log:StepWorkflowProcessEventLog, crId:ChangeRequestId) : Box[WorkflowProcessEventLog] = {
    val buf = repo.getOrElse(crId, Buffer[WorkflowProcessEventLog]())
    buf += log
    repo += (crId -> buf)

    Full(log)
  }

  def getChangeRequestHistory(id: ChangeRequestId) : Box[Seq[WorkflowProcessEventLog]] = {
    Full(repo.getOrElse(id, Seq()))
  }

  def getLastLog(id:ChangeRequestId) : Box[Option[WorkflowProcessEventLog]] = {
        Full(repo.getOrElse(id, Seq()).lastOption)
  }
}