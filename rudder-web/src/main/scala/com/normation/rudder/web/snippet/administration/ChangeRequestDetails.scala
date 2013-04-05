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

package com.normation.rudder.web.snippet.administration


import net.liftweb.common._
import bootstrap.liftweb.RudderConfig
import net.liftweb.http._
import net.liftweb.http.js._
import JE._
import JsCmds._
import net.liftweb.util._
import Helpers._
import scala.xml.Text
import scala.xml.NodeSeq
import com.normation.rudder.domain.workflows._
import com.normation.rudder.web.model._
import org.joda.time.DateTime
import com.normation.cfclerk.domain.TechniqueId
import com.normation.rudder.domain.policies._
import com.normation.rudder.web.components._
import com.normation.eventlog.EventActor


object ChangeRequestDetails {
  def header =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "header", xml)
    }) openOr Nil

 def popup =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "popup", xml)
    }) openOr Nil

     def popupContent =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "popupContent", xml)
    }) openOr Nil

}

class ChangeRequestDetails extends DispatchSnippet with Loggable {
  import ChangeRequestDetails._


  private[this] val techRepo = RudderConfig.techniqueRepository
  private[this] val rodirective = RudderConfig.roDirectiveRepository
  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestEventLogService = RudderConfig.changeRequestEventLogService
  private[this] val woChangeRequestRepository = RudderConfig.woChangeRequestRepository
  private[this] val roChangeRequestRepository = RudderConfig.roChangeRequestRepository
  private[this] val workFlowEventLogService =  RudderConfig.workflowEventLogService
  private[this] val workflowService = RudderConfig.workflowService
  private[this] val changeRequestTableId = "ChangeRequestId"
  private[this] val CrId: Box[Int] = {S.param("crId").map(x=>x.toInt) }
  private[this] var changeRequest: Box[ChangeRequest] = CrId match {
    case Full(id) => roChangeRequestRepository.get(ChangeRequestId(id)) match {
      case Full(Some(cr)) => Full(cr)
      case Full(None) => Failure(s"There is no Cr with id :${id}")
      case eb:EmptyBox =>       val fail = eb ?~ "no id selected"
      Failure(s"Error in the cr id asked: ${fail.msg}")
    }
    case eb:EmptyBox =>
      val fail = eb ?~ "no id selected"
      Failure(s"Error in the cr id asked: ${fail.msg}")
  }

  private[this] def step = changeRequest.flatMap(cr => workflowService.findStep(cr.id))

  def dispatch = {
    // Display Change request Header
    case "header" =>
      ( xml =>
        changeRequest match {
          case eb:EmptyBox => NodeSeq.Empty
          case Full(id) => displayHeader(id)
        }
      )

    // Display change request details
    case "details" =>
      ( xml =>

        changeRequest match {
          case eb:EmptyBox => <div> Error {eb}</div>
          case Full(cr) =>
            new ChangeRequestEditForm(
                cr.info
              , workflowService
              , cr.id
              , changeDetailsCallback(cr) _
            ).display
        }
      )

    // Display change request content
    case "changes" =>
      ( xml =>
        changeRequest match {
          case eb:EmptyBox => NodeSeq.Empty
          case Full(id) =>
            val form = new ChangeRequestChangesForm(id).dispatch("changes")(xml)
            <div id="changeRequestChanges">{form}</div>
        }
      )

    // Add action buttons
    case "actions" =>
      ( xml =>
        changeRequest match {
          case eb:EmptyBox => NodeSeq.Empty
          case Full(cr) =>
            step match {
            case eb:EmptyBox =>  NodeSeq.Empty
            case Full(step) =>

              ("#backStep" #> { workflowService.backSteps(step) match {
                  case Nil => NodeSeq.Empty
                  case steps =>
                    SHtml.ajaxButton(
                        "Cancel"
                      , () => ChangeStepPopup("Refuse", steps, cr)
              ) } }  &
               "#nextStep" #> {workflowService.nextSteps(step) match {
                             case Nil => NodeSeq.Empty
                              case steps =>
                               SHtml.ajaxButton(
                                   "Accept"
                                 , () => ChangeStepPopup("Accept",steps,cr)
                               ) } }
             ) (xml)
            }
        }
      )
  }

  private[this] def changeDetailsCallback (cr:ChangeRequest)(statusUpdate:ChangeRequestInfo) =  {
    val newCR = ChangeRequest.updateInfo(cr, statusUpdate)
    changeRequest = Full(newCR)
    woChangeRequestRepository.updateChangeRequest(newCR, CurrentUser.getActor, None)
    SetHtml("changeRequestHeader", displayHeader(newCR)) &
    SetHtml("changeRequestChanges", new ChangeRequestChangesForm(newCR).dispatch("changes")(NodeSeq.Empty))
  }

  def displayHeader(cr:ChangeRequest) = {
    //last action on the change Request (name/description changed):
    val (action,date) = changeRequestEventLogService.getLastLog(cr.id) match {
      case eb:EmptyBox => ("Error when retrieving the last action",None)
      case Full(None)  => ("Error, no action were recorded for that change request",None) //should not happen here !
      case Full(Some(ChangeRequestEventLog(actor,date,reason,diff))) =>
        val actionName = diff match {
          case ModifyToChangeRequestDiff(_) => "Modified"
          case AddChangeRequestDiff(_)    => "Created"
          case DeleteChangeRequestDiff(_) => "Deleted"
        }
        (s"${actionName} on ${DateFormaterService.getFormatedDate(date)} by ${actor.name}",Some(date))
    }

    // Last workflow change on that change Request
    val (step,stepDate) = workFlowEventLogService.getLastLog(cr.id) match {
      case eb:EmptyBox => ("Error when retrieving the last action",None)
      case Full(None)  => ("Error when retrieving the last action",None) //should not happen here !
      case Full(Some(StepWorkflowProcessEventLog(actor,date,reason,from,to))) =>
        (s"Sent from ${from} to ${to} on ${DateFormaterService.getFormatedDate(date)} by ${actor.name}",Some(date))
    }

    // Compare both to find the oldest
    val last = (date,stepDate) match {
      case (None,None) => action
      case (Some(_),None) => action
      case (None,Some(_)) => step
      case (Some(date),Some(stepDate)) => if (date.isAfter(stepDate)) action else step
    }
    val link = <b style="margin-top:10px"> ← Back to change request list</b>
    ( "#backButton *" #> SHtml.a(() => S.redirectTo("/secure/utilities/changeRequests"),link) &
      "#CRName *" #> s"CR #${cr.id}: ${cr.info.name}" &
      "#CRStatus *" #> workflowService.findStep(cr.id).map(x => Text(x.value)).openOr(<div class="error">Cannot find the status of this change request</div>) &
      "#CRLastAction *" #> s"${ last }"
    ) (header)

  }

  def ChangeStepPopup(action:String,nextSteps:Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId])],cr:ChangeRequest) = {
    type stepChangeFunction = (ChangeRequestId,EventActor, Option[String]) => Box[WorkflowNodeId]

    def closePopup : JsCmd =
      SetHtml("changeRequestHeader", displayHeader(cr)) &
      SetHtml("CRStatusDetails",workflowService.findStep(cr.id).map(x => Text(x.value)).openOr(<div class="error">Cannot find the status of this change request</div>) ) &
      SetHtml("changeRequestChanges", new ChangeRequestChangesForm(cr).dispatch("changes")(NodeSeq.Empty)) &
      JsRaw("""correctButtons();
               $.modal.close();""")

    var nextChosen = nextSteps.head._2
    val nextSelect =
      SHtml.selectObj(
          nextSteps.map(v => (v._2,v._1.value)), Full(nextChosen)
        , {t:stepChangeFunction => nextChosen = t}
      )
    def nextOne(next:String) : NodeSeq=

        <span id="CRStatus">
          {next}
        </span>

    val stepMessage =
      new WBTextAreaField("Message", "") {
        override def setFilter = notNull _ :: trim _ :: Nil
        override def inputField = super.inputField  % ("style" -> "height:5em;")
        override def errorClassName = ""
        override def validations() = valMinLen(5, "The message must have at least 5 characters.") _ :: Nil
      }

    def confirm() : JsCmd = {
      nextChosen(cr.id,CurrentUser.getActor,Some(stepMessage.is))
      closePopup
    }

    val next = {
      nextSteps match {
        case Nil => <span id="CRStatus">Error</span>
        case (head,_) :: Nil =>  <span id="CRStatus"> {head.value} </span>
        case _ => nextSelect
      }
    }

    val content = {
      ( "#intro *+" #>  s"You choose to ${action}  change request #${cr.id}, please enter a Confirmation message." &
        "#header"   #>  s"${action} CR #${cr.id}: ${cr.info.name}" &
        "#form *+"  #>
          SHtml.ajaxForm(
            ( "#reason"  #> stepMessage.toForm_! &
              "#next"    #> next &
              "#cancel"  #> SHtml.ajaxButton("Cancel", () => closePopup ) &
              "#confirm" #> SHtml.ajaxSubmit("Confirm", () => confirm())
            ) (popupContent)
          )
      ) ( popup ) ++
      Script(JsRaw("correctButtons();"))
    }


    SetHtml("popupContent",content) &
    JsRaw("createPopup('changeStatePopup', 150, 850)")

  }
}
