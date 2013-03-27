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


  val techRepo = RudderConfig.techniqueRepository
  val rodirective = RudderConfig.roDirectiveRepository
//  val adirectiveId = DirectiveId("11c2fdab-053e-4100-a663-b6a29e8b049c")
//  val directive = rodirective.getDirective(adirectiveId).get
//  val tech = rodirective.getActiveTechnique(adirectiveId).get.techniqueName
//  val aDirectiveAddDiff = AddDirectiveDiff(tech,directive)
//  val aDirectiveChangeItem =  DirectiveChangeItem(CurrentUser.getActor,DateTime.now.minusHours(6),None,aDirectiveAddDiff)
//  val aDirectiveChange = DirectiveChange(Some(directive),aDirectiveChangeItem,Seq())
//  val aDirectiveChanges = DirectiveChanges(aDirectiveChange,Seq())
//
//  val dummyCR = ConfigurationChangeRequest(
//      ChangeRequestId("1")
//    , ChangeRequestInfo(
//          "MyFirstChangeRequest"
//        , "blablabla"
//        , false
//      )
//    , Map( adirectiveId -> aDirectiveChanges )
//    , Map()
//  )
//  val dummyCR2 = ConfigurationChangeRequest(
//      ChangeRequestId("2")
//    , ChangeRequestInfo(
//          "MyFirstChangeRequest"
//        , "blablabla"
//        , false
//      )
//    , Map( adirectiveId -> aDirectiveChanges )
//    , Map()
//  )
//
  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestEventLogService = RudderConfig.changeRequestEventLogService
  private[this] val woChangeRequestRepository = RudderConfig.woChangeRequestRepository
  private[this] val roChangeRequestRepository = RudderConfig.roChangeRequestRepository
  private[this] val workFlowEventLogService =  RudderConfig.workflowEventLogService
  private[this] val workflowService = RudderConfig.workflowService
  private[this] val changeRequestTableId = "ChangeRequestId"
  private[this] val CrId: Box[String] = {S.param("crId") }
  private[this] var changeRequest: Box[ChangeRequest] = CrId match {
//    case Full("1") => Full(dummyCR)
//    case Full("2") => Full(dummyCR2)
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

  def dispatch = {
    case "header" => (xml => changeRequest match {
        case eb:EmptyBox =>
          <div id="content">
            <div> Error</div>
          </div>
          /* detail page */

        case Full(id) => displayHeader(id)
      }
    )

    case "details" => (xml => changeRequest match {
        case eb:EmptyBox => <div> Error {eb}</div>
        case Full(cr) =>
          new ChangeRequestEditForm(
              cr.info
            , workflowService
            , cr.id
            , (statusUpdate:ChangeRequestInfo) =>  {
                val newCR = ChangeRequest.updateInfo(
                    cr
                  , statusUpdate
                )
                changeRequest = Full(newCR)
                logger.warn(changeRequest)

                woChangeRequestRepository.updateChangeRequest(newCR, CurrentUser.getActor, None)

                SetHtml("changeRequestHeader", displayHeader(newCR)) &
                SetHtml("changeRequestChanges", new ChangeRequestChangesForm(newCR).dispatch("changes")(NodeSeq.Empty))
              }
          ).display
    })

    case "changes" => (xml => changeRequest match {
        case eb:EmptyBox => <div> Error</div>
        case Full(id) =>
          <div id="changeRequestChanges">{
            new ChangeRequestChangesForm(id).dispatch("changes")(NodeSeq.Empty)
          }</div>
    })

    case "actions" => (xml => changeRequest match {
      case eb:EmptyBox => NodeSeq.Empty
      case Full(cr) => ("#backStep" #> SHtml.ajaxButton("Refuse", () => ChangeStepPopup("Refuse",workflowService.backSteps(workflowService.findStep(cr.id)),cr)) &
         "#nextStep" #> SHtml.ajaxButton("Valid", () => ChangeStepPopup("Accept",workflowService.nextSteps(workflowService.findStep(cr.id)),cr)))(xml)

    })
  }

  def displayHeader(cr:ChangeRequest) = {
    //last action:
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

    val (step,stepDate) = workFlowEventLogService.getLastLog(cr.id) match {
      case eb:EmptyBox => ("Error when retrieving the last action",None)
      case Full(None)  => ("Error when retrieving the last action",None) //should not happen here !
      case Full(Some(StepWorkflowProcessEventLog(actor,date,reason,from,to))) =>
        (s"Sent from ${from} to ${to} on ${DateFormaterService.getFormatedDate(date)} by ${actor.name}",Some(date))
    }
     val last = (date,stepDate) match {
       case (None,None) => action
       case (Some(_),None) => action
       case (None,Some(_)) => step
       case (Some(date),Some(stepDate)) => if (date.isAfter(stepDate)) action else step
     }
    ("#backButton *" #> SHtml.ajaxButton("← Back",() => S.redirectTo("/secure/utilities/changeRequests")) &
       "#CRName *" #> s"CR#${cr.id}: ${cr.info.name}" &
       "#CRStatus *" #> workflowService.findStep(cr.id).value &
       "#CRLastAction *" #> s"${ last }") (header)

  }

  def ChangeStepPopup(action:String,nextSteps:Seq[(WorkflowNodeId,(ChangeRequestId,EventActor, Option[String]) => Box[ChangeRequestId])],cr:ChangeRequest) = {
    type functionType = (ChangeRequestId,EventActor, Option[String]) => Box[ChangeRequestId]
    def closePopup : JsCmd = SetHtml("changeRequestHeader", displayHeader(cr)) &
                SetHtml("CRStatusDetails",Text(workflowService.findStep(cr.id).value )) &
                SetHtml("changeRequestChanges", new ChangeRequestChangesForm(cr).dispatch("changes")(NodeSeq.Empty)) &JsRaw("""          correctButtons();
                    $.modal.close();
           """)
      var nextChosen = nextSteps.head._2
      val nextSelect = SHtml.selectObj(nextSteps.map(v => (v._2,v._1.value)), Full(nextChosen)
          , {t:functionType => nextChosen = t})
      def nextOne(next:String) : NodeSeq= <div class="wbBaseField">
          <b class="threeCol">Next: </b>
          <span id="CRStatus">
            {next}
           </span>
        </div>

      val description = new WBTextAreaField("Message", "") {
      override def setFilter = notNull _ :: trim _ :: Nil
      override def inputField = super.inputField  % ("style" -> "height:5em;")
      override def errorClassName = ""
      override def validations() = valMinLen(5, "The reason must have at least 5 characters.") _ :: Nil
    }
          def confirm() : JsCmd = { val crId = nextChosen(cr.id,CurrentUser.getActor,Some(description.is))
      logger.info(crId)
              closePopup
    }
      SetHtml("popupContent",(
          "#intro *+" #>  s"You choose to ${action}  change request #${cr.id}, please enter a Confirmation message." &
          "#header" #>  s"${action} CR#${cr.id}: ${cr.info.name}" &
          "#form *+" #>
            SHtml.ajaxForm(
              ("#reason" #> description.toForm_! &
               "#next" #> {nextSteps match {
                 case Nil => <span>Error</span>
                 case (head,_) :: Nil => nextOne(head.value)
                 case _ => nextSelect
               }} &
               "#cancel" #> SHtml.ajaxButton("Cancel", () => closePopup ) &
               "#confirm" #> SHtml.ajaxSubmit("Confirm", () => confirm())
          )(popupContent)))(popup++ Script((JsRaw("correctButtons();"))))) &
          JsRaw("createPopup('changeStatePopup', 150, 850)")

  }
}
