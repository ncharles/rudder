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
}
class ChangeRequestDetails extends DispatchSnippet with Loggable {
import ChangeRequestDetails._


  val techRepo = RudderConfig.techniqueRepository
  val rodirective = RudderConfig.roDirectiveRepository
  val adirectiveId = DirectiveId("11c2fdab-053e-4100-a663-b6a29e8b049c")
  val directive = rodirective.getDirective(adirectiveId).get
  val tech = rodirective.getActiveTechnique(adirectiveId).get.techniqueName
  val aDirectiveAddDiff = AddDirectiveDiff(tech,directive)
  val aDirectiveChangeItem =  DirectiveChangeItem(CurrentUser.getActor,DateTime.now.minusHours(6),None,aDirectiveAddDiff)
  val aDirectiveChange = DirectiveChange(Some(directive),aDirectiveChangeItem,Seq())
  val aDirectiveChanges = DirectiveChanges(aDirectiveChange,Seq())
//  val dummyStatus = ChangeRequestInfo("MyFirstChangeRequest","blablabla",false)
//  val dummyStatus2 = ChangeRequestInfo("MySecondChangeRequest","blablabla",false)
//  val startStatus = AddChangeRequestStatusDiff(dummyStatus)
//  val startStatus2 = AddChangeRequestStatusDiff(dummyStatus2)
//  val startStatusItem = ChangeRequestStatusItem(
//                            CurrentUser.getActor
//                          , DateTime.now.minusDays(1)
//                          , None
//                          , AddChangeRequestStatusDiff(dummyStatus)
//                        )
//  val startStatusItem2 = ChangeRequestStatusItem(
//                             CurrentUser.getActor
//                           , DateTime.now.minusDays(1)
//                           , None
//                           , AddChangeRequestStatusDiff(dummyStatus2)
//                         )

  val dummyCR = ConfigurationChangeRequest(
      ChangeRequestId("1")
    , ChangeRequestInfo(
          "MyFirstChangeRequest"
        , "blablabla"
        , false
      )
    , Map( adirectiveId -> aDirectiveChanges )
    , Map()
  )
  val dummyCR2 = ConfigurationChangeRequest(
      ChangeRequestId("2")
    , ChangeRequestInfo(
          "MyFirstChangeRequest"
        , "blablabla"
        , false
      )
    , Map( adirectiveId -> aDirectiveChanges )
    , Map()
  )

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestEventLogService = RudderConfig.changeRequestEventLogService
  private[this] val woChangeRequestRepository = RudderConfig.woChangeRequestRepository

  private[this] val changeRequestTableId = "ChangeRequestId"
  private[this] val CrId: Box[String] = {S.param("crId") }
  private[this] var changeRequest: Box[ChangeRequest] = CrId match {case Full("1") => Full(dummyCR)
    case Full("2") => Full(dummyCR2)
    case Full(id) => Failure(s"${id} is not a good Change request id")
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
            , cr.id
            , (statusUpdate:ChangeRequestInfo) =>  {
                val newCR = ChangeRequest.updateInfo(
                    cr
                  , statusUpdate
                )
                changeRequest = Full(newCR)
                logger.warn(changeRequest)
                //todo: save update change request, that will create the event log,
                //and so populate the eventlog history
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
      case Full(cr) => ("#backStep" #> SHtml.ajaxButton("Refuse", () => ChangeStepPopup("Refuse",List("Draft"),cr)) &
         "#nextStep" #> SHtml.ajaxButton("Valid", () => ChangeStepPopup("Accept",List("Deploy Later","Deploy Directly"),cr)))(xml)

    })
  }

  def displayHeader(cr:ChangeRequest) = {

    //last action:
    val action = changeRequestEventLogService.getLastLog(cr.id) match {
      case eb:EmptyBox => "Error when retrieving the last action"
      case Full(None)  => "Error, no action were recorded for that change request" //should not happen here !
      case Full(Some(ChangeRequestEventLog(actor,date,reason,diff))) =>
        val actionName = diff match {
          case ModifyToChangeRequestDiff(_) => "Modified"
          case AddChangeRequestDiff(_)    => "Created"
          case DeleteChangeRequestDiff(_) => "Deleted"
          case RebaseChangeRequestDiff(_) => "Resynchronise with current values"
        }
        s"${actionName} on ${DateFormaterService.getFormatedDate(date)} by ${actor.name}"
    }

    ("#backButton *" #> SHtml.ajaxButton("← Back",() => S.redirectTo("/secure/utilities/changeRequests")) &
       "#CRName *" #> s"CR#${cr.id}: ${cr.info.name}" &
       "#CRStatus *" #> "Status" &
       "#CRLastAction *" #> s"${ action }") (header)

  }

  def ChangeStepPopup(action:String,nextStep:List[String],cr:ChangeRequest) = {
      val closePopup : JsCmd = JsRaw(""" $.modal.close();""")
      val nextSelect = new WBSelectField("Next", nextStep.map(v => (v,v)))
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
      SetHtml("popupContent",("#header" #>  s"${action} CR#${cr.id}: ${cr.info.name}" &
          "#intro *+" #>  s"You choose to ${action}  change request #${cr.id}, please enter a Confirmation message." &
          "#reason" #> description.toForm_! &
          "#next" #> {nextStep match {
            case Nil => <span>Error</span>
            case head :: Nil => nextOne(head)
            case _ => nextSelect.toForm_!
          }} &
          "#cancel" #> SHtml.ajaxButton("Cancel", () => closePopup ) &
          "#confirm" #> SHtml.ajaxSubmit("Confirm", () => closePopup)
          )( SHtml.ajaxForm(popup) ++ Script((JsRaw("correctButtons();"))))) &  JsRaw("createPopup('changeStatePopup', 150, 850)")

  }
}
