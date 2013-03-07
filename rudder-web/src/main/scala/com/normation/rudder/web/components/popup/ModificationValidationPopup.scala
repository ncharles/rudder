/*
*************************************************************************************
* Copyright 2011 Normation SAS
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

package com.normation.rudder.web.components.popup

import net.liftweb.http.js._
import JsCmds._
import com.normation.rudder.domain.policies._
import JE._
import net.liftweb.common._
import net.liftweb.http.{SHtml,DispatchSnippet,Templates}
import scala.xml._
import net.liftweb.util.Helpers._
import com.normation.utils.StringUuidGenerator
import com.normation.rudder.web.model.{
  WBTextField, FormTracker, WBTextAreaField
}
import com.normation.cfclerk.services.TechniqueRepository
import com.normation.cfclerk.domain.{TechniqueVersion,TechniqueName}
import bootstrap.liftweb.RudderConfig
import com.normation.rudder.domain.workflows._
import com.normation.rudder.domain.nodes.NodeGroup
import net.liftweb.http.SHtml.ChoiceHolder
import com.normation.rudder.web.model.WBSelectField
import com.normation.cfclerk.domain.TechniqueName
import com.normation.rudder.domain.nodes.AddNodeGroupDiff
import com.normation.rudder.web.model.CurrentUser
import org.joda.time.DateTime
import com.normation.rudder.domain.nodes.ModifyToNodeGroupDiff
import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.rudder.web.components.RuleGrid
import com.normation.rudder.services.policies.OnlyDisableable
import com.normation.rudder.services.policies.OnlyEnableable
import com.normation.rudder.repository.RoChangeRequestRepository
import com.normation.rudder.repository.WoChangeRequestRepository
import com.normation.rudder.services.workflows.ChangeRequestService
import com.normation.rudder.services.workflows.WorkflowService

/**
 * Validation pop-up for modification on group and directive.
 *
 * The validation pop-up contains 3 mains parts, optionnaly empty:
 *
 * - the list of impacted Rules by that modification
 * - the change message
 * - the workflows part (asking what to do if the wf is enable)
 *
 */

object ModificationValidationPopup extends Loggable {
  val htmlId_popupContainer = "validationContainer"

  private def html = {
    val path = "templates-hidden" :: "components" :: "ModificationValidationPopup" :: Nil
    (for {
      xml <- Templates(path)
    } yield {
      chooseTemplate("component", "validationPopup", xml)
    }) openOr {
      logger.error("Missing template <component:validationPopup> at path: %s.html".format(path.mkString("/")))
      <div/>
    }
  }

  /* Text variation for
   * - Directive and groups,
   * - Enable & disable (for directive), delete, modify (save)
   *
   * Expects "Directive" or "Group" as argument
   */
  private def titles(item:String) = Map(
      "enable"  -> "Enable a Directive"
    , "disable" -> "Disable a Directive"
    , "delete"  -> s"Delete a ${item}"
    , "save"    -> s"Update a ${item}"
  )

  private def explanationMessages(item:String) = Map(
      "enable"  ->
      <div>
        <img src="/images/icWarn.png" alt="Warning!" height="32" width="32" class="warnicon"/>
        <h2>Are you sure that you want to enable this {item}?</h2>
        <br />
        <div id="dialogDisableWarning">
          Enabling this {item} will have an impact on the following Rules which apply it.
        </div>
      </div>
    , "disable" ->
      <div>
        <img src="/images/icWarn.png" alt="Warning!" height="32" width="32" class="warnicon"/>
        <h2>Are you sure that you want to disable this {item}?</h2>
        <br />
        <div id="dialogDisableWarning">
          Disabling this {item} will have an impact on the following Rules which apply it.
        </div>
      </div>
    , "delete"  ->
      <div>
        <img src="/images/icWarn.png" alt="Warning!" height="32" width="32" class="warnicon"/>
        <h2>Are you sure that you want to delete this {item}?</h2>
        <br />
        <div id="dialogDisableWarning">
          Deleting this {item} will also remove it from the following Rules.
        </div>
      </div>
    , "save"    ->
      <div>
         <img src="/images/icDetails.png" alt="Details" height="20" width="22" class="icon"/>
         <h2>Are you sure that you want to update this {item}?</h2>
         <br />
         <div id="directiveDisabled" class="nodisplay">
           <img src="/images/icWarn.png" alt="Warning!" height="32" width="32" class="warnicon"/>
           <b>Warning:</b> This {item} is currently disabled. Your changes will not take effect until it is enabled.
         </div>
         <div>
           Updating this {item} will have an impact on the following Rules which apply it.
         </div>
      </div>
  )

}


class ModificationValidationPopup(
    //if we are creating a new item, then None, else Some(x)
    item              : Either[
                            (TechniqueName, Directive, Option[Directive])
                          , (NodeGroup, Option[NodeGroup])
                        ]
  , action            : String //one among: save, delete, enable, disable
  , onSuccessCallback : NodeSeq => JsCmd = { x => Noop }
  , onFailureCallback : NodeSeq => JsCmd = { x => Noop }
) extends DispatchSnippet with Loggable {

  import ModificationValidationPopup._

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val userPropertyService = RudderConfig.userPropertyService
  private[this] val roChangeRequestRepo:RoChangeRequestRepository = ???
  private[this] val woChangeRequestRepo:WoChangeRequestRepository = ???
  private[this] val changeRequestService:ChangeRequestService = ???
  private[this] val workflowService:WorkflowService = ???
  private[this] val dependencyService      = RudderConfig.dependencyAndDeletionService


  def dispatch = {
    case "popupContent" => { _ => popupContent }
  }


  def popupContent() : NodeSeq = {
    val name = if(item.isLeft) "Directive" else "Group"

    (
      "#validationForm" #> { (xml:NodeSeq) => SHtml.ajaxForm(xml) } &
      "#dialogTitle" #> titles(name)(action) &
      "#explanationMessageZone" #> explanationMessages(name)(action) &
      "#disableItemDependencies" #> showDependentRules() &
      ".reasonsFieldsetPopup" #> {
        crReasons.map { f =>
          "#explanationMessage" #> <div>{userPropertyService.reasonsFieldExplanation}</div> &
          "#reasonFieldError" #> (if(f.hasErrors) {
                                   <ul>{f.errors.map { e => <li>{e}</li> }}</ul>
                                 } else { NodeSeq.Empty } ) &
          "#reasonsField" #> f.toForm_!
        }
      } &
      "#radioQuick" #> radioQuick &
      "#radioNew" #> radioNew &
      "#radioExisting" #> radioExisting &
      "#changeRequestName" #> changeRequestName.toForm &
      "#changeRequestDescription" #> changeRequestDescription.toForm &
      "#existingChangeRequest" #> existingChangeRequest &
      "#cancel" #> (SHtml.ajaxButton("Cancel", { () => closePopup() }) % ("tabindex","5")) &
      "#saveKeepOpen" #> (SHtml.ajaxSubmit("Configure", onSubmitKeepOpen) % ("id", "createDirectiveSaveButton") % ("tabindex","4")) &
      "#saveStartWorkflow" #> (SHtml.ajaxSubmit("Configure", onSubmitStartWorkflow _) % ("id", "createDirectiveSaveButton") % ("tabindex","3"))
    )(html ++ Script(OnLoad(JsRaw("correctButtons();"))))

  }

  private[this] def showDependentRules() : NodeSeq = {
    if(action == "create") {
      NodeSeq.Empty
    } else {

      val rules = item match {
        case Left((_, directive, _)) =>
          action match {
            case "delete" => dependencyService.directiveDependencies(directive.id).map(_.rules)
            case "disable" => dependencyService.directiveDependencies(directive.id, OnlyEnableable).map(_.rules)
            case "enable" => dependencyService.directiveDependencies(directive.id, OnlyDisableable).map(_.rules)
          }

        case Right((nodeGroup, _)) => dependencyService.targetDependencies(GroupTarget(nodeGroup.id)).map( _.rules)
      }

      rules match {
        case e: EmptyBox =>
          <div class="error">An error occurred while trying to find dependent item</div>
        case Full(rules) => {
          val cmp = new RuleGrid("remove_popup_grid", rules, None, false)
          cmp.rulesGrid(popup = true,linkCompliancePopup = false)
        }
      }
    }
  }

  ///////////// fields for category settings ///////////////////
  val (radioQuick, radioNew, radioExisting) = {
    val radios : ChoiceHolder[String] = SHtml.radio(Seq("quick","new", "existing"), Full(currentRadio), { s =>
      currentRadio = s
    } )
    (radios(0),radios(1),radios(2))
  }

  private[this] var currentRadio : String = "quick"

  private[this] val crReasons = {
    import com.normation.rudder.web.services.ReasonBehavior._
    userPropertyService.reasonsFieldBehavior match {
      case Disabled => None
      case Mandatory => Some(buildReasonField(true, "subContainerReasonField"))
      case Optionnal => Some(buildReasonField(false, "subContainerReasonField"))
    }
  }


  def buildReasonField(mandatory:Boolean, containerClass:String = "twoCol") = {
    new WBTextAreaField("Message", "") {
      override def setFilter = notNull _ :: trim _ :: Nil
      override def inputField = super.inputField  %  ("style" -> "height:8em;")
      override def subContainerClassName = containerClass
      override def validations() = {
        if(mandatory){
          valMinLen(5, "The reason must have at least 5 characters.") _ :: Nil
        } else {
          Nil
        }
      }
    }
  }

  private[this] val changeRequestName = new WBTextField("Name", "") {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def errorClassName = ""
    override def inputField = super.inputField % ("onkeydown" , "return processKey(event , 'createDirectiveSaveButton')") % ("tabindex","1")
    override def validations =
      valMinLen(3, "The name must have at least 3 characters") _ :: Nil
  }

  private[this] val changeRequestDescription = new WBTextAreaField("Short description", "") {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def inputField = super.inputField  % ("style" -> "height:7em") % ("tabindex","2")
    override def errorClassName = ""
    override def validations = Nil

  }

  //TODO : get existing change request
  private[this] val changeRequestList = Seq(("Private Draft 1", "pvd1"), ("Change Request 42", "cr42"))
  private[this] val existingChangeRequest = new WBSelectField("Existing change requests", changeRequestList, "") {

  }

  private[this] val formTracker = new FormTracker(
        crReasons.toList
    ::: changeRequestName
     :: changeRequestDescription
     :: existingChangeRequest
     :: Nil
  )

  private[this] var notifications = List.empty[NodeSeq]

  private[this] def error(msg:String) = <span class="error">{msg}</span>


  private[this] def closePopup() : JsCmd = {
    JsRaw(""" $.modal.close();""")
  }

  /**
   * Update the form when something happened
   */
  private[this] def updateFormClientSide() : JsCmd = {
    SetHtml(htmlId_popupContainer, popupContent())
  }


  private[this] def onSubmitStartWorkflow() : JsCmd = {
    onSubmit(keepOpen = false)
  }

  private[this] def onSubmitKeepOpen() : JsCmd = {
    onSubmit(keepOpen = true)
  }

  private[this] def onSubmit(keepOpen:Boolean) : JsCmd = {

    if(formTracker.hasErrors) {
      onFailure
    } else {

      //based on the choice of the user, create or update a Change request
      val savedChangeRequest = {
        currentRadio match {
          case "quick" | "new" =>
            val cr = item match {
              case Left((techniqueName, directive, optOriginal)) =>
                changeRequestService.createChangeRequestFromDirective(
                    changeRequestName.get
                  , changeRequestDescription.get
                  , techniqueName
                  , directive
                  , optOriginal
                  , CurrentUser.getActor
                  , crReasons.map( _.get )
                )
              case Right((nodeGroup, optOriginal)) =>
                changeRequestService.createChangeRequestFromNodeGroup(
                    changeRequestName.get
                  , changeRequestDescription.get
                  , nodeGroup
                  , optOriginal
                  , CurrentUser.getActor
                  , crReasons.map( _.get )
                )
            }
            woChangeRequestRepo.createChangeRequest(cr)

          case "existing" =>
            for {
              cr    <- roChangeRequestRepo.get(ChangeRequestId(existingChangeRequest.get))
              ccr   <- cr match {
                         case x:ConfigurationChangeRequest => Full(x)
                         case x => Failure(s"ChangeRequest of type ${x.getClass} can not be updated with a node group or a directive, only ConfigurationChangeRequest can")
                       }
              newCr =  item match {
                         case Left((techniqueName, directive, optOriginal)) =>
                           changeRequestService.updateChangeRequestWithDirective(
                               ccr
                             , techniqueName
                             , directive
                             , optOriginal
                             , CurrentUser.getActor
                             , crReasons.map( _.get )
                           )
                         case Right((nodeGroup, optOriginal)) =>
                           changeRequestService.updateChangeRequestWithNodeGroup(
                               ccr
                             , nodeGroup
                             , optOriginal
                             , CurrentUser.getActor
                             , crReasons.map( _.get )
                           )
                       }
              saved <- woChangeRequestRepo.updateChangeRequest(newCr)
            } yield {
              saved
            }
        }
      }

      //other steps based on user choice: close the ChangeRequest and start the wf
      val res = if(keepOpen) savedChangeRequest else {
        for {
          saved     <- savedChangeRequest
          closed    <- woChangeRequestRepo.setReadOnly(saved.id)
          wfStarted <- workflowService.startWorkflow(closed)
        } yield {
          wfStarted
        }
      }

      res match {
        case Full(_) => onSuccessCallback(Text("TODO: a custom message if in Draft, or a workflow status"))
        case eb:EmptyBox =>
          val e = (eb ?~! "Error when trying to save you modification")
          e.rootExceptionCause.foreach { ex =>
            logger.error(s"Exception when trying to update a change request:", ex)
          }
          onFailureCallback(Text(e.messageChain))
      }
    }
  }


  private[this] def onFailure : JsCmd = {
    formTracker.addFormError(error("The form contains some errors, please correct them"))
    updateFormClientSide()
  }


  private[this] def updateAndDisplayNotifications() : NodeSeq = {
    notifications :::= formTracker.formErrors
    formTracker.cleanErrors

    if(notifications.isEmpty) NodeSeq.Empty
    else {
      val html = <div id="notifications" class="notify"><ul>{notifications.map( n => <li>{n}</li>) }</ul></div>
      notifications = Nil
      html
    }
  }
}


