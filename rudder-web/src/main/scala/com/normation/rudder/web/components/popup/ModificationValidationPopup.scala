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
import com.normation.rudder.repository.WoDraftChangeRequestRepository
import com.normation.rudder.services.workflows.ChangeRequestService
import com.normation.rudder.repository.RoChangeRequestRepository
import com.normation.rudder.repository.WoChangeRequestRepository
import com.normation.rudder.services.workflows.WorkflowService
import com.normation.cfclerk.domain.SectionSpec
import com.normation.rudder.web.model.RudderBaseField

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
    val path = "templates-hidden" :: "Popup" :: "ModificationValidationPopup" :: Nil
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
    , "create"  -> s"Create a ${item}"    
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
    , "create"    ->
      <div><h2>Are you sure that you want to create this {item}?</h2></div>
  )

}


class ModificationValidationPopup(
    //if we are creating a new item, then None, else Some(x)
    item              : Either[
                            (TechniqueName, SectionSpec ,Directive, Option[Directive])
                          , (NodeGroup, Option[NodeGroup])
                        ]
  , action            : String //one among: save, delete, enable, disable or create
  , isANewItem        : Boolean
  , onSuccessCallback : NodeSeq => JsCmd = { x => Noop }
  , onFailureCallback : NodeSeq => JsCmd = { x => Noop }
) extends DispatchSnippet with Loggable {

  import ModificationValidationPopup._

  private[this] val userPropertyService      = RudderConfig.userPropertyService
  private[this] val roDraftChangeRequestRepo = RudderConfig.roDraftChangeRequestRepository
  private[this] val woChangeRequestRepo      = RudderConfig.woChangeRequestRepository
  private[this] val changeRequestService     = RudderConfig.changeRequestService
  private[this] val workflowService          = RudderConfig.workflowService
  private[this] val dependencyService        = RudderConfig.dependencyAndDeletionService
  private[this] val woDraftChangeRequestRepo = RudderConfig.woDraftChangeRequestRepository
  private[this] val workflowEnabled          = RudderConfig.RUDDER_ENABLE_APPROVAL_WORKFLOWS

  def dispatch = {
    case "popupContent" => { _ => popupContent }
  }

  def popupContent() : NodeSeq = {
    val name = if(item.isLeft) "Directive" else "Group"
    val buttonName = workflowEnabled match {
      case true => "Create Draft"
      case false => "Save"
    }
    (
      "#validationForm" #> { (xml:NodeSeq) => SHtml.ajaxForm(xml) } andThen
      "#dialogTitle *" #> titles(name)(action) &
      "#explanationMessageZone" #> explanationMessages(name)(action) &
      "#disableItemDependencies" #> showDependentRules() &
      ".reasonsFieldsetPopup" #> {
        crReasons.map { f =>
          <div>
            <div style="margin:10px 0px 5px 0px; color:#444">
              {userPropertyService.reasonsFieldExplanation}
            </div>
              {f.toForm_!}
        </div>
        }
      } &
      "#newChangeRequest [class]" #> (if (workflowEnabled) Text("display") else Text("nodisplay")) &
      "#changeRequestName" #> changeRequestName.toForm &
      "#changeRequestDescription" #> changeRequestDescription.toForm &
      "#existingChangeRequest" #> existingChangeRequest.toForm &
//      "#cancel" #> (SHtml.ajaxButton("Cancel", { () => closePopup() }) % ("tabindex","5")) &
      "#saveStartWorkflow" #> (SHtml.ajaxSubmit(buttonName, onSubmitStartWorkflow _) % ("id", "createDirectiveSaveButton") % ("tabindex","3"))
    )(html ++ Script(OnLoad(
        JsRaw("correctButtons();"))))
  }

  private[this] def showError(field:RudderBaseField) : NodeSeq = {
    if(field.hasErrors) {
      <ul>{field.errors.map { e => <li>{e}</li> }}</ul>
    } else { NodeSeq.Empty }
  }

  private[this] def showDependentRules() : NodeSeq = {
    if(isANewItem) {
      NodeSeq.Empty
    } else {

      val rules = item match {
        case Left((_, _, directive, _)) =>
          action match {
            case "delete" => dependencyService.directiveDependencies(directive.id).map(_.rules)
            case "disable" | "save" => dependencyService.directiveDependencies(directive.id, OnlyEnableable).map(_.rules)
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
      //override def subContainerClassName = containerClass
      override def validations() = {
        if(mandatory){
          valMinLen(5, "The reason must have at least 5 characters.") _ :: Nil
        } else {
          Nil
        }
      }
    }
  }

  private[this] val defaultRequestName = item match {
    case Left((t,r,d,opt)) => s"Update Directive ${d.name}"
    case Right((g,opt)) => s"Update Group ${g.name}"
  }

  private[this] val changeRequestName = new WBTextField("Name", defaultRequestName) {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def errorClassName = ""
    override def inputField = super.inputField % ("onkeydown" , "return processKey(event , 'createDirectiveSaveButton')") % ("tabindex","1")
    override def validations =
      valMinLen(3, "The name must have at least 3 characters") _ :: Nil
  }

  private[this] val changeRequestDescription = new WBTextAreaField("Description", "") {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def inputField = super.inputField  % ("style" -> "height:7em") % ("tabindex","2") % ("class" -> "nodisplay")
    override def errorClassName = ""
    override def validations = Nil

  }

  //TODO : get existing change request
  private[this] val changeRequestList = Seq(("Private Draft 1", "pvd1"), ("Change Request 42", "cr42"))
  private[this] val existingChangeRequest = new WBSelectField("Existing change requests", changeRequestList, "") {
    override def inputField = super.inputField % ("class" -> "nodisplay")
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
    onSubmit()
  }

  private[this] def DirectiveDiffFromAction(
      techniqueName: TechniqueName
    , rootSection  : SectionSpec
    , directive    : Directive
    , initialState : Option[Directive]
  ) : Box[ChangeRequestDirectiveDiff] = {
    initialState match {
      case None =>
        if ((action=="save") || (action == "create"))
          Full(AddDirectiveDiff(techniqueName,directive))
        else
          Failure(s"Action ${action} is not possible on a new directive")
      case Some(d) =>
        action match {
          case "delete" => Full(DeleteDirectiveDiff(techniqueName,directive))
          case "save"|"disable"|"enable"|"create" => Full(ModifyToDirectiveDiff(techniqueName,directive,rootSection))
          case _ =>         Failure(s"Action ${action} is not possible on a existing directive")
        }
    }
  }

  private[this] def onSubmit() : JsCmd = {

    if(formTracker.hasErrors) {
      onFailure
    } else {

      //based on the choice of the user, create or update a Change request
      val savedChangeRequest = {
        // we only have quick change request now
        val cr = item match {
          case Left((techniqueName, rootSection, directive, optOriginal)) =>
              val action = DirectiveDiffFromAction(techniqueName, rootSection, directive, optOriginal)
              action.map(
                changeRequestService.createChangeRequestFromDirective(
                      changeRequestName.get
                    , changeRequestDescription.get
                    , techniqueName
                    , rootSection
                    , directive.id
                    , optOriginal
                    , _
                    , CurrentUser.getActor
                    , crReasons.map( _.get )
                ) )

          case Right((nodeGroup, optOriginal)) =>
              Full(changeRequestService.createChangeRequestFromNodeGroup(
                  changeRequestName.get
                , changeRequestDescription.get
                , nodeGroup
                , optOriginal
                , CurrentUser.getActor
                , crReasons.map(_.get))
              )
        }
        cr.flatMap { cr =>
          for {
            saved     <- woChangeRequestRepo.createChangeRequest(cr, CurrentUser.getActor, crReasons.map(_.get))
            wfStarted <- workflowService.startWorkflow(saved.id, CurrentUser.getActor, crReasons.map(_.get))
          } yield {
            saved.id
          }
        }
      }

      savedChangeRequest match {
        case Full(_) =>
          val changeText = workflowEnabled match {
            case true => 
              item match {
                case Left((techniqueName, rootSection, directive, optOriginal)) =>
                  <div>Your change on directive <b>{directive.name}</b> has been submited</div>
                case Right((nodeGroup, optOriginal)) =>
                  <div>Your change on group <b>{nodeGroup.name}</b> has been submited</div>
              }
            case false =>
              // No workflow means nothing to warn the user about
              <div/>
          }          
          onSuccessCallback(changeText)
        case eb:EmptyBox =>
          val e = (eb ?~! "Error when trying to save your modification")
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


