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
import com.normation.cfclerk.domain.TechniqueId
import com.normation.rudder.domain.nodes.NodeGroupDiff
import com.normation.rudder.domain.nodes.ChangeRequestNodeGroupDiff
import com.normation.eventlog.ModificationId
import com.normation.rudder.batch.AutomaticStartDeployment
import com.normation.rudder.domain.eventlog.RudderEventActor


/**
 * Validation pop-up for modification on group and directive.
 *
 * The validation pop-up contains 3 mains parts, optionnaly empty:
 *
 * - the list of impacted Rules by that modification
 * - the change message
 * - the workflows part (asking what to do if the wf is enable)
 * 
 * For Creation or Clone, there is no Workflow, hence no ChangeRequest !
 * On success, we return on the forms with the newly created directive
 * On failure, we return on the form with the error
 * 
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
      <div>Are you sure you want to create the {item}?</div>
  )

}


class ModificationValidationPopup(
    //if we are creating a new item, then None, else Some(x)
    item              : Either[
                            (TechniqueName, ActiveTechniqueId, SectionSpec ,Directive, Option[Directive])
                          , (NodeGroup, Option[NodeGroup])
                        ]
  , action            : String //one among: save, delete, enable, disable or create
  , isANewItem        : Boolean
  , onSuccessCallback : ChangeRequestId => JsCmd = { x => Noop }
  , onFailureCallback : NodeSeq => JsCmd = { x => Noop }
  , onCreateSuccessCallBack : (Either[Directive,ChangeRequestId]) => JsCmd = { x => Noop }
  , onCreateFailureCallBack : JsCmd = { Noop }
  , parentFormTracker : Option[FormTracker] = None
) extends DispatchSnippet with Loggable {

  import ModificationValidationPopup._

  private[this] val userPropertyService      = RudderConfig.userPropertyService
  private[this] val woChangeRequestRepo      = RudderConfig.woChangeRequestRepository
  private[this] val changeRequestService     = RudderConfig.changeRequestService
  private[this] val workflowService          = RudderConfig.workflowService
  private[this] val dependencyService        = RudderConfig.dependencyAndDeletionService
  private[this] val workflowEnabled          = RudderConfig.RUDDER_ENABLE_APPROVAL_WORKFLOWS

  private[this] val directiveRepository      = RudderConfig.woDirectiveRepository
  private[this] val uuidGen                  = RudderConfig.stringUuidGenerator
  private[this] val techniqueRepo            = RudderConfig.techniqueRepository
  private[this] val asyncDeploymentAgent     = RudderConfig.asyncDeploymentAgent

  def dispatch = {
    case "popupContent" => { _ => popupContent }
  }

  def popupContent() : NodeSeq = {
    val name = if(item.isLeft) "Directive" else "Group"
    val (buttonName, classForButton) = workflowEnabled match {
      case true => 
        isANewItem match {
          case false => ("Submit for Validation", "wideButton")
          case true => ("Create", "")
        }
      case false => ("Save", "")
    }
    val titleWorkflow = workflowEnabled match {
      case true => 
        <div>
          <h2 style="padding-left:42px;">Workflows are enabled in Rudder, your change has to be validated in a change request</h2>
        </div>
      case false => NodeSeq.Empty
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
      "#titleWorkflow *" #> titleWorkflow &
      "#changeRequestName [class]" #> (if ((workflowEnabled)&(!isANewItem)) Text("display") else Text("nodisplay")) &
      "#changeRequestName" #> changeRequestName.toForm &
//      "#cancel" #> (SHtml.ajaxButton("Cancel", { () => closePopup() }) % ("tabindex","5")) &
      "#saveStartWorkflow" #> (SHtml.ajaxSubmit(buttonName, () => onSubmitStartWorkflow(), ("class" -> classForButton)) % ("id", "createDirectiveSaveButton") % ("tabindex","3")) andThen
       ".notifications *" #> updateAndDisplayNotifications()
      
    )(html ++ Script(OnLoad(JsRaw("correctButtons();"))))
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
        case Left((_, _, _, directive, _)) =>
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
    case Left((t,a,r,d,opt)) => s"Update Directive ${d.name}"
    case Right((g,opt)) => s"Update Group ${g.name}"
  }

  private[this] val changeRequestName = new WBTextField("Title", defaultRequestName) {
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
  /*
  private[this] val changeRequestList = Seq(("Private Draft 1", "pvd1"), ("Change Request 42", "cr42"))
  private[this] val existingChangeRequest = new WBSelectField("Existing change requests", changeRequestList, "") {
    override def inputField = super.inputField % ("class" -> "nodisplay")
  }*/

  // The formtracker needs to check everything only if its not a creation and there is workflow
  private[this] val formTracker = {
    if ((workflowEnabled)&(!isANewItem)) {
        new FormTracker(
                crReasons.toList
            ::: changeRequestName
             :: changeRequestDescription
             :: Nil
        )
        
    } else {
      new FormTracker(
               crReasons.toList
      )
    }
  }
  private[this] def error(msg:String) = <span class="error">{msg}</span>


  private[this] def closePopup() : JsCmd = {
    JsRaw("""$.modal.close();""") 
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
    , directive    : Directive
    , initialState : Option[Directive]
  ) : Box[ChangeRequestDirectiveDiff] = {

    techniqueRepo.get(TechniqueId(techniqueName,directive.techniqueVersion)).map(_.rootSection) match {
      case None => Failure(s"Could not get root section for technique ${techniqueName.value} version ${directive.techniqueVersion}")
      case Some(rootSection) =>
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
  }


  private[this] def groupDiffFromAction(
      group        : NodeGroup
    , initialState : Option[NodeGroup]
  ) : Box[ChangeRequestNodeGroupDiff] = {
      ???
  }
  private[this] def onSubmit() : JsCmd = {

    if(formTracker.hasErrors) {
      onFailure
    } else {
      // we create a CR only if we are not creating
      if (!isANewItem) {
        //based on the choice of the user, create or update a Change request
        val savedChangeRequest = {
          // we only have quick change request now
          val cr = item match {
            case Left((techniqueName, activeTechniqueId, oldRootSection, directive, optOriginal)) =>
                val action = DirectiveDiffFromAction(techniqueName, directive, optOriginal)
                action.map(
                  changeRequestService.createChangeRequestFromDirective(
                        changeRequestName.get
                      , crReasons.map( _.get ).getOrElse("")
                      , techniqueName
                      , oldRootSection
                      , directive.id
                      , optOriginal
                      , _
                      , CurrentUser.getActor
                      , crReasons.map( _.get )
                  ) )
  
            case Right((nodeGroup, optOriginal)) =>
                val action = groupDiffFromAction(nodeGroup, optOriginal)
                action.map(
                changeRequestService.createChangeRequestFromNodeGroup(
                    changeRequestName.get
                  , crReasons.map( _.get ).getOrElse("")
                  , nodeGroup
                  , optOriginal
                  , _
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
          case Full(cr) =>
            onSuccessCallback(cr)
          case eb:EmptyBox =>
            val e = (eb ?~! "Error when trying to save your modification")
            e.rootExceptionCause.foreach { ex =>
              logger.error(s"Exception when trying to update a change request:", ex)
            }
            onFailureCallback(Text(e.messageChain))
        }
      } else {
        // if creation or clone, we create everything immediately
        item match {
          case Left((techniqueName, activeTechniqueId, oldRootSection, directive, optOriginal)) =>
            saveAndDeployDirective(directive, activeTechniqueId, crReasons.map( _.get ))
          case _ => 
            Alert("var")
        }
      }
    }
  }

  private[this] def saveAndDeployDirective(
      directive: Directive
    , activeTechniqueId: ActiveTechniqueId
    , why:Option[String]
    ): JsCmd = {
    val modId = ModificationId(uuidGen.newUuid)
    directiveRepository.saveDirective(activeTechniqueId, directive, modId, CurrentUser.getActor, why) match {
      case Full(optChanges) =>
        optChanges match {
          case Some(_) => // There is a modification diff, launch a deployment.
            asyncDeploymentAgent ! AutomaticStartDeployment(modId, RudderEventActor)
          case None => // No change, don't launch a deployment
        }
        println("this exactly")
        closePopup() & onCreateSuccessCallBack(Left(directive))
      case Empty => 
        parentFormTracker match {
          case None => 
            logger.error("Invalid use of modificationValidationPopup : no parentFormTracker defined when creating/cloning directive") 
          case Some(tracker) => 
            tracker.addFormError(Text("There was an error on creating this directive"))
        
        }
        closePopup() & onCreateFailureCallBack
      case Failure(m, _, _) =>
        parentFormTracker match {
          case None => 
            logger.error("Invalid use of modificationValidationPopup : no parentFormTracker defined when creating/cloning directive")
          case Some(tracker) =>
            tracker.addFormError(Text(m))

        }
        closePopup() & onCreateFailureCallBack        
    }
  }
  
  private[this] def onFailure : JsCmd = {
    formTracker.addFormError(error("The form contains some errors, please correct them"))
    updateFormClientSide()
  }


  private[this] def updateAndDisplayNotifications() : NodeSeq = {
    val notifications = formTracker.formErrors
    formTracker.cleanErrors
    if(notifications.isEmpty) NodeSeq.Empty
    else {
      val html = <div id="notifications" class="notify"><ul>{notifications.map( n => <li>{n}</li>) }</ul></div>
      html
    }
  }
}


