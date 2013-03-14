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
import com.normation.rudder.domain.workflows.ChangeRequest
import com.normation.rudder.domain.nodes.NodeGroup
import net.liftweb.http.SHtml.ChoiceHolder

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
      SHtml.ajaxForm(chooseTemplate("component", "validationPopup", xml))
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
    item              : Either[(TechniqueName, Directive), NodeGroup]
  , action            : String //one among: save, delete, enable, disable
  , onSuccessCallback : (Directive) => JsCmd = { (directive : Directive) => Noop }
  , onFailureCallback : () => JsCmd = { () => Noop }
) extends DispatchSnippet with Loggable {

  import ModificationValidationPopup._

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val userPropertyService = RudderConfig.userPropertyService

  def dispatch = {
    case "popupContent" => { _ => popupContent }
  }


  def popupContent() : NodeSeq = {
    val name = if(item.isLeft) "Directive" else "Group"

    (
      "#dialogTitle" #> titles(name)(action) &
      "#explanationMessageZone" #> explanationMessages(name)(action) &
      "#reasonField" #> updateAndDisplayNotifications() &
      ".reasonsFieldsetPopup" #> {
        crReasons.map { f =>
          "#explanationMessage" #> <div>{userPropertyService.reasonsFieldExplanation}</div> &
          "#reasonFieldError" #> (if(f.hasErrors) {
                                   <ul>{f.errors.map { e => <li>{e}</li> }}</ul>
                                 } else { NodeSeq.Empty } ) &
          "#reasonsField" #> f.toForm_!
        }
      } &
      "#cancel" #> (SHtml.ajaxButton("Cancel", { () => closePopup() }) % ("tabindex","4"))&
      "#save" #> (SHtml.ajaxSubmit("Configure", onSubmit _) % ("id", "createDirectiveSaveButton") % ("tabindex","3"))
    )(html ++ Script(OnLoad(JsRaw("correctButtons();"))))

  }

  ///////////// fields for category settings ///////////////////
  val (radioQuick, radioNew, radioExisting) = {
    val radios : ChoiceHolder[String] = SHtml.radio(Seq("quick","new", "existing"), Empty, { s =>
      ??? //TODO
    } )
    (radios(0),radios(1),radios(2))
  }

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

  private[this] val formTracker = new FormTracker(crReasons.toList ::: changeRequestName :: changeRequestDescription :: Nil)

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

  private[this] def onSubmit() : JsCmd = {

    if(formTracker.hasErrors) {
      onFailure & onFailureCallback()
    } else {

      //TODO
      //closePopup() & onSuccessCallback(directive)
      ???
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


