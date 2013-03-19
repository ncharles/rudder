package com.normation.rudder.web.components

import com.normation.rudder.domain.workflows._
import net.liftweb.http.js.JsCmd
import net.liftweb.http._
import com.normation.rudder.web.model._
import net.liftweb.common._
import net.liftweb.http.js.JsCmds._
import scala.xml._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE._

object ChangeRequestEditForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "details", xml)
    }) openOr Nil
 }

class ChangeRequestEditForm (var status: ChangeRequestStatus,crId:ChangeRequestId,
    SuccessCallback: ChangeRequestStatus => JsCmd)   extends DispatchSnippet with Loggable {
import ChangeRequestEditForm._

  def dispatch = {
    case "details" => { _ => display }
  }
  private[this] val changeRequestName =
    new WBTextField("Name", status.name) {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def className = "twoCol"
    override def validations =
      valMinLen(3, "The name must have at least 3 characters") _ :: Nil
  }

  private[this] val changeRequestDescription=
    new WBTextAreaField("Description", status.description) {
      override def className = "twoCol"
      override def setFilter = notNull _ :: trim _ :: Nil
      override val maxLen = 255
      override def validations = Nil
  }


  def display: NodeSeq = { logger.info(status)
    ("#detailsForm *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
        ClearClearable &
        "#rebaseButton *" #> {if (status.readOnly) Text("you can't apply those change on actual state anymore") else <span>Reprepare your change request over the current configuration?</span>++SHtml.ajaxButton("Reprepare", () => Noop,("style","margin-left:10px;"))} &
       "#warning [class+]" #> {if (true/* condition de rebase*/) "" else "nodisplay"} &
        "#CRName *" #> changeRequestName.toForm_! &
        "#CRId *"   #> crId.value &
        "#CRStatus *"   #> "Status" &
        "#CRDescription *" #> changeRequestDescription.toForm_! &
        "#CRSave" #> SHtml.ajaxSubmit("Save", () =>  submit)
        ) (form) ++ Script(JsRaw("correctButtons();"))}

  def submit = {
    status = status.copy(name=changeRequestName.is, description = changeRequestDescription.is)
    SuccessCallback(status) & SetHtml("changeRequestDetails",display)
  }
}