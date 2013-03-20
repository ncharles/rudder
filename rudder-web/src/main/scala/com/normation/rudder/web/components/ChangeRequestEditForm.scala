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

package com.normation.rudder.web.components

import scala.xml._
import com.normation.rudder.domain.workflows._
import com.normation.rudder.web.model._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._
import net.liftweb.util._
import net.liftweb.util.Helpers._

object ChangeRequestEditForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "details", xml)
    }) openOr Nil
 }

class ChangeRequestEditForm (var status: ChangeRequestInfo,crId:ChangeRequestId,
    SuccessCallback: ChangeRequestInfo => JsCmd)   extends DispatchSnippet with Loggable {
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