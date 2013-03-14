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
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.DispatchSnippet
import bootstrap.liftweb.RudderConfig
import net.liftweb.http._
import net.liftweb.http.js._
import JE._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import scala.xml.Text
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import com.normation.rudder.domain.workflows._
import com.normation.rudder.web.model._
import com.normation.rudder.domain.policies.DirectiveId


object ChangeRequestDetails {
  def header =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "header", xml)
    }) openOr Nil
 }
class ChangeRequestDetails extends DispatchSnippet with Loggable {
import ChangeRequestDetails._

  var dummyStatus = ChangeRequestStatus("MyFirstChangeRequest","blablabla",false)
  var dummyStatus2 = ChangeRequestStatus("MySecondChangeRequest","blablabla",false)
  val startStatus = AddChangeRequestStatusDiff(dummyStatus)
  val startStatus2 = AddChangeRequestStatusDiff(dummyStatus2)
  var dummyStatusChange = ChangeRequestStatusHistory(startStatus)
  var dummyStatusChange2 = ChangeRequestStatusHistory(startStatus2)
  var dummyCR = ConfigurationChangeRequest(ChangeRequestId("1"),dummyStatusChange,Map())
  var dummyCR2 = ConfigurationChangeRequest(ChangeRequestId("2"),dummyStatusChange2,Map())

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestTableId = "ChangeRequestId"
  private[this] val CrId: Box[String] = {S.param("crId") }
  private[this] var Cr: Box[ChangeRequest] = CrId match {case Full("1") => Full(dummyCR)
  case Full("2") => Full(dummyCR2)
  case Full(id) => Failure(s"${id} is not a good Change request id")
  case eb:EmptyBox => val fail = eb ?~ "no id selected"
  Failure(s"Error in the cr id asked: ${fail.msg}")
  }


  def dispatch = {
    case "header" => xml => Cr match { case eb:EmptyBox => <div id="content">

      <div> Error</div>
    </div>
      /* detail page */
    case Full(id) => displayHeader(id)
    }

    case "details" => xml =>
    Cr match { case eb:EmptyBox => <div> Error {eb}</div>

     case Full(cr) => new ChangeRequestEditForm(cr.status, (statusUpdate:ChangeRequestStatus) =>  {
       val newCR = Cr//.map(_.changeStatus(statusUpdate))
       Cr = newCR
       logger.warn(Cr)
       SetHtml("changeRequestHeader",displayHeader(newCR.get))}).display
    }
    case "changes" => xml => Cr match { case eb:EmptyBox => <div> Error</div>

     case Full(id) => <div>{new ChangeRequestChangesForm(id).dispatch("changes")(NodeSeq.Empty)}</div>
    }
  }

  def displayHeader(cr:ChangeRequest) =
    ("#backButton *" #> SHtml.ajaxButton("back",() => S.redirectTo("/secure/administration/changeRequests")) &
       "#CRName *" #> cr.status.name &
       "#CRStatus *" #> "status" &
       "#CRLastAction *" #> s"${cr.statusHistory.history.last.diff match {
         case ModifyToChangeRequestStatusDiff(_) => "Modified"
         case AddChangeRequestStatusDiff(_)    => "Created"
         case DeleteChangeRequestStatusDiff => "Deleted"}}") (header)




}

object ChangeRequestEditForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "details", xml)
    }) openOr Nil
 }

class ChangeRequestEditForm (var changeRequest: ChangeRequestStatus,
    SuccessCallback: ChangeRequestStatus => JsCmd)   extends DispatchSnippet with Loggable {
import ChangeRequestEditForm._

  def dispatch = {
    case "details" => { _ => display }
  }
  private[this] val changeRequestName =
    new WBTextField("Name", changeRequest.name) {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def className = "twoCol"
    override def validations =
      valMinLen(3, "The name must have at least 3 characters") _ :: Nil
  }

  private[this] val changeRequestDescription=
    new WBTextAreaField("Description", changeRequest.description) {
      override def className = "twoCol"
      override def setFilter = notNull _ :: trim _ :: Nil
      override val maxLen = 255
      override def validations = Nil
  }


  def display: NodeSeq = { logger.info(changeRequest)
    ("#detailsForm *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
        ClearClearable &
        "#CRName *" #> changeRequestName.toForm_! &
        "#CRDescription *" #> changeRequestDescription.toForm_! &
        "#CRSave" #> SHtml.ajaxSubmit("Save", () =>  submit)
        ) (form) ++ Script(JsRaw("correctButtons();"))}

  def submit = {
    changeRequest = changeRequest.copy(name=changeRequestName.is, description = changeRequestDescription.is)
    SuccessCallback(changeRequest) & SetHtml("changeRequestDetails",display)
  }
}

object ChangeRequestChangesForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "changes", xml)
    }) openOr Nil
 }

class ChangeRequestChangesForm(changeRequest:ChangeRequest)  extends DispatchSnippet with Loggable {
import ChangeRequestChangesForm._

  def dispatch = {
    case "changes" => { _ => form }


  }

}

case class CRRootreeNode(changeRequest:ConfigurationChangeRequest) extends JsTreeNode{
  val body = Text("Changes")
  val children = if(changeRequest.directives.isEmpty) Nil else List(CRDirectivesTreeNode(changeRequest.directives))
  override val attrs = List(( "rel" -> { "changeType" } ))
}


case class CRDirectivesTreeNode(directives:Map[DirectiveId,DirectiveChanges]) extends JsTreeNode{
  val body = Text("Directives")
  val children = Nil
    override val attrs = List(( "rel" -> { "changeType" } ))
}


