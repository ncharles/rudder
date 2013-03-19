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
import org.joda.time.DateTime
import com.normation.rudder.web.model._
import com.normation.rudder.domain.policies.DirectiveId
import org.joda.time.DateTime
import com.normation.cfclerk.domain.TechniqueId
import com.normation.rudder.domain.policies.AddDirectiveDiff
import com.normation.rudder.domain.policies.DirectiveDiff
import com.normation.rudder.domain.policies.AddDirectiveDiff
import com.normation.rudder.web.components.DateFormaterService


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


  val techRepo = RudderConfig.techniqueRepository
  val rodirective = RudderConfig.roDirectiveRepository
  val adirectiveId = DirectiveId("11c2fdab-053e-4100-a663-b6a29e8b049c")
  val directive = rodirective.getDirective(adirectiveId).get
  val tech = rodirective.getActiveTechnique(adirectiveId).get.techniqueName
  val aDirectiveAddDiff = AddDirectiveDiff(tech,directive)
  val aDirectiveChangeItem =  DirectiveChangeItem(CurrentUser.getActor,DateTime.now.minusHours(6),None,aDirectiveAddDiff)
  val aDirectiveChange = DirectiveChange(Some(directive),aDirectiveChangeItem,Seq())
  val aDirectiveChanges = DirectiveChanges(aDirectiveChange,Seq())

  val adirectiveId2 = DirectiveId("E7221865-CEB6-46BB-B621-AA2B2E1466F1".toLowerCase())
  val directive2 = rodirective.getDirective(adirectiveId2).get
  val tech2 = rodirective.getActiveTechnique(adirectiveId2).get.techniqueName
  val aDirectiveAddDiff2 = AddDirectiveDiff(tech2,directive2)
  val aDirectiveChangeItem2 =  DirectiveChangeItem(CurrentUser.getActor,DateTime.now.minusHours(4),None,aDirectiveAddDiff2)
  val aDirectiveChange2 = DirectiveChange(Some(directive2),aDirectiveChangeItem2,Seq())
  val aDirectiveChanges2 = DirectiveChanges(aDirectiveChange2,Seq())
  var dummyStatus = ChangeRequestStatus("MyFirstChangeRequest","blablabla",false)
  var dummyStatus2 = ChangeRequestStatus("MySecondChangeRequest","blablabla",false)
  var dummyStatusChange = ChangeRequestStatusItem(CurrentUser.getActor,DateTime.now.minusDays(1),None,AddChangeRequestStatusDiff(dummyStatus))
  var dummyStatusChange2 = ChangeRequestStatusItem(CurrentUser.getActor,DateTime.now.minusHours(9),None,AddChangeRequestStatusDiff(dummyStatus2))
  var dummyCR = ConfigurationChangeRequest(ChangeRequestId("1"),List(dummyStatusChange),Map((adirectiveId,aDirectiveChanges),(adirectiveId2,aDirectiveChanges2)),Map())
  var dummyCR2 = ConfigurationChangeRequest(ChangeRequestId("2"),List(dummyStatusChange2),Map((adirectiveId,aDirectiveChanges),(adirectiveId2,aDirectiveChanges2)),Map())

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

     case Full(cr) => new ChangeRequestEditForm(cr.status,cr.id, (statusUpdate:ChangeRequestStatus) =>  {
       val newCR = Cr.map(_.updateStatus(statusUpdate,CurrentUser.getActor))
       Cr = newCR
       logger.warn(Cr)
       SetHtml("changeRequestHeader",displayHeader(newCR.get))& SetHtml("changeRequestChanges",new ChangeRequestChangesForm(newCR.get).dispatch("changes")(NodeSeq.Empty))
 }).display    }
    case "changes" => xml => Cr match { case eb:EmptyBox => <div> Error</div>

     case Full(id) => <div id="changeRequestChanges">{new ChangeRequestChangesForm(id).dispatch("changes")(NodeSeq.Empty)}</div>
    }
   case "actions" => xml => Cr match { case eb:EmptyBox => NodeSeq.Empty

     case Full(id) => ("#backStep" #> SHtml.ajaxButton("Refuse", () => Noop) &
         "#nextStep" #> SHtml.ajaxButton("Valid", () => Noop))(xml)

    }
  }

  def displayHeader(cr:ChangeRequest) =
    ("#backButton *" #> SHtml.ajaxButton("← Back",() => S.redirectTo("/secure/utilities/changeRequests")) &
       "#CRName *" #> s"CR#${cr.id}: ${cr.status.name}" &
       "#CRStatus *" #> "Status" &
       "#CRLastAction *" #> { val status = cr.statusHistory.head
         s"${status.diff match {
         case ModifyToChangeRequestStatusDiff(_) => "Modified"
         case AddChangeRequestStatusDiff(_)    => "Created"
         case DeleteChangeRequestStatusDiff => "Deleted"}} on ${DateFormaterService.getFormatedDate(status.creationDate)} by ${status.actor.name}"}) (header)




}

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

object ChangeRequestChangesForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "changes", xml)
    }) openOr Nil
 }

class ChangeRequestChangesForm(var changeRequest:ChangeRequest)  extends DispatchSnippet with Loggable {
import ChangeRequestChangesForm._

  val roDirectiveRepo = RudderConfig.roDirectiveRepository

  def dispatch = {
    case "changes" => { _ => changeRequest match {case cr: ConfigurationChangeRequest => ("#changeTree ul *" #>  treeNode(cr).toXml &
      "#history *" #> displayHistory (cr.directives.values.map(_.changes).toList) &
      "#diff *" #> diff(cr.directives.values.map(_.changes).toList)).apply(form) ++ Script(JsRaw(s"""buildChangesTree("#changeTree","${S.contextPath}");
    $$( "#changeDisplay" ).tabs();"""))
    case _ => Text("not implemented :(")
    }
    }


  }

 def treeNode(changeRequest:ConfigurationChangeRequest) = new JsTreeNode{


  def directiveChild(directiveId:DirectiveId) = new JsTreeNode{
    val directive= roDirectiveRepo.getDirective(directiveId)

      val body =         SHtml.a(
          {() => SetHtml("history",displayHistory(List(changeRequest.directives(directiveId).changes)))}
        , <span>{directive.map(_.name).getOrElse("Unknown Directive")}</span>
        )
  val children = Nil
  }
  val directivesChild = new JsTreeNode{
      val body =         SHtml.a(
          {() => SetHtml("history",displayHistory(changeRequest.directives.values.map(_.changes).toList) )}
        , <span>Directives</span>
        )
  val children = changeRequest.directives.keys.map(directiveChild(_)).toList
    override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "directives"}))
  }

  val rulesChild = new JsTreeNode{
      val body =         SHtml.a(
          {() => SetHtml("history",displayHistory(Nil) )}
        , <span>Rules</span>
        )
  val children = Nil
    override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "directives"}))
  }

  val groupsChild = new JsTreeNode{
      val body =         SHtml.a(
          {() => SetHtml("history",displayHistory(Nil ))}
        , <span>Groups</span>
        )
  val children = Nil
    override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "directives"}))
  }

  val body =         SHtml.a(
          {() => SetHtml("history",displayHistory (changeRequest.directives.values.map(_.changes).toList) )}
        , <span>Changes</span>
        )
  val children = directivesChild :: rulesChild :: groupsChild ::  Nil

  override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "changes"}))

}
 def displayHistory (directives : List[DirectiveChange])= {
  ( "#crBody" #> { changeRequest.statusHistory.flatMap(CRLine(_)) ++ directives.flatMap(CRLine(_))}).
  apply(CRTable) ++ Script(SetHtml("diff",diff(directives)) &
        JsRaw(s"""$$('#changeHistory').dataTable( {
                    "asStripeClasses": [ 'color1', 'color2' ],
                    "bAutoWidth": false,
                    "bFilter" : true,
                    "bPaginate" : true,
                    "bLengthChange": true,
                    "sPaginationType": "full_numbers",
                    "bJQueryUI": true,
                    "aaSorting": [[ 2, "desc" ]],
                    "oLanguage": {
                      "sSearch": ""
                    },
                    "sDom": '<"dataTables_wrapper_top"fl>rt<"dataTables_wrapper_bottom"ip>',
                    "aoColumns": [
                      { "sWidth": "100px" },
                      { "sWidth": "40px" },
                      { "sWidth": "40px" }
                    ],
                  } );
                  $$('.dataTables_filter input').attr("placeholder", "Search"); """))
  }
    val CRTable =
    <table id="changeHistory">
      <thead>
       <tr class="head tablewidth">
        <th>Action</th>
        <th>Actor</th>
        <th>Date</th>
      </tr>
      </thead>
      <tbody >
      <div id="crBody"/>
      </tbody>
    </table>

  def diff(cr : List[DirectiveChange]) = cr match {
      case directive::Nil => Text(directive.initialState.get.name)
      case Nil => Text("Nothing to show")
      case list => <ul>{list.flatMap(directive => <li>{directive.initialState.get.name}</li>)}</ul>
    }
  def CRLine(cr: ChangeRequestStatusItem)=
    <tr>
      <td id="action">
         {cr.diff match {
           case AddChangeRequestStatusDiff(_) => "Change request created"
           case ModifyToChangeRequestStatusDiff(_) => "Change request details changed"
           case DeleteChangeRequestStatusDiff => "Change request deleted"
           case RebaseChangeRequestStatusDiff => "Rebased"
         }}
      </td>
      <td id="actor">
         {cr.actor.name}
      </td>
      <td id="date">
         {DateFormaterService.getFormatedDate(cr.creationDate)}
      </td>
   </tr>

  def CRLine(cr: DirectiveChange)=
    <tr>
      <td id="action">
         {cr.firstChange.diff match {
           case a :AddDirectiveDiff => s"Create Directive ${a.directive.name}"
           case _ => "another change ..."
         }}
      </td>
      <td id="actor">
         {cr.firstChange.actor.name}
      </td>
      <td id="date">
         {DateFormaterService.getFormatedDate(cr.firstChange.creationDate)}
      </td>
   </tr>



}



