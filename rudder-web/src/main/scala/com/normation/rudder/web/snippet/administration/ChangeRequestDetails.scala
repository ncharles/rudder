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
import org.joda.time.DateTime
import com.normation.cfclerk.domain.TechniqueId
import com.normation.rudder.domain.policies.AddDirectiveDiff
import com.normation.rudder.domain.policies.DirectiveDiff
import com.normation.rudder.domain.workflows.ChangeRequestInfo
import com.normation.rudder.services.eventlog.ChangeRequestEventLogService


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
  }

  def displayHeader(cr:ChangeRequest) = {

    //last action:
    val action = changeRequestEventLogService.getLastLog(cr.id) match {
      case eb:EmptyBox => "Error when retrieving the last action"
      case Full(None)  => "Error, no action were recorded for that change request" //should not happen here !
      case Full(Some(ChangeRequestEventLog(_,_,_,diff))) => diff match {
        case ModifyToChangeRequestDiff(_) => "Modified"
        case AddChangeRequestDiff(_)    => "Created"
        case DeleteChangeRequestDiff(_) => "Deleted"
        case RebaseChangeRequestDiff(_) => "Resynchronise with current values"
      }
    }

    ("#backButton *" #> SHtml.ajaxButton("back",() => S.redirectTo("/secure/administration/changeRequests")) &
       "#CRName *" #> cr.info.name &
       "#CRStatus *" #> "status" &
       "#CRLastAction *" #> s"${ action }") (header)

  }


}

object ChangeRequestEditForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "details", xml)
    }) openOr Nil
 }

class ChangeRequestEditForm (
    var changeRequest: ChangeRequestInfo
  , SuccessCallback  : ChangeRequestInfo => JsCmd
) extends DispatchSnippet with Loggable {

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

class ChangeRequestChangesForm(var changeRequest:ChangeRequest)  extends DispatchSnippet with Loggable {
import ChangeRequestChangesForm._

  val roDirectiveRepo = RudderConfig.roDirectiveRepository
  val changeRequestEventLogService =  RudderConfig.changeRequestEventLogService

  def dispatch = {
    case "changes" => { _ => ("#changeTree ul *" #> {changeRequest match {case cr: ConfigurationChangeRequest => treeNode(cr).toXml
    case _ => Text("not implemented :(")}
    }).apply(form) ++ Script(JsRaw(s"""buildChangesTree("#changeTree","${S.contextPath}");""")) }


  }

 def treeNode(changeRequest:ConfigurationChangeRequest) = new JsTreeNode{


  def directiveChild(directiveId:DirectiveId) = new JsTreeNode{
    val directive= roDirectiveRepo.getDirective(directiveId)

      val body =         SHtml.a(
          {() => SetHtml("changeDisplay",displayHistory(List(changeRequest.directives(directiveId).changes)))}
        , <span>{directive.map(_.name).getOrElse("Unknown Directive")}</span>
        )
  val children = Nil
  }
  val directivesChild = new JsTreeNode{
      val body =         SHtml.a(
          {() => SetHtml("changeDisplay",displayHistory() )}
        , <span>Directives</span>
        )
  val children = changeRequest.directives.keys.map(directiveChild(_)).toList
  }
  val body =         SHtml.a(
          {() => SetHtml("changeDisplay",displayHistory () )}
        , <span>Changes</span>
        )
  val children = directivesChild :: Nil
//  val startStatusItem = ChangeRequestStatusItem(CurrentUser.getActor,DateTime.now.minusDays(1),None,changeRequest.originalStatusLog.diff)

  val logs = changeRequestEventLogService.getChangeRequestHistory(changeRequest.id).getOrElse(Seq())

  override val attrs = List(( "rel" -> { "changeType" } ))
  def displayHistory (directives : List[DirectiveChange] = changeRequest.directives.values.map(_.changes).toList)= {
  ( "#crBody" #> {logs.flatMap(CRLine(_)) ++ directives.flatMap(CRLine(_))}).
  apply(CRTable) ++ Script(
        JsRaw(s"""$$('#changeHistory').dataTable( {
                    "asStripeClasses": [ 'color1', 'color2' ],
                    "bAutoWidth": false,
                    "bFilter" : true,
                    "bPaginate" : true,
                    "bLengthChange": true,
                    "sPaginationType": "full_numbers",
                    "bJQueryUI": true,
                    "oLanguage": {
                      "sSearch": ""
                    },
                    "sDom": '<"dataTables_wrapper_top"fl>rt<"dataTables_wrapper_bottom"ip>',
                    "aaSorting": [[ 1, "asc" ]],
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


  def CRLine(cr: ChangeRequestEventLog)=
    <tr>
      <td id="action">
         {cr.diff match {
           case AddChangeRequestDiff(_) => "Create"
           case ModifyToChangeRequestDiff(_) => "Modify"
           case DeleteChangeRequestDiff(_) => "Delete"
           case RebaseChangeRequestDiff( _) => "Rebased"
         }}
      </td>
      <td id="actor">
         {cr.actor}
      </td>
      <td id="date">
         {cr.creationDate}
      </td>
   </tr>

  def CRLine(cr: DirectiveChange)=
    <tr>
      <td id="action">
         {cr.firstChange.diff}
      </td>
      <td id="actor">
         {cr.firstChange.actor}
      </td>
      <td id="date">
         {cr.firstChange.creationDate}
      </td>
   </tr>


}

}



