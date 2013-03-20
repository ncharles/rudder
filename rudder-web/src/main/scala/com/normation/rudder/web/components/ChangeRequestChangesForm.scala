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
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.workflows._
import com.normation.rudder.web.model._
import bootstrap.liftweb._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._


object ChangeRequestChangesForm {
  def form =
    (for {
      xml <- Templates("templates-hidden" :: "components" :: "ComponentChangeRequest" :: Nil)
    } yield {
      chooseTemplate("component", "changes", xml)
    }) openOr Nil
 }

class ChangeRequestChangesForm(
    var changeRequest:ChangeRequest
 ) extends DispatchSnippet with Loggable {
import ChangeRequestChangesForm._

  val roDirectiveRepo = RudderConfig.roDirectiveRepository
  val changeRequestEventLogService =  RudderConfig.changeRequestEventLogService

  def dispatch = {
    case "changes" => _ => changeRequest match {
      case cr: ConfigurationChangeRequest =>
        ( "#changeTree ul *" #>  treeNode(cr).toXml &
          "#history *" #> displayHistory (cr.directives.values.map(_.changes).toList) &
          "#diff *" #> diff(cr.directives.values.map(_.changes).toList)).apply(form) ++
          Script(JsRaw(s"""buildChangesTree("#changeTree","${S.contextPath}");
                           $$( "#changeDisplay" ).tabs();""") )
      case _ => Text("not implemented :(")
    }


  }

 def treeNode(changeRequest:ConfigurationChangeRequest) = new JsTreeNode{

  def directiveChild(directiveId:DirectiveId) = new JsTreeNode{

    val directive= roDirectiveRepo.getDirective(directiveId)

    val changes = List(changeRequest.directives(directiveId).changes)

    val body = SHtml.a(
        () => SetHtml("history",displayHistory(changes))
      , <span>{directive.map(_.name).getOrElse("Unknown Directive")}</span>
    )

    val children = Nil
  }


  val directivesChild = new JsTreeNode{

    val changes = changeRequest.directives.values.map(_.changes).toList
    val body = SHtml.a(
        {() => SetHtml("history",displayHistory(changes) )}
      , <span>Directives</span>
    )
    val children = changeRequest.directives.keys.map(directiveChild(_)).toList

    override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "directives"}))
  }

  val rulesChild = new JsTreeNode{
    val changes = Nil
    val body = SHtml.a(
        () => SetHtml("history",displayHistory(changes) )
      , <span>Rules</span>
    )
    val children = Nil
    override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "rules"}))
  }

  val groupsChild = new JsTreeNode{
    val changes = Nil
    val body = SHtml.a(
        () => SetHtml("history",displayHistory(changes) )
      , <span>Groups</span>
    )
    val children = Nil
    override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "groups"}))
  }

  val body = SHtml.a(
     () => SetHtml("history",displayHistory (changeRequest.directives.values.map(_.changes).toList) )
   , <span>Changes</span>
  )

  val children = directivesChild :: rulesChild :: groupsChild ::  Nil

  override val attrs = List(( "rel" -> { "changeType" } ),("id" -> { "changes"}))

}

  def displayHistory (directives : List[DirectiveChange])= {
    val logs = changeRequestEventLogService.getChangeRequestHistory(changeRequest.id).getOrElse(Seq())


  ( "#crBody" #> { logs.flatMap(CRLine(_)) ++ directives.flatMap(CRLine(_))}
  ) apply CRTable ++
    Script(
      SetHtml("diff",diff(directives) ) &
      JsRaw(s"""
        $$('#changeHistory').dataTable( {
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
        $$('.dataTables_filter input').attr("placeholder", "Search"); """
    ) )
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
      case Nil            => Text("Nothing to show")
      case list           => <ul>{list.flatMap(directive => <li>{directive.initialState.get.name}</li>)}</ul>
    }

  def CRLine(cr: ChangeRequestEventLog)=
    <tr>
      <td id="action">
         {cr.diff match {
           case AddChangeRequestDiff(_) => "Change request created"
           case ModifyToChangeRequestDiff(_) => "Change request details changed"
           case DeleteChangeRequestDiff(_) => "Change request deleted"
           case RebaseChangeRequestDiff(_) => "Rebased"
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