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
import com.normation.rudder.domain.nodes.NodeGroupId
import com.normation.rudder.domain.nodes.AddNodeGroupDiff
import com.normation.rudder.domain.nodes.DeleteNodeGroupDiff
import com.normation.rudder.domain.nodes.ModifyNodeGroupDiff
import com.normation.rudder.domain.nodes.ModifyToNodeGroupDiff
import com.normation.cfclerk.domain.TechniqueId


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
  val techniqueRepo = RudderConfig.techniqueRepository
  val roGroupRepo = RudderConfig.roNodeGroupRepository
  val changeRequestEventLogService =  RudderConfig.changeRequestEventLogService
  val workFlowEventLogService =  RudderConfig.workflowEventLogService
  val diffService =  RudderConfig.diffService

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
    val directiveName = changeRequest.directives(directiveId).changes.firstChange.diff match{
           case a :AddDirectiveDiff => a.directive.name
           case d :DeleteDirectiveDiff => d.directive.name
           case modTo : ModifyToDirectiveDiff => modTo.directive.name
    }

    val body = SHtml.a(
        () => SetHtml("history",displayHistory(changes))
      , <span>{directiveName}</span>
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

    def groupChild(groupId:NodeGroupId) = new JsTreeNode{

    val directive= roGroupRepo.getNodeGroup(groupId)

    val changes = List(changeRequest.nodeGroups(groupId).changes)
    val groupeName = changeRequest.nodeGroups(groupId).changes.firstChange.diff match{
           case a :AddNodeGroupDiff => a.group.name
           case d :DeleteNodeGroupDiff => d.group.name
           case modTo : ModifyToNodeGroupDiff => modTo.group.name
           case mod : ModifyNodeGroupDiff => mod.name
    }
    val body = SHtml.a(
        () => SetHtml("history",displayHistory(List()))
      , <span>{groupeName}</span>
    )

    val children = Nil
  }
  val groupsChild = new JsTreeNode{
    val changes =  changeRequest.nodeGroups.values.map(_.changes).toList
    val body = SHtml.a(
        () => SetHtml("history",displayHistory(Nil) )
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


  ( "#crBody" #> {
    workFlowEventLogService.getChangeRequestHistory(changeRequest.id).getOrElse(Seq()).flatMap(CRLine(_)) ++
    logs.flatMap(CRLine(_)) ++
    directives.flatMap(CRLine(_))
    }
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

  private[this] val xmlPretty = new scala.xml.PrettyPrinter(80, 2)

  private[this] val DirectiveXML =
    <div>
      <h4>Directive overview:</h4>
      <ul class="evlogviewpad">
        <li><b>ID:&nbsp;</b><value id="directiveID"/></li>
        <li><b>Name:&nbsp;</b><value id="directiveName"/></li>
        <li><b>Description:&nbsp;</b><value id="shortDescription"/></li>
        <li><b>Technique name:&nbsp;</b><value id="techniqueName"/></li>
        <li><b>Technique version:&nbsp;</b><value id="techniqueVersion"/></li>
        <li><b>Priority:&nbsp;</b><value id="priority"/></li>
        <li><b>Enabled:&nbsp;</b><value id="isEnabled"/></li>
        <li><b>System:&nbsp;</b><value id="isSystem"/></li>
        <li><b>Details:&nbsp;</b><value id="longDescription"/></li>
        <li><b>Parameters:&nbsp;</b><value id="parameters"/></li>
      </ul>
    </div>
  def diff(cr : List[DirectiveChange]) =
    <ul>{
      cr.flatMap(directiveChange =>
        <li>{
          directiveChange.change.map(_.diff match {
            case AddDirectiveDiff(techniqueName,directive) =>
              val techniqueId = TechniqueId(techniqueName,directive.techniqueVersion)
              val parameters = techniqueRepo.get(techniqueId).map(_.rootSection) match {
                case Some(rs) =>
                  xmlPretty.format(SectionVal.toXml(SectionVal.directiveValToSectionVal(rs,directive.parameters)))
                case None =>
                  logger.error(s"Could not find rootSection for technique ${techniqueName.value} version ${directive.techniqueVersion}" )
                  <div> directive.parameters </div>
              }
              ("#directiveID" #> directive.id.value.toUpperCase &
               "#directiveName" #> directive.name &
               "#techniqueVersion" #> directive.techniqueVersion.toString &
               "#techniqueName" #> techniqueName.value &
               "#techniqueVersion" #> directive.techniqueVersion.toString &
               "#techniqueName" #> techniqueName.value &
               "#priority" #> directive.priority &
               "#isEnabled" #> directive.isEnabled &
               "#isSystem" #> directive.isSystem &
               "#shortDescription" #> directive.shortDescription &
               "#longDescription" #> directive.longDescription &
               "#parameters" #> <pre>{parameters}</pre>
              )(DirectiveXML)
            case DeleteDirectiveDiff(techniqueName,directive) =>
              val techniqueId = TechniqueId(techniqueName,directive.techniqueVersion)
              val parameters = techniqueRepo.get(techniqueId).map(_.rootSection) match {
                case Some(rs) =>
                  xmlPretty.format(SectionVal.toXml(SectionVal.directiveValToSectionVal(rs,directive.parameters)))
                case None =>
                  logger.error(s"Could not find rootSection for technique ${techniqueName.value} version ${directive.techniqueVersion}" )
                  <div> directive.parameters </div>
              }
              ("#directiveID" #> directive.id.value.toUpperCase &
               "#directiveName" #> directive.name &
               "#techniqueVersion" #> directive.techniqueVersion.toString &
               "#techniqueName" #> techniqueName.value &
               "#techniqueVersion" #> directive.techniqueVersion.toString &
               "#techniqueName" #> techniqueName.value &
               "#priority" #> directive.priority &
               "#isEnabled" #> directive.isEnabled &
               "#isSystem" #> directive.isSystem &
               "#shortDescription" #> directive.shortDescription &
               "#longDescription" #> directive.longDescription &
               "#parameters" #> <pre>{parameters}</pre>
              )(DirectiveXML)
            case ModifyToDirectiveDiff(techniqueName,directive,rootSection) =>
              val Some((initialDirective,initialRS)) = directiveChange.initialState.map(init => (init._2,init._3))
              val techniqueId = TechniqueId(techniqueName,directive.techniqueVersion)
              val parameters = techniqueRepo.get(techniqueId).map(_.rootSection)
              val diff = diffService.diffDirective(initialDirective, initialRS, directive, rootSection)
              ("#directiveID" #> directive.id.value.toUpperCase &
               "#directiveName" #> diff.modName.map(value => displaydirectiveInnerFormDiff(value, "name")).getOrElse(Text(directive.name)) &
               "#techniqueVersion" #> diff.modTechniqueVersion.map(value => displaydirectiveInnerFormDiff(value, "techniqueVersion")).getOrElse(Text(directive.techniqueVersion.toString)) &
               "#techniqueName" #> techniqueName.value &
               "#priority" #> diff.modPriority.map(value => displaydirectiveInnerFormDiff(value, "priority")).getOrElse(Text(directive.priority.toString)) &
               "#isEnabled" #> diff.modIsActivated.map(value => displaydirectiveInnerFormDiff(value, "active")).getOrElse(Text(directive.isEnabled.toString)) &
               "#isSystem" #> directive.isSystem &
               "#shortDescription" #> diff.modShortDescription.map(value => displaydirectiveInnerFormDiff(value, "short")).getOrElse(Text(directive.shortDescription)) &
               "#longDescription" #> diff.modLongDescription.map(value => displaydirectiveInnerFormDiff(value, "long")).getOrElse(Text(directive.longDescription)) &
               "#parameters" #> {
               implicit val fun = (section:SectionVal) => xmlPretty.format(SectionVal.toXml(section))
                 diff.modParameters.map{
                 displaydirectiveInnerFormDiff(_,"parameters")
               }.getOrElse(<pre>{fun(SectionVal.directiveValToSectionVal(rootSection,directive.parameters))}</pre>)
               }
              )(DirectiveXML)
            }
          ).getOrElse(<div>Error</div>)
        }</li>
      )
    }
  </ul>
  private[this] def displaydirectiveInnerFormDiff[T](diff: SimpleDiff[T], name:String)(implicit fun: T => String = (t:T) => t.toString) = {
    <pre style="width:200px;" id={s"before${name}"}
    class="nodisplay">{fun(diff.oldValue)}</pre>
    <pre style="width:200px;" id={s"after${name}"}
    class="nodisplay">{fun(diff.newValue)}</pre>
    <pre id={s"result${name}"} ></pre>  ++
    Script(
      OnLoad(
        JsRaw(
          s"""
            var before = "before${name}";
            var after  = "after${name}";
            var result = "result${name}";
            makeDiff(before,after,result);"""
        )
      )
    )
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
    def CRLine(cr: WorkflowProcessEventLog)=
    cr match { case StepWorkflowProcessEventLog(actor, date, reason, from, to) =>
    <tr>
      <td id="action">
         {s"Change workflow step from ${from} to ${to}"
         }
      </td>
      <td id="actor">
         {actor.name}
      </td>
      <td id="date">
         {DateFormaterService.getFormatedDate(date)}
      </td>
   </tr>
    case _ => NodeSeq.Empty
    }

  def CRLine(cr: DirectiveChange)=
    <tr>
      <td id="action">
         {cr.firstChange.diff match {
           case a : AddDirectiveDiff => s"Create Directive ${a.directive.name}"
           case d : DeleteDirectiveDiff => s"Delete Directive ${d.directive.name}"
           case m : ModifyToDirectiveDiff => s"Modify Directive ${m.directive.name}"
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