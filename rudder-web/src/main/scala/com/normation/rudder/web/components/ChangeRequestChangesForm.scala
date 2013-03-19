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