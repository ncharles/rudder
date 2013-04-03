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
import com.normation.rudder.domain.workflows._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import scala.xml.Text
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import com.normation.rudder.web.model.CurrentUser
import org.joda.time.DateTime
import com.normation.rudder.services.workflows.ChangeRequestService
import com.normation.rudder.web.components.DateFormaterService
import scala.xml.Node
import scala.xml.Elem

class ChangeRequestManagement extends DispatchSnippet with Loggable {

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val roCrRepo = RudderConfig.roChangeRequestRepository
  private[this] val workflowService = RudderConfig.workflowService
  private[this] val changeRequestEventLogService = RudderConfig.changeRequestEventLogService
  private[this] val changeRequestTableId = "ChangeRequestId"

  private[this] val initFilter : Box[String] = S.param("filter").map(_.replace("_", " "))

  val dataTableInit =
    s"""$$('#${changeRequestTableId}').dataTable( {
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
            { "sWidth": "20px" },
            { "sWidth": "40px" },
            { "sWidth": "100px" },
            { "sWidth": "40px" },
            { "sWidth": "40px" }
          ],
        } );
        $$('.dataTables_filter input').attr("placeholder", "Search");
        ${initFilter match {
          case Full(filter) => s"$$('#${changeRequestTableId}').dataTable().fnFilter('${filter}',1,true,false,true);"
          case eb:EmptyBox => s"$$('#${changeRequestTableId}').dataTable().fnFilter('pending',1,true,false,true);"
          }
        }"""


  def CRLine(cr: ChangeRequest)=
    <tr>
      <td id="crId">
         {SHtml.a(() => S.redirectTo(s"/secure/utilities/changeRequest/${cr.id}"), Text(cr.id.value))}
      </td>
      <td id="crStatus">
         {workflowService.findStep(cr.id)}
      </td>
      <td id="crName">
         {cr.info.name}
      </td>
      <td id="crOwner">
         {changeRequestEventLogService.getChangeRequestHistory(cr.id) match {
           case eb :EmptyBox => "Error while fetching Creator"
           case Full(seq) => seq.headOption.map(_.actor.name).getOrElse("Unknown User")
         }}
      </td>
      <td id="crDate">
         {changeRequestEventLogService.getLastLog(cr.id) match {
           case eb :EmptyBox => "Error while fetching last action Date"
           case Full(seq) => seq.map(event => DateFormaterService.getFormatedDate(event.creationDate)).getOrElse("Error while fetching last action Date")
         }}
      </td>
   </tr>
  def dispatch = {
    case "filter" =>
      xml => ("#actualFilter *" #> statusFilter).apply(xml)
    case "display" => xml =>
      ( "#crBody" #> roCrRepo.getAll.get.flatMap(CRLine(_)) ).apply(xml) ++
      Script(OnLoad(JsRaw(dataTableInit)))
  }


  def statusFilter = {

    val values =  workflowService.stepsValue.map(_.value)
    val selectValues =  values.map(x=> (x,x))
    var value = ""

    val filterFunction =
      s"""var filter = [];
          $$(this).children(":selected").each(function () {
            filter.push($$(this).attr("value"));
          } );
          $$('#${changeRequestTableId}').dataTable().fnFilter(filter.join("|"),1,true,false,true);"""
    val onChange = ("onchange" -> JsRaw(filterFunction))



    def filterForm (select:Elem,text:String, transform: String => NodeSeq) = {
      val submit =
          SHtml.ajaxSubmit(
              text
            , () => SetHtml("actualFilter",transform(value))
            , ("class","expand")
            , ("style","margin: 5px 10px; float:right; height:15px; width:18px;  padding: 0; border-radius:25px")
         ) ++ Script(JsRaw("correctButtons()"))

      SHtml.ajaxForm(
        <b style="float:left; margin: 5px 10px">Status:</b> ++
        select % onChange  ++ submit
    )
    }
    def unexpandedFilter(default:String):NodeSeq = {
      val select = SHtml.select(
          ("","All") :: ("Pending","Pending") :: selectValues
        , Full(default)
        , list => value = list
        , ("style","width:auto;")
      )
      filterForm(select,"...",expandedFilter)
  }

    def expandedFilter(default:String) = {
      val extendedDefault =
        if (default == "Pending")
          values.filter(_.contains("Pending"))
        else if (values.exists(_ == default))
            List(default)
          else
            Nil

      def computeDefault(list:List[String]) =
        value = if (list.size==4)
          "All"
                            else if (list.forall(_.contains("Pending") && list.size == 2))
                                "Pending"
                              else
                                list.head
      val multiSelect =  SHtml.multiSelect(
            selectValues
          , extendedDefault
          , list => value = if (list.size==4)
                              "All"
                            else if (list.forall(_.contains("Pending") && list.size == 2))
                                "Pending"
                              else
                                list.head
          , ("style","width:auto;padding-right:3px;")
        )
      filterForm(multiSelect,".",unexpandedFilter)
    }

  unexpandedFilter(initFilter.getOrElse("Pending"))
  }
}
