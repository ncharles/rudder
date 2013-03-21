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

class ChangeRequestManagement extends DispatchSnippet with Loggable {

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val roCrRepo = RudderConfig.roChangeRequestRepository
  private[this] val workflowService = RudderConfig.workflowService
  private[this] val changeRequestEventLogService = RudderConfig.changeRequestEventLogService
  private[this] val changeRequestTableId = "ChangeRequestId"

  private[this] val CrId: Box[String] = S.param("crId")

  val dummyStatus = ChangeRequestInfo("MyFirstChangeRequest","blablabla",false)
  val dummyStatus2 = ChangeRequestInfo("MySecondChangeRequest","blablabla",false)
//  val startStatus = ChangeRequestStatusItem(CurrentUser.getActor, DateTime.now, None, AddChangeRequestStatusDiff(dummyStatus))
//  val startStatus2 = ChangeRequestStatusItem(CurrentUser.getActor, DateTime.now, None, AddChangeRequestStatusDiff(dummyStatus2))
  val dummyCR = ConfigurationChangeRequest(ChangeRequestId("1"), dummyStatus, Map(), Map())
  val dummyCR2 = ConfigurationChangeRequest(ChangeRequestId("2"), dummyStatus2,Map(), Map())

    val CRTable =
    <table id={changeRequestTableId}>
      <thead>
       <tr class="head tablewidth">
        <th>ID</th>
        <th>Status</th>
        <th>Name</th>
        <th>Creator</th>
        <th>Last modification</th>
      </tr>
      </thead>
      <tbody >
      <div id="crBody"/>
      </tbody>
    </table>


  def CRLine(cr: ChangeRequest)=
    <tr>
      <td id="crId">
         {SHtml.a(() => S.redirectTo(s"changeRequest/${cr.id}"), Text(cr.id.value))}
      </td>
      <td id="crStatus">
         {workflowService.findStep(cr.id)}
      </td>
      <td id="crName">
         {cr.info.name}
      </td>
      <td id="crOwner">
         {"someone"}
      </td>
      <td id="crDate">
         {"date"}
      </td>
   </tr>
  def dispatch = {
    case "filter" => xml => <div id="content">

      <div id="nameFilter" style="margin-left:40px;"><span><span id="actualFilter" style="margin-left:10px; display:inline-block">{statusFilter}</span></span></div>
    </div>
    case "display" => xml =>  <div style="margin: 0 40px; overflow:auto;"> {
      ( "#crBody" #> /*Seq(dummyCR,dummyCR2)*/roCrRepo.getAll.get.flatMap(CRLine(_)) ).apply(CRTable)
      }
    </div> ++ Script(OnLoad(
        JsRaw(s"""$$('#${changeRequestTableId}').dataTable( {
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
                  $$('.dataTables_filter input').attr("placeholder", "Search"); """)))
  }





   def statusFilter = {
     val values =  "Draft" :: "Validation" :: Nil
     val selectValues =  values.map(x=> (x,x))
     var value = ""
     val onChange = (
         "onchange" -> JsRaw(s"""
             var filter = [];
             $$(this).children(":selected").each(function () {
               filter.push($$(this).attr("value"));
             } );
             $$('#${changeRequestTableId}').dataTable().fnFilter(filter.join("|"),1,true,false,true);
         """)
     )

     def submit(text:String, transform: String => NodeSeq) =
       SHtml.ajaxSubmit(
           text
         , () => SetHtml("actualFilter",transform(value))
         , ("class","expand")
         , ("style","margin: 5px 10px; float:right; height:15px; width:18px;  padding: 0; border-radius:25px")
       ) ++ Script(JsRaw("correctButtons()"))

     def unexpandedFilter(default:String):NodeSeq = {
    SHtml.ajaxForm(
      <b style="float:left; margin: 5px 10px">Status:</b> ++
      SHtml.select(
          ("","All")::selectValues
        , Full(default)
        , list => {logger.info(list)
          value = list}
        , ("style","width:auto;")
      ) % onChange  ++ submit("...",expandedFilter)

    )
  }

    def expandedFilter(default:String) = {
      val extendedDefault = if (values.exists(_ == default)) List(default) else Nil
      SHtml.ajaxForm(
        <b style="float:left; margin: 5px 10px">Status:</b> ++
        SHtml.multiSelect(
            selectValues
          , extendedDefault
          , list => {logger.warn(list)
            value = if (list.size==1) list.head else "All"}
          , ("style","width:auto;padding-right:3px;")
        ) % onChange ++ submit(".",unexpandedFilter)
      )
    }

  unexpandedFilter("All")
  }
}
