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

class ChangeRequestManagement extends DispatchSnippet with Loggable {

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestTableId = "ChangeRequestId"

  private[this] val CrId: Box[String] = S.param("crId")

  val dummyStatus = ChangeRequestStatus("MyFirstChangeRequest","blablabla",false)
  val dummyStatus2 = ChangeRequestStatus("MySecondChangeRequest","blablabla",false)
  val startStatus = AddChangeRequestStatusDiff(dummyStatus)
  val startStatus2 = AddChangeRequestStatusDiff(dummyStatus2)
  val dummyStatusChange = ChangeRequestStatusHistory(startStatus)
  val dummyStatusChange2 = ChangeRequestStatusHistory(startStatus2)
  val dummyCR = ConfigurationChangeRequest(ChangeRequestId("1"),dummyStatusChange,Map())
  val dummyCR2 = ConfigurationChangeRequest(ChangeRequestId("2"),dummyStatusChange2,Map())

    val CRTable =
    <table id={changeRequestTableId}>
      <thead>
       <tr class="head tablewidth">
        <th>ID</th>
        <th>Status</th>
        <th>Name</th>
        <th>Owner</th>
        <th>last modified date</th>
      </tr>
      </thead>
      <tbody >
      <div id="crBody"/>
      </tbody>
    </table>


  def changerUrl(id:String):JsCmd = {
    JsRaw(s"""
    pageurl = 'changeRequest/${id}';
    //to get the ajax content and display in div with id 'content'
    $$.ajax({url:pageurl,success: function(data){
      $$('#content').html(pageurl);
    }});
    console.log(window.history)
    //to change the browser URL to the given link location
    if(pageurl!=window.location){
      window.history.replaceState({path:pageurl},'',pageurl);
    }
  """)
  }
  def CRLine(cr: ChangeRequest)=
    <tr>
      <td id="crId">
         {SHtml.a(() => S.redirectTo(s"changeRequest/${cr.id}"), Text(cr.id))}
      </td>
      <td id="crStatus">
         {cr.statusHistory.history.lastOption.getOrElse(cr.statusHistory.initialState)}
      </td>
      <td id="crName">
         {cr.status.name}
      </td>
      <td id="crOwner">
         {"someone"}
      </td>
      <td id="crDate">
         {"date"}
      </td>
   </tr>
  def dispatch = {
    case "filter" => xml => CrId match { case eb:EmptyBox => <div id="content">

      <div id="nameFilter"><span><b>Status</b><span id="actualFilter">{statusFilter}</span></span></div>
    </div>
      /* detail page */
    case Full(id) =>
      <div>
        <div>{SHtml.ajaxButton("back",() => S.redirectTo("/secure/administration/changeRequest"))}</div>
        <h2 style="float:left; margin-left:50px;font-size:60px;">{if (id=="1") dummyCR.status.name else if (id=="2") dummyCR2.status.name else "not a CR" }</h2>
        <div class="statusdiv" style="float: right; color : #F79D10; font-size:30px; background-color:#111; margin-right:50px; padding:5px" >status</div>
      </div>
    }
    case "display" => xml => CrId match { case eb:EmptyBox => <div> {
      ( "#crBody" #> Seq(dummyCR,dummyCR2).flatMap(CRLine(_)) ).apply(CRTable)
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
     case Full(id) => <div>{SHtml.ajaxButton("back",() => S.redirectTo("/secure/administration/changeRequest"))}</div>
    }
  }




  val noexpand:NodeSeq = SHtml.select(Seq(("Validation","Validation"),("Draft","Draft")), Full("Draft"), list => logger.info(list)) %
         ("onchange" ->
        JsRaw(s"""
        var filter = [];
        $$(this).children(":selected").each(function () {
            filter.push($$(this).text());
                });

           $$('#${changeRequestTableId}').dataTable().fnFilter(filter.join("|"),2,true,false,true);  """)) ++
       SHtml.ajaxButton("+", () => SetHtml("actualFilter",expand))

  val expand =  SHtml.multiSelect(Seq(("Validation","Validation"),("Draft","Draft")), List(), list => logger.info(list)) % ("onchange" ->
        JsRaw(s"""
        var filter = [];
        $$(this).children(":selected").each(function () {
            filter.push($$(this).text());
                });
           $$('#${changeRequestTableId}').dataTable().fnFilter(filter.join("|"),2,true,false,true);  """)) ++
       SHtml.ajaxButton("-", () => SetHtml("actualFilter",noexpand))

   def statusFilter = {
           noexpand
   }
}
