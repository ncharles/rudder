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

class ChangeRequestManagement extends DispatchSnippet with Loggable {

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestTableId = "ChangeRequestId"

  private[this] val CrId: Box[String] = S.param("crId")

  val dummyStatus = ChangeRequestStatus("Draft","blablabla",false)
  val dummyStatusChange = ChangeRequestStatusChange(dummyStatus,AddChangeRequestStatusDiff,Seq())
  val dummyCR = ConfigurationChangeRequest(ChangeRequestId("1"),dummyStatusChange,Map())
  def dispatch = {
    case "filter" => xml => <div>{CrId.map(_ => SHtml.ajaxButton("back",() => changerUrl(""))).getOrElse("no cr asked for")}<div id="content"> future filter here </div></div>

    case "display" => xml => <div> {
      ( "#crBody *" #> Seq(dummyCR,dummyCR).map(CRLine(_))).apply(CRTable)
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
      <tbody id="crBody">
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
         {SHtml.a(() => changerUrl(cr.id), Text(cr.id))}
      </td>
      <td id="crStatus">
         {cr.status.firstChange}
      </td>
      <td id="crName">
         {cr.status.initialState.name}
      </td>
      <td id="crOwner">
         {"someone"}
      </td>
      <td id="crDate">
         {"date"}
      </td>
   </tr>

}
