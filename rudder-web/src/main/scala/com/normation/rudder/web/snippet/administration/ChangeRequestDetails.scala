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
import bootstrap.liftweb.RudderConfig
import net.liftweb.http._
import net.liftweb.http.js._
import JE._
import JsCmds._
import net.liftweb.util._
import Helpers._
import scala.xml.Text
import scala.xml.NodeSeq
import com.normation.rudder.domain.workflows._
import com.normation.rudder.web.model._
import org.joda.time.DateTime
import com.normation.cfclerk.domain.TechniqueId
import com.normation.rudder.domain.policies._
import com.normation.rudder.web.components._


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





