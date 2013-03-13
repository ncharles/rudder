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
import com.normation.rudder.web.model.WBTextField

class ChangeRequestDetails extends DispatchSnippet with Loggable {


  val dummyStatus = ChangeRequestStatus("Draft","blablabla",false)
  val dummyStatus2 = ChangeRequestStatus("Validation","blablabla",false)
  val dummyStatusChange = ChangeRequestStatusChange(dummyStatus,AddChangeRequestStatusDiff,Seq())
  val dummyStatusChange2 = ChangeRequestStatusChange(dummyStatus2,AddChangeRequestStatusDiff,Seq())
  val dummyCR = ConfigurationChangeRequest(ChangeRequestId("1"),dummyStatusChange,Map())
  val dummyCR2 = ConfigurationChangeRequest(ChangeRequestId("2"),dummyStatusChange2,Map())

  private[this] val uuidGen = RudderConfig.stringUuidGenerator
  private[this] val changeRequestTableId = "ChangeRequestId"
  private[this] val CrId: Box[String] = {S.param("crId") }
  private[this] val Cr: Box[ChangeRequest] = Full(dummyCR)


  def dispatch = {
    case "header" => xml => CrId match { case eb:EmptyBox => <div id="content">

      <div> Error</div>
    </div>
      /* detail page */
    case Full(id) =>
      ("#backButton *" #> SHtml.ajaxButton("back",() => S.redirectTo("/secure/administration/changeRequests")) &
       "#CRName *" #> (if (id=="1") dummyCR.status.initialState.name else if (id=="2") dummyCR2.status.initialState.name else "not a CR") &
       "#CRStatus *" #> "status" &
       "#CRLastAction *" #> "Was sent to validation by machin on the 03/03/13") (xml)
    }

    case "details" => xml => logger.info(Cr)
    Cr match { case eb:EmptyBox => <div> Error {eb}</div>

     case Full(cr) => logger.info(cr)
       ("#detailsForm *" #> { (n:NodeSeq) => SHtml.ajaxForm(n) } andThen
        ClearClearable &
        "#CRName *" #> changeRequestName(cr).toForm_! &
        "#CRDescription *" #> changeRequestDescription(cr).toForm_!) (xml)
    }
    case "display" => xml => CrId match { case eb:EmptyBox => <div> Error</div>

     case Full(id) => <div>{SHtml.ajaxButton("back",() => S.redirectTo("/secure/administration/changeRequest"))}</div>
    }
  }

  private[this] def changeRequestName(Cr:ChangeRequest) =
    new WBTextField("Name", Cr.status.initialState.name) {
    override def setFilter = notNull _ :: trim _ :: Nil
    override def className = "twoCol"
    override def validations =
      valMinLen(3, "The name must have at least 3 characters") _ :: Nil
  }

  private[this] def changeRequestDescription(Cr:ChangeRequest) =
    new WBTextField("Description", Cr.status.initialState.description) {
      override def className = "twoCol"
      override def setFilter = notNull _ :: trim _ :: Nil
      override val maxLen = 255
      override def validations = Nil
  }

}
