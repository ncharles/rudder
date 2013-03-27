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

package com.normation.rudder.web.snippet

import scala.xml._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._
import Helpers._
import net.liftweb.http.js._
import JsCmds._
import JE._
import net.liftweb.util.Helpers._

import bootstrap.liftweb.RudderConfig

class WorkflowInformation extends DispatchSnippet with Loggable {
  private[this] val workflowService = RudderConfig.workflowService

  def dispatch = {
    case "pendingModification" => (pendingModifications & pendingDeployment)
  }

  def pendingModifications = {
    workflowService.getValidation match {
      case Full(seq) =>
        seq.size match {
          case 0 => ".pendingModifications" #> <span>There is no change request pending review</span>
          case 1 => ".pendingModifications" #> <span>There is <a href="/secure/utilities/changeRequests/Pending_validation">1</a> change request pending review</span>
          case size => ".pendingModifications" #>  <span>There are <a href="/secure/utilities/changeRequests/Pending_validation">{size}</a> change requests pending review</span>
        }
      case e:EmptyBox => ".pendingModifications" #>  <p class="error">Error when trying to fetch pending change requests.</p>
    }
  }

  def pendingDeployment = {
    workflowService.getDeployed match {
      case Full(seq) =>
        seq.size match {
          case 0 => ".pendingDeployment" #> <span>There is no change request pending deployment</span>
          case 1 => ".pendingDeployment" #> <span>There is <a href="/secure/utilities/changeRequests/Pending_deployment">1</a> change request pending deployment</span>
          case size => ".pendingDeployment" #>  <span>There are <a href="/secure/utilities/changeRequests/Pending_deployment">{size}</a> changes requests pending deployment</span>
        }
      case e:EmptyBox => ".pendingDeployment" #>  <p class="error">Error when trying to fetch pending change requests.</p>
    }
  }
}