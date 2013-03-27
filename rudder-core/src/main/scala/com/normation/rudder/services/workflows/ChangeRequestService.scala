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

package com.normation.rudder.services.workflows

import org.joda.time.DateTime
import com.normation.cfclerk.domain.TechniqueName
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.nodes.NodeGroup
import com.normation.rudder.domain.policies.{ AddDirectiveDiff, Directive, ModifyToDirectiveDiff }
import com.normation.rudder.domain.workflows.{ ConfigurationChangeRequest, DirectiveChange, DirectiveChangeItem, DirectiveChanges }
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.utils.StringUuidGenerator
import com.normation.rudder.domain.workflows.ChangeRequestInfo
import com.normation.cfclerk.domain.SectionSpec
import net.liftweb.common.Loggable
import com.normation.rudder.domain.policies.DeleteDirectiveDiff
import com.normation.rudder.domain.policies.ChangeRequestDirectiveDiff
import com.normation.rudder.domain.policies.DirectiveId



/**
 * A service that handle all the logic about how
 * a change request is created / updated from basic parts.
 */
trait ChangeRequestService {

  def createChangeRequestFromDirective(
      changeRequestName: String
    , changeRequestDesc: String
    , readOnly         : Boolean
    , techniqueName    : TechniqueName
    , rootSection      : SectionSpec
    , directiveId      : DirectiveId
    , originalDirective: Option[Directive]
    , diff             : ChangeRequestDirectiveDiff
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest

  def createChangeRequestFromNodeGroup(
      changeRequestName: String
    , changeRequestDesc: String
    , nodeGroup        : NodeGroup
    , originalNodeGroup: Option[NodeGroup]
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest

  def updateChangeRequestWithDirective(
      changeRequest: ConfigurationChangeRequest
    , techniqueName: TechniqueName
    , rootSection      : SectionSpec
    , directive    : Directive
    , originalDirective: Option[Directive]
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest

  def updateChangeRequestWithNodeGroup(
      changeRequest    : ConfigurationChangeRequest
    , nodeGroup        : NodeGroup
    , originalNodeGroup: Option[NodeGroup]
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest


}


class ChangeRequestServiceImpl(
    uuidGen: StringUuidGenerator
) extends ChangeRequestService with Loggable {

  private[this] var nextId:Int = 1042

  private[this] def getNextId = {
    val id = nextId
    nextId = nextId + 1
    id.toString
  }

  def createChangeRequestFromDirective(
      changeRequestName: String
    , changeRequestDesc: String
    , readOnly         : Boolean
    , techniqueName    : TechniqueName
    , rootSection      : SectionSpec
    , directiveId      : DirectiveId
    , originalDirective: Option[Directive]
    , diff             : ChangeRequestDirectiveDiff
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest = {

    val initialState = originalDirective match {
      case None =>  None
      case Some(x) => Some((techniqueName, x, rootSection))
    }
    val change = DirectiveChange(
                     initialState = initialState
                   , firstChange = DirectiveChangeItem(actor, DateTime.now, reason, diff)
                   , Seq()
                 )
    logger.debug(change)
    ConfigurationChangeRequest(
        ChangeRequestId(getNextId)
      , ChangeRequestInfo(
            changeRequestName
          , changeRequestDesc
          , readOnly
        )
      , Map(directiveId -> DirectiveChanges(change, Seq()))
      , Map()
    )
  }

  def createChangeRequestFromNodeGroup(
      changeRequestName: String
    , changeRequestDesc: String
    , nodeGroup        : NodeGroup
    , originalNodeGroup: Option[NodeGroup]
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest = {
    logger.info("update")
    ???
  }

  def updateChangeRequestWithDirective(
      changeRequest: ConfigurationChangeRequest
    , techniqueName: TechniqueName
    , rootSection      : SectionSpec
    , directive    : Directive
    , originalDirective: Option[Directive]
    , actor            : EventActor
    , reason           : Option[String]
  ) = {
    logger.info("update")
    ???
  }

  def updateChangeRequestWithNodeGroup(
      changeRequest    : ConfigurationChangeRequest
    , nodeGroup        : NodeGroup
    , originalNodeGroup: Option[NodeGroup]
    , actor            : EventActor
    , reason           : Option[String]
  ) : ConfigurationChangeRequest = {
    logger.info("update")
    ???
  }

}