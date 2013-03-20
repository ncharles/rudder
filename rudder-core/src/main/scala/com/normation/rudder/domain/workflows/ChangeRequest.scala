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

package com.normation.rudder.domain.workflows

import org.joda.time.DateTime
import com.normation.eventlog.EventActor
import com.normation.rudder.domain.policies._
import com.normation.rudder.domain.nodes._
import com.normation.eventlog.EventActor


/*
 * Event log on change request
 */
sealed trait ChangeRequestDiff

case class AddChangeRequestDiff(
    changeRequest: ChangeRequest
)extends ChangeRequestDiff

case class DeleteChangeRequestDiff(
    changeRequest: ChangeRequest
) extends ChangeRequestDiff

case class ModifyToChangeRequestDiff(
    changeRequest: ChangeRequest
) extends ChangeRequestDiff

case class RebaseChangeRequestDiff(
    changeRequest: ChangeRequest
) extends ChangeRequestDiff

case class ChangeRequestEventLog(
    actor       : EventActor
  , creationDate: DateTime
  , reason      : Option[String]
  , diff        : ChangeRequestDiff
)

/*
 * Question:
 * - do we need a ChangeRequestDraft object?
 *   With a different ID type, perhaps different
 *   draft type, etc. ?
 */


case class ChangeRequestId(value:String) {
  override def toString = value
}


//a container for changer request infos
final case class ChangeRequestInfo(
    name       : String
  , description: String
  // A marker to know if the ChangeRequest can be modified or not.
  // A change request can be modified only at certain point in
  // workflow, and for example we don't want someone to modify a
  // changeRequest just after the moment a reviewer validated it
  , readOnly   : Boolean
)

object ChangeRequest {

  def updateInfo[T <: ChangeRequest](cr:T, newInfo:ChangeRequestInfo): T = {
    cr match {
      case x:ConfigurationChangeRequest =>
        x.copy(info = newInfo).asInstanceOf[T]
      case x:RollbackChangeRequest =>
        x.copy(info = newInfo).asInstanceOf[T]
    }
  }

}

sealed trait ChangeRequest {

  def id: ChangeRequestId //modification Id ?

  def info: ChangeRequestInfo
}




////////////////////////////////////////
///// Some types of change request /////
////////////////////////////////////////

/**
 * A global configuration change request.
 * Can modify any number of Directives,
 * Rules and Group.
 */
case class ConfigurationChangeRequest(
    id         : ChangeRequestId //modification Id ?
  , info       : ChangeRequestInfo
  , directives : Map[DirectiveId, DirectiveChanges]
  , nodeGroups : Map[NodeGroupId, NodeGroupChanges]
  // ... TODO: complete for groups and rules
) extends ChangeRequest


case class RollbackChangeRequest(
    id         : ChangeRequestId //modification Id ?
  , info       : ChangeRequestInfo
  , rollback   : Null // TODO: rollback change request
) extends ChangeRequest


//////////////////////////////////
///// example for directives /////
//////////////////////////////////


sealed trait ChangeItem[DIFF] {
  def actor       : EventActor
  def creationDate: DateTime
  def reason      : Option[String]
  def diff        : DIFF
}

// A change for the given type is either the value
// of the item from the "current" environment or
// a diff.
// More preciselly, we have sequence of change related
// to an initial state (which can be empty).
sealed trait Change[T, DIFF, T_CHANGE <: ChangeItem[DIFF]] {
  // A change for the given type is either the value
  // of the item from the "current" environment or
  // a diff.
  // More preciselly, we have sequence of change related
  // to an initial state (which can be empty).

  //we have at least one such sequence
  def initialState: Option[T]
  def firstChange: T_CHANGE
  //non-empty list
  def nextChanges: Seq[T_CHANGE]
}


/**
 * A list of modification on a given item (directive, etc).
 * The parametrisation is as follow:
 * - T is the item type ;
 * - T_DIFF is the type of the eventLog with the diff for
 *   that object.
 * As the class is sealed, you can see implementation example
 * below.
 *
 * Younger generation are on head.
 */
sealed trait Changes[T, DIFF, T_CHANGE <: ChangeItem[DIFF]] {

  // A change for the given type is either the value
  // of the item from the "current" environment or
  // a diff.
  // More preciselly, we have sequence of change related
  // to an initial state (which can be empty).

  //we have at least one such sequence
  def changes: Change[T, DIFF, T_CHANGE]

  //older changes
  def changeHistory: Seq[Change[T, DIFF, T_CHANGE]]

  //TODO: we want to be able to compose diff so that
  //we are able to have a "final" view of the Diff.
  //for example: Add(title, desc), Mod(title2), Mod(description2)
  // => Add(title2, description2)

}

case class DirectiveChangeItem(
  //no ID: that object does not have any meaning outside
  // a change request
    actor       : EventActor
  , creationDate: DateTime
  , reason      : Option[String]
  , diff        : DirectiveDiff
) extends ChangeItem[DirectiveDiff]


case class DirectiveChange(
    val initialState: Option[Directive]
  , val firstChange: DirectiveChangeItem
  , val nextChanges: Seq[DirectiveChangeItem]
) extends Change[Directive, DirectiveDiff, DirectiveChangeItem]

case class DirectiveChanges(
    val changes: DirectiveChange
  , val changeHistory: Seq[DirectiveChange]
)extends Changes[Directive, DirectiveDiff, DirectiveChangeItem]


case class NodeGroupChangeItem(
  //no ID: that object does not have any meaning outside
  // a change request
    actor       : EventActor
  , creationDate: DateTime
  , reason      : Option[String]
  , diff        : NodeGroupDiff
) extends ChangeItem[NodeGroupDiff]

case class NodeGroupChange(
    val initialState: Option[NodeGroup]
  , val firstChange: NodeGroupChangeItem
  , val nextChanges: Seq[NodeGroupChangeItem]
) extends Change[NodeGroup, NodeGroupDiff, NodeGroupChangeItem]

case class NodeGroupChanges(
    val changes: NodeGroupChange
  , val changeHistory: Seq[NodeGroupChange]
)extends Changes[NodeGroup, NodeGroupDiff, NodeGroupChangeItem]

