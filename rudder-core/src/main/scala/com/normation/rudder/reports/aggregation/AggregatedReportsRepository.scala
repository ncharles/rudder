/*
*************************************************************************************
* Copyright 2013 Normation SAS
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

package com.normation.rudder.reports.aggregation

import com.normation.rudder.domain.policies.RuleId
import com.normation.inventory.domain.NodeId
import org.joda.time.DateTime
import net.liftweb.common.Box

trait AggregatedReportsRepository {

  /**
   * Returns all the advanced reports for a given rule and node
   * Returns really a Seq, and not a stream
   */
  def getAggregatedReports(
      	ruleId : RuleId
      , serial : Int
      , nodeId : NodeId
  ) : Box[Seq[AggregatedReport]]

  /**
   * Get the last aggregated report for a given Node/Rule
   */
  def getLatestAggregatedReports(
      ruleId : RuleId
    , nodeId : NodeId
  ) : Box[Seq[AggregatedReport]]

  /**
   * Save the reports :
   * create them if they don't exist
   * update them otherwise
   */
  def saveAggregatedReports(reports : Seq[AggregatedReport]) : Unit

  def updateAggregatedReports(reports : Seq[AggregatedReport]) : Seq[Int]

  def createAggregatedReports(reports : Seq[AggregatedReport]) : Seq[AggregatedReport]

   /**
    *  Retrieve all the reports for a Rule between two dates
    *  The Rule may not exists, in that case no report will be returned.
    *  Some reports may be missing, for example if the Rule was created
    *  after beginDate.
    *  The reports are clipped to the begin and end date : if a reports starts before,
    *  the date will be replaced by the begin date
    *  begineDate date is included,
    *  endDate date is excluded.
    *
    */
  def getAggregatedReportsByDate(
      ruleId    : RuleId
    , beginDate : DateTime
    , endDate   : DateTime
  ) : Box[Seq[AggregatedReport]]

  /**
   * Retrieve all reports for a Node between to date
   *  The reports are clipped to the begin and end date : if a reports starts before,
    *  the date will be replaced by the begin date
    *  Do not fetch the system rules
    *
   */

  def getAggregatedReportsByNodeAndDate(
      nodeId    : NodeId
    , beginDate : DateTime
    , endDate   : DateTime
  ) : Box[Seq[AggregatedReport]]


  /**
   * Return true if the rule is applied to other reports passed on argument
   * It means that it returns true if the rule is not applied exactly and only to the group of node
   */
  def isNotAppliedOnlyToThisGroup(
      ruleId    : RuleId
    , reports   : Seq[AggregatedReport]
    , beginDate : DateTime
    , endDate   : DateTime
  ) : Box[Boolean]

    def deleteAggregatedReports(reports : Seq[AggregatedReport]) : Seq[Int]
}
