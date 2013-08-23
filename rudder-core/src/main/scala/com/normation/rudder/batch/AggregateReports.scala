package com.normation.rudder.batch

import net.liftweb.common.Box
import net.liftweb.util.Helpers.tryo
import com.normation.rudder.reports.aggregation.AggregationService


/**
 * That batch scheduler periodically aggregate reports.
 *
 * The actual copy is delegated to
 * <code>com.normation.rudder.reports.aggregation.AggregationService.aggregateReports</code> method.
 *
 */
class AggregateReports(
    aggregationService: AggregationService
  , override val updateInterval   : Int // in minutes
) extends AbstractScheduler {

  override type T = Unit
  override val executeTask: () => Box[Unit]  = () => tryo{aggregationService.newAggregation}
  override lazy val displayName : String = "Aggregate Report"
  override lazy val propertyName : String = "rudder.aggregateReport.updateInterval"

}