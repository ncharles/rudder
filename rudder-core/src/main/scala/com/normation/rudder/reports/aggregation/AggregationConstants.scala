package com.normation.rudder.reports.aggregation

import org.joda.time.Duration
import com.normation.rudder.domain.reports.bean.Reports._
import org.joda.time.format.DateTimeFormat

object AggregationConstants {

  val NAME_UPDATE_KEY = "nameUpdates"
  val AGREGATION_UPDATE_KEY = "agregationUpdates"


  // a no answer time is 10 minutes
  val NoAnswerTime = new Duration(10*60*1000)


  val NOANSWERKEY = "NoAnswer"
  val ERRORKEY = RESULT_ERROR

  //the date-time format for reports
  val DATETIME_FORMAT = "yyyy-MM-dd"
  val DATETIME_PARSER = DateTimeFormat.forPattern(DATETIME_FORMAT)
}