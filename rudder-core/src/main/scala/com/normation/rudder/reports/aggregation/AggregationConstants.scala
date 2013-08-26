package com.normation.rudder.reports.aggregation

import org.joda.time.Duration
import com.normation.rudder.domain.reports.bean.Reports._
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.sql.Timestamp

object AggregationConstants {

  val AGGREGATION_STATUS = "aggregationStatus"

  // a no answer time is 10 minutes
  val NoAnswerTime = new Duration(10*60*1000)


  val NOANSWERKEY = "NoAnswer"
  val ERRORKEY = RESULT_ERROR

  //the date-time format for reports
  val DATETIME_FORMAT = "yyyy-MM-dd"
  val DATETIME_PARSER = DateTimeFormat.forPattern(DATETIME_FORMAT)

  implicit def toTimeStamp(d:DateTime) : Timestamp = new Timestamp(d.getMillis)
}