package com.normation.rudder.reports.aggregation

import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import com.normation.rudder.domain.reports.bean.Reports
import com.normation.rudder.domain.policies._
import com.normation.inventory.domain.NodeId
import com.normation.rudder.domain.reports.bean.ResultSuccessReport
import org.joda.time.DateTime
import com.normation.rudder.domain.reports.bean.SuccessReportType
import java.sql.Timestamp


/**
 *
 */
@RunWith(classOf[JUnitRunner])
class AggregationTest extends Specification {
  import AggregationConstants._

  private implicit def str2DirectiveId(s:String) = DirectiveId(s)
  private implicit def str2RuleId(s:String) = RuleId(s)
  private implicit def str2nodeId(s:String) = NodeId(s)

  val now = DateTime.now()
  val reportToAdd : AggregatedReport = {
    val baseReport = new ResultSuccessReport(now, "cr", "policy", "one", 12, "component", "value",now, "message")
    AggregatedReport(baseReport,SuccessReportType,1,1)
  }

  val baseReport : AggregatedReport = AggregatedReport (
    "one"
  , "policy"
  , "cr"
  , 12
  , 12
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now.minusMinutes(10)
  , now.plusMinutes(10)
  , 0
  , 1
)

  val begining : AggregatedReport = AggregatedReport (
    "one"
  , "policy"
  , "cr"
  , 12
  , 12
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now.minusMinutes(10)
  , now
  , 0
  , 1
)

  val dummyAgregation = new AggregationService(null, null, null, null, null,0,0)

  val ending : AggregatedReport = AggregatedReport (
    "one"
  , "policy"
  , "cr"
  , 12
  , 12
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now
  , now.plusMinutes(10)
  , 0
  , 1
 )


  "Aggregation" should {

    val (begin,reports,end) = dummyAgregation.resolveconflictingReport(baseReport, reportToAdd)
    "be a success when we have a success" in {

      begin.map(AggregatedReportDummy(_)) === Some(AggregatedReportDummy(begining))
    }

  }

}

case class AggregatedReportDummy (
    nodeId: String,
    policyInstanceId: String,
    configurationRuleId: String,
    beginSerial: Int,
    endSerial: Int,
    component: String,
    keyValue: String,
    state: DBReportType,
    message: String,
    startTime: Timestamp,
    endTime: Timestamp,
    received: Int,
    expected: Int
)

object AggregatedReportDummy {
  def apply(report : (AggregatedReport,Boolean)) : AggregatedReportDummy = {
    val a = report._1
    AggregatedReportDummy(
        a.nodeId,
        a.directiveId,
        a.ruleId,
        a.beginSerial,
        a.endSerial,
        a.component,
        a.keyValue,
        a.state,
        a.message,
        a.startTime,
        a.endTime,
        a.received,
        a.expected
    )
  }
  def apply(a : AggregatedReport) : AggregatedReportDummy = {

    AggregatedReportDummy(
        a.nodeId,
        a.directiveId,
        a.ruleId,
        a.beginSerial,
        a.endSerial,
        a.component,
        a.keyValue,
        a.state,
        a.message,
        a.startTime,
        a.endTime,
        a.received,
        a.expected
    )
  }
}