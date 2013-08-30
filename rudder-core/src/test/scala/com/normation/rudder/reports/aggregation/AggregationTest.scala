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

  val report: Reports  = ResultSuccessReport(now, "cr", "policy", "one", 12, "component", "value",now, "message")

  val expectedReport = LinearisedExpectedReport(
    NodeId("one")       : NodeId
  , DirectiveId("policy")  : DirectiveId
  , RuleId("cr")       : RuleId
  , 12       : Int
  , "component"    : String
  , 1  : Int
  , "value"     : String
  , now minusMinutes(10)    : DateTime
  , now plusMinutes(10)      : DateTime
)
  val reportToAdd : AggregatedReport = AggregatedReport(report,SuccessReportType,1,1)


    val baseReport : AggregatedReport = AggregatedReport (
    NodeId("one")
  , RuleId("cr")
  , 12
  , 12
  , DirectiveId("policy")
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now.minusMinutes(5)
  , now.plusMinutes(5)
  , 0
  , 1
  , 42
)
  val beforebaseReport : AggregatedReport = AggregatedReport (
    NodeId("one")
  , RuleId("cr")
  , 12
  , 12
  , DirectiveId("policy")
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now.minusMinutes(15)
  , now.minusMinutes(5)
  , 1
  , 1
  , 41
)

  val beginning2 : AggregatedReport = AggregatedReport (
    NodeId("one")
  , RuleId("cr")
  , 12
  , 12
  , DirectiveId("policy")
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now.minusMinutes(15)
  , now
  , 1
  , 1
  , 41
)

  val begining : AggregatedReport = AggregatedReport (
    NodeId("one")
  , RuleId("cr")
  , 12
  , 12
  , DirectiveId("policy")
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now.minusMinutes(5)
  , now
  , 0
  , 1
  , 42
)

  val dummyAgregation = new AggregationService(null, null, null, null, null,0,0)

  val ending : AggregatedReport = AggregatedReport (
    NodeId("one")
  , RuleId("cr")
  , 12
  , 12
  , DirectiveId("policy")
  , "component"
  , "value"
  , SuccessReportType
  , ""
  , now
  , now.plusMinutes(5)
  , 0
  , 1
  , 0
 )


  "Aggregation" should {

    val (begin,reports,end) = dummyAgregation.splitConflict(baseReport, reportToAdd)
    "have a beginning" in {

      begin === Some(begining)
    }

    "have an ending" in   {
      end === Some(ending)
    }

    "have a report" in {
      reports === Seq(reportToAdd)
    }

  }

  "Reports" should {
    val result = dummyAgregation.createAggregatedReportsFromReports(Seq(report), Seq(expectedReport))
    result.head === reportToAdd
  }

  "Merge" should {
    val (toSave,toDelete) = dummyAgregation.mergeAggregatedReports(Seq(baseReport), Seq(reportToAdd))
    val (toSave2,toDelete2) = dummyAgregation.mergeAggregatedReports(Seq(beforebaseReport,baseReport), Seq(reportToAdd))
    println(toSave2)

    "save must have 3 elements to save " in {

      toSave must haveTheSameElementsAs (Seq(begining,reportToAdd, ending))
    }

    "Delete must be Empty" in {
      toDelete must beEmpty
    }

        "save2 must have 3 elements to save " in {

      toSave2 must haveTheSameElementsAs (Seq( ending,beginning2))
    }
  }

    "MergeOne" should {
    val (toSave,toDelete,_) = dummyAgregation.mergeOneAggregatedReport(Seq(baseReport), reportToAdd)

    "have 3 elements to save " in {

      toSave.size === 3
    }

    "Delete must be Empty" in {
      toDelete.isEmpty
    }


  }


}
/*
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
}*/