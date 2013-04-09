package com.normation.rudder.services.eventlog

import com.normation.rudder.domain.workflows._
import com.normation.rudder.domain.eventlog._
import com.normation.eventlog._
import net.liftweb.common._
import com.normation.rudder.repository.EventLogRepository
import com.normation.utils.StringUuidGenerator


trait WorkflowEventLogService {

  def saveEventLog(stepChange:WorkflowStepChange, actor:EventActor, reason:Option[String]) : Box[EventLog]

  def getChangeRequestHistory(id: ChangeRequestId) : Box[Seq[WorkflowStepChanged]]

  def getLastLog(id:ChangeRequestId) : Box[Option[WorkflowStepChanged]]
}

class WorkflowEventLogServiceImpl (
    eventLogRepository : EventLogRepository
  , uuidGen            : StringUuidGenerator
) extends WorkflowEventLogService with Loggable {
  def saveEventLog(stepChange:WorkflowStepChange, actor:EventActor, reason:Option[String]) : Box[EventLog] = {
    val modId = ModificationId(uuidGen.newUuid)
    eventLogRepository.saveWorkflowStep(modId, actor, stepChange, reason)
  }

  def getChangeRequestHistory(id: ChangeRequestId) : Box[Seq[WorkflowStepChanged]] = {
    eventLogRepository.getEventLogByChangeRequest(id,"/entry/workflowStep/changeRequestId/text()").map(_.collect{case w:WorkflowStepChanged => w})
  }

  def getLastLog(id:ChangeRequestId) : Box[Option[WorkflowStepChanged]] = {
    eventLogRepository.getEventLogByChangeRequest(id,"/entry/workflowStep/changeRequestId/text()",Some(1),Some("creationDate desc")).map(_.collect{case w:WorkflowStepChanged => w}.headOption)
  }
  }
