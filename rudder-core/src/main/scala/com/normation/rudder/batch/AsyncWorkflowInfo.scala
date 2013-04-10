package com.normation.rudder.batch

import net.liftweb.actor.LiftActor
import net.liftweb.common.Loggable
import net.liftweb.http.ListenerManager
import com.normation.rudder.services.workflows.WorkflowUpdate

class AsyncWorkflowInfo extends LiftActor with Loggable with ListenerManager {


  def createUpdate = WorkflowUpdate

  override protected def lowPriority = {
    case WorkflowUpdate => updateListeners
  }
}