/*??
 * COPYRIGHT (C) 2012 CHERIMOIA LLC. ALL RIGHTS RESERVED.
 *
 * THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
 * MODIFY IT UNDER THE TERMS OF THE APACHE LICENSimport static com.zotoh.core.util.LoggerFactory.getLogger;

E,
 * VERSION 2.0 (THE "LICENSE").
 *
 * THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
 * BUT WITHOUT ANY WAimport com.zotoh.core.util.Logger;
import com.zotoh.maedr.core.DeviceFactory;
RRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
 * AND LIMITATIONS UNDER THE LICENSE.
 *
 * You should have received a copy of the Apache License
 * along with this distribution; if not, you may obtain a copy of the
 * License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 ??*/

package com.zotoh.wflow

import com.zotoh.frwk.util.CoreUtils._
import org.slf4j._
import com.zotoh.frwk.util.Schedulable
import java.util.concurrent.atomic.AtomicLong
import com.zotoh.wflow.core.Job


trait PipelineDelegate {

  def getStartActivity() : Activity
  def onStop(job:Job) : Unit
  def onError(err:Throwable, curPt:FlowPoint) : Activity

}

trait IContainer {
  def core() : Schedulable
}

object Pipeline {
  private val _sn= new AtomicLong(0L)
  private def nextId() = _sn.incrementAndGet()
}

/**
 * @author kenl
 *
 */
class Pipeline (private val _theJob:Job, private val _delegateClass:String) {

  private val _log:Logger= LoggerFactory.getLogger(classOf[Pipeline] )
  def tlog() = _log

  import Pipeline._

  private val _delegate = Thread.currentThread().getContextClassLoader().loadClass(_delegateClass).getDeclaredConstructor().newInstance() match {
    case x:PipelineDelegate => x
    case _ =>
    throw new ClassCastException("Class " + _delegateClass + " must implement PipelineDelegate.")
  }

  private val _pid= nextId()
  private var _active=false

  tlog().debug("{}: {} => pid : {}" , "Pipeline", getClass().getName() , asJObj(_pid))
  require(_theJob != null, "Job is null.")

  def core() : Schedulable = container() match {
    case x:IContainer => x.core()
    case _ => null
  }

  def container() = _theJob.container()
  def isActive() = _active
  def job() = _theJob
  def getPID() = _pid

  protected def onEnd() {
    _delegate.onStop(_theJob)
  }

  protected[wflow] def onError(e:Throwable, cur:FlowPoint) : Activity = {
    tlog().error("", e)
    _delegate.onError(e,cur) match {
      case x:Activity => x
      case _ => new Nihil()
    }
  }

  protected def onStart():Activity = {
    _delegate.getStartActivity() match {
      case x:Activity => x
      case _ => new Nihil()
    }
  }

  def start() {
    tlog().debug("{}: {} => pid : {} => starting" , "Pipeline", _delegateClass , asJObj(_pid))

    val f1= onStart().reify( new NihilPoint(this))
    _active=true
    try {
      core().run(f1)
    } catch {
      case e:Throwable =>
        tlog().error("", e)
        stop()
    }
  }

  def stop() {
    try {
      onEnd()
    } catch {
      case e:Throwable =>
      tlog().error("",e)
    }

    tlog().debug("{}: {} => pid : {} => end" , "Pipeline", _delegateClass , asJObj(_pid))
  }

  override def toString() = {
    _delegateClass + "(" + _pid + ")"
  }

//  override def finalize() {
//    super.finalize()
//    println("=========================> Pipeline: " + getClass.getName + " finz'ed")
//  }
  
}
