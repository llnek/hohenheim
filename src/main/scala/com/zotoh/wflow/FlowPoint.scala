/*??
 * COPYRIGHT (C) 2012 CHERIMOIA LLC. ALL RIGHTS RESERVED.
 *
 * THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
 * MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE,
 * VERSION 2.0 (THE "LICENSE").
 *
 * THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
 * BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
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

import org.slf4j._
import java.util.concurrent.atomic.AtomicLong
import com.zotoh.wflow.core.Job

object FlowPoint {
  private val _sn= new AtomicLong(0)
  private def nextID() = _sn.incrementAndGet()
}
/**
 * @author kenl
 *
 */
abstract class FlowPoint protected[wflow](protected val _parent:Pipeline) extends Runnable {

  private val _log:Logger= LoggerFactory.getLogger(classOf[FlowPoint])
  def tlog() = _log

  import FlowPoint._

  private var _nextPtr:FlowPoint = null
  private var _defn:Activity = null
  private var _closure:Any= null
  private val _pid=nextID()

  /**
   * @param s
   * @param a
   */
  protected def this(s:FlowPoint, a:Activity) {
    this(s.flow)
    _nextPtr=s
    _defn=a
  }

  def eval(j:Job ):FlowPoint

  def id() = _pid

  def nextPoint() = _nextPtr
  def getDef() = _defn

  def attachClosureArg(c:Any) {
    _closure=c
  }

  def realize() : this.type = {
    getDef().realize(this)
    clsClosure()
    postRealize()
    this
  }

  protected def postRealize() {}

  protected def clsClosure() { _closure=null }
  def getClosureArg() = _closure
  def popClosureArg() = {
    try { val c=_closure; c }
    finally 
    { _closure=null }
  }

  def forceNext(n:FlowPoint) {
    _nextPtr=n
  }
  
  def flow() = _parent

  def rerun() {
    flow().container() match {
      case x:IContainer => x.core().reschedule(this)
      case _ =>
    }
  }

  def run() {
    var rc:FlowPoint = null
    var err:Activity =null
    val f= flow()

    f.container() match {
      case x:IContainer => x.core().dequeue(this)
    }
    try {
      rc=eval( f.job )
    } catch {
      case e:Throwable =>
        err= flow().onError(e, this)
    }

    if (err != null) { rc= err.reify(new NihilPoint(flow())) }
    if (rc==null) {
      tlog().debug("FlowPoint: rc==null => skip.")
      // indicate skip, happens with joins
    } else {
      runAfter(f,rc)
    }

  }

  private def runAfter(f:Pipeline, rc:FlowPoint) {
    val ct= f.container() match {
      case x:IContainer => x.core()
      case _ => null
    }
    val np= rc.nextPoint()
    rc match {
      case x:DelayPoint => ct.delay(np, x.delayMillis())
      case x:AsyncWaitPoint => ct.hold( np)
      case x:NihilPoint => f.stop()
      case _ => ct.run(rc)
    }
  }

//  override def finalize() {
//    super.finalize()
//    println("=========================> FlowPoint: " + getClass.getName + " finz'ed")
//  }

}

