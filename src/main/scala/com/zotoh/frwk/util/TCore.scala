/*??
 * COPYRIGHT (C) 2012-2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

package com.zotoh.frwk.util

import scala.math._
import java.util.concurrent.{LinkedBlockingQueue,RejectedExecutionHandler}
import java.util.concurrent.{ThreadPoolExecutor,TimeUnit}
import org.slf4j._
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService

/**
 * @author kenl
 */
object TCore {
  private var _log:Logger = LoggerFactory.getLogger(classOf[TCore])
}

/**
 * @author kenl
 */
class TCore(private val _id:String, tds:Int) extends RejectedExecutionHandler {

  //private val serialVersionUID = 404521678153694367L

  private var _scd:ExecutorService= null
  def tlog() = TCore._log
  private val _tds = max(4, tds)

  def start() { activate }

  def stop() {}

  def dispose() {
    //_scd.shutdownNow()
    _scd.shutdown()
  }

  def schedule(work:Runnable) {
    _scd.execute(work)
  }

  def rejectedExecution(r:Runnable, x:ThreadPoolExecutor) {
    //TODO: deal with too much work for the core...
    tlog.error("\"{}\" rejected work - threads/queue are max'ed out" , _id);
  }
  
  private def activate() {
//    _scd= Executors.newCachedThreadPool( new TFac(_id) )
    _scd= new ThreadPoolExecutor( _tds, _tds, 5000,
        TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable](),
        new TFac(_id) , this )
    tlog.info("Core \"{}\" activated with threads = {}" , _id , "" + _tds, "")
  }

}

