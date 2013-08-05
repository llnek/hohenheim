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

import com.zotoh.wflow.core.Job

/**
 * A For is treated sort of like a while with the test-condition being (i &lt; upperlimit).
 * 
 * @author kenl
 *
 */
class For(private val _loopCntr:ForLoopCountExpr,body:Activity) extends While(body) {

  override def reifyPoint(cur:FlowPoint) = new ForPoint(cur,this)

  override def realize(fp:FlowPoint) {
    val p=fp.asInstanceOf[ForPoint]
    super.realize(fp)
    p.withTest(new ForLoopExpr(p, _loopCntr))
  }

}

/**
 * @author kenl
 *
 */
class ForLoopExpr(
  private val _point:FlowPoint , 
  private val _cnt:ForLoopCountExpr ) extends BoolExpr {

  private var _started=false
  private var _loop=0

  def evaluate(j:Job ) = {
    try {
      if (!_started) {
        _loop=_cnt.getCount(j)
        _started=true
      }
      _point.tlog().debug("ForLoopExpr: loop {}", _loop)
      _loop > 0

    } finally {
      _loop -= 1
    }
  }

}


