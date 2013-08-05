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
 * @author kenl
 *
 */
class PTaskPoint(s:FlowPoint, a:PTask ) extends FlowPoint(s,a) {

  private var _work:Work = null

  def withWork(w:Work): this.type = {
    _work=w
    this
  }

  def eval(j:Job ) = {
    //tlog.debug("PTaskPoint: {} about to perform work.", this.id )
    val a= _work.perform(this, j, popClosureArg())
    var rc= nextPoint()

    a match {
      case n:Nihil =>
        rc = new NihilPoint(flow() )
      case x:Activity =>
        rc = x.reify(rc)
      case _  =>
        if (rc != null) {
          rc.attachClosureArg(a)
        }
    }

    rc
  }


}

