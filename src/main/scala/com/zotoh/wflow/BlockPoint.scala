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
class BlockPoint protected[wflow](s:FlowPoint, a:Block) extends CompositePoint(s,a) {

  def eval(j:Job ) = {
    val c= getClosureArg()   // data pass back from previous async call?
    var rc:FlowPoint =null

    if ( ! _inner.isEmpty()) {
      //tlog.debug("BlockPoint: {} element(s.)",  _inner.size )
      val n=_inner.next()
      n.attachClosureArg(c)
      rc = n.eval(j)
    } else {
      //tlog.debug("BlockPoint: no more elements.")
      rc=nextPoint()
      if (rc != null) {  rc.attachClosureArg(c) }
      realize()
    }

    rc
  }

}
