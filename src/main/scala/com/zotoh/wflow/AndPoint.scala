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

package com.zotoh.wflow

import com.zotoh.frwk.util.CoreUtils._
import com.zotoh.wflow.core.Job

/**
 * A "AND" join enforces that all bound activities must return before Join continues.
 *
 * @author kenl
 *
 */
class AndPoint protected[wflow](s:FlowPoint,a:And) extends JoinPoint(s,a) {

  def eval(j:Job) = {
    val nv= _cntr.incrementAndGet()
    val c= getClosureArg()
    var rc:FlowPoint= null

    tlog().debug("AndPoint: size={}, cntr={}, join={}",
      asJObj( size()),  asJObj(nv), asJObj(this))

    // all branches have returned, proceed...
    if (nv == size() ) {
      rc= if (_body == null) nextPoint() else _body
      if (rc != null) { rc.attachClosureArg(c) }
      realize()
    }

    rc
  }

}
