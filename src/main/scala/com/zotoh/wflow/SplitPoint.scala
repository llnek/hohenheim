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
class SplitPoint(s:FlowPoint, a:Split) extends CompositePoint(s,a) {

  private var _fallThru=false

  def eval(j:Job ) = {
    val core = flow().container() match {
      case x:IContainer => x.core()
      case _ => null
    }
    val c= getClosureArg()
    var rc:FlowPoint =null

    while ( !_inner.isEmpty() ) {
      rc = _inner.next()
      rc.attachClosureArg(c)
      core.run(rc)
    }

    realize()

    // should we also pass the closure to the next step ? not for now
    if (_fallThru) nextPoint() else null
  }

  def withBranches(w:Iter): this.type = {
    _inner=w
    this
  }

  def fallThrough(): this.type = {
    _fallThru=true
    this
  }

}

