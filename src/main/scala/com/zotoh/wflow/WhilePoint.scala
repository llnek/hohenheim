/*??
*
* Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
*
* This library is distributed in the hope that it will be useful
* but without any warranty; without even the implied warranty of
* merchantability or fitness for a particular purpose.
*
* The use and distribution terms for this software are covered by the
* Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
* which can be found in the file epl-v10.html at the root of this distribution.
*
* By using this software in any fashion, you are agreeing to be bound by
* the terms of this license.
* You must not remove this notice, or any other, from this software.
*
 ??*/


package com.zotoh.wflow

import com.zotoh.wflow.core.Scope

/**
 * @author kenl
 *
 */
class WhilePoint(s:FlowPoint , a:While) extends ConditionalPoint(s,a) {

  private var _body:FlowPoint =null

  def eval(j:Scope ) = {
    var rc:FlowPoint = this
    val c= getClosureArg()

    if ( ! test(j)) {
      //tlog().debug("WhilePoint: test-condition == false")
      rc= nextPoint()
      if (rc != null) { rc.attachClosureArg(c) }
      realize()
    } else {
      //tlog().debug("WhilePoint: looping - eval body")
      _body.attachClosureArg(c)
      val f= _body.eval(j)
      f match {
        case x:AsyncWaitPoint =>
          x.forceNext(rc)
          rc=x
        case x:DelayPoint =>
          x.forceNext(rc)
          rc=x
        case s:FlowPoint if ! (s eq this) => _body = s
        case _ => /* never */
      }

    }

    rc
  }

  def withBody(body:FlowPoint ): this.type = {
    _body=body
    this
  }

}
