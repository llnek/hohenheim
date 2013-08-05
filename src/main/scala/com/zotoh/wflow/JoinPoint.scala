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

import java.util.concurrent.atomic.AtomicInteger

/**
 * @author kenl
 *
 */
abstract class JoinPoint protected[wflow](s:FlowPoint, a:Join) extends FlowPoint(s,a) {

  protected val _cntr=new AtomicInteger(0)
  protected var _body:FlowPoint= null
  private var _branches= 0

  def withBody(body:FlowPoint ): this.type = {
    _body=body
    this
  }

  def withBranches(n:Int): this.type = {
    _branches=n
    this
  }

  def size() = _branches

  override def postRealize() {
    _cntr.set(0)
  }

}
