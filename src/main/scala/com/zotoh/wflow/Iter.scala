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

import scala.collection.mutable

/**
 * @author kenl
 *
 */
class Iter(private val _outer:FlowPoint) {

  private val _acts= new mutable.ArrayBuffer[Activity]()

  def this(c:FlowPoint, a:Seq[Activity]) {
    this(c)
    a.foreach { (aa) => _acts += aa }
  }

  def isEmpty() = _acts.size == 0

  def next() = if (_acts.size > 0) _acts.remove(0).reify(_outer) else null

  def size() = _acts.size

}
