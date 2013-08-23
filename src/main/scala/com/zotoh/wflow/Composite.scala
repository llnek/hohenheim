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

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * @author kenl
 *
 */
abstract class Composite extends Activity {

  private val _children= mutable.ArrayBuffer[Activity]()

  def size() = _children.size()

  protected def add(a:Activity ) {
    _children += a
    onAdd(a)
  }

  protected def onAdd(a:Activity ) {}

  def listChildren() = _children.toSeq

  def realize(fp:FlowPoint) {
    val p=fp.asInstanceOf[CompositePoint]
    p.reifyInner( listChildren )
  }

}


abstract class CompositePoint(cur:FlowPoint,a:Activity) extends FlowPoint(cur,a) {
  protected var _inner:Iter = null
  def reifyInner(children:Seq[Activity]) {
    _inner=new Iter(this,children)
  }
  def inner() = _inner
}
