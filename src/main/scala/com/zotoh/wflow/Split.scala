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

import com.zotoh.wlow.Iter
import com.zotoh.wflow.core.Job


/**
 * @author kenl
 *
 */
class Split(protected var _theJoin:Join) extends Composite {

  def this() {
    this(null)
  }

  def addSplit(a:Activity ): this.type = {
    add(a)
    this
  }

  def reifyPoint(cur:FlowPoint ) = new SplitPoint(cur, this)

  override def realize(fp:FlowPoint ) {
    val p=fp.asInstanceOf[SplitPoint]
    if ( _theJoin != null) {
      _theJoin.withBranches( size() )
    } else {
      _theJoin= new NullJoin()
    }

    val s = _theJoin.reify(p.nextPoint() )
    // note: get all *children* to come back to the join
    p.withBranches( new Iter(s, listChildren() ) )

    _theJoin match {
      case n:NullJoin =>
        p.fallThrough()
      case _ =>
    }
  }


}

/**
 * @author kenl
 *
 */
class NullJoin extends Join(null) {

  def reifyPoint(cur:FlowPoint ) = new NullJoinPoint(cur, this)

  def realize(cur:FlowPoint ) {}

}

/**
 * @author kenl
 *
 */
class NullJoinPoint(s:FlowPoint, a:Join ) extends JoinPoint(s,a) {

  def eval(j:Job ) = null

}
