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
class Switch(private var _expr:SwitchChoiceExpr ) extends Activity {

  private val _choices= mutable.HashMap[Any,Activity]()
  private var _def:Activity =null

  def withChoice(matcher:Any, body:Activity ): this.type = {
    _choices.put(matcher, body)
    this
  }

  def withDef(a:Activity ): this.type = {
    _def=a
    this
  }

  def reifyPoint(cur:FlowPoint ) = new SwitchPoint(cur, this)

  def realize(fp:FlowPoint ) {
    val t= mutable.HashMap[Any,FlowPoint]()
    val p=fp.asInstanceOf[SwitchPoint]
    val nxt= p.nextPoint()

    _choices.foreach { (en) =>
      t.put(en._1,
              en._2.reify(nxt))
    }

    p.withChoices(t.toMap)
    if (_def != null) {
      p.withDef( _def.reify(nxt))
    }
    p.withExpr(_expr)
  }








}
