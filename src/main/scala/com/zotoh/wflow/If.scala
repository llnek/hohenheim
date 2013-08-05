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

/**
 * @author kenl
 *
 */
class If(
  private val _thenCode:Activity,
  private val _elseCode:Activity,
  expr:BoolExpr) extends Conditional(expr) {

  def this(thenCode:Activity,expr:BoolExpr ) {
    this(thenCode, null, expr)
  }

  def reifyPoint(cur:FlowPoint ) = new IfPoint(cur,this)

  def realize(fp:FlowPoint ) {
    val s=fp.asInstanceOf[IfPoint]
    val np= s.nextPoint()
    s.withElse( if (_elseCode ==null) np else _elseCode.reify(np) )
    s.withThen( _thenCode.reify(np))
    s.withTest( expr())
  }

}
