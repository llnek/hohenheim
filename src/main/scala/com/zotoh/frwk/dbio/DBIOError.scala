/*??
 * COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

package com.zotoh.frwk.dbio

import java.sql.SQLException

@SerialVersionUID(113241635256073760L)
class DBIOError(msg:String, t:Throwable) extends SQLException(msg,t) {

  /**
   * @param msg
   */
  def this(msg:String) {
    this(msg, null)
  }

  /**
   * @param t
   */
  def this(t:Throwable) {
    this("",t)
  }

  def this() {
    this("")
  }

}

@SerialVersionUID(143241635256073760L)
class OptLockError(opcode:String, table:String, rowID:Long) extends 
SQLException(
"Possible Optimistic lock failure for table: " +
               table +
               ", rowid= " + rowID  + " during " + opcode) {
}




