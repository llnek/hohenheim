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

package com.zotoh.frwk.util

/**
 * @author kenl
 */
trait Schedulable {

  def dequeue(w:Runnable) : Unit
  def run(w:Runnable) : Unit
  def postpone(w:Runnable, delayMillis:Long) : Unit
  def hold(pid:Long, w:Runnable) : Unit
  def hold(w:Runnable) : Unit

  def dispose() : Unit

  def wakeup(w:Runnable) : Unit
  def wakeAndRun(pid:Long,w:Runnable) : Unit
  def reschedule(w:Runnable) : Unit

}


