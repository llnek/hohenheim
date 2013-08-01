/*??
 * COPYRIGHT (C) 2012-2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

/**
 * The default thread factory - from javasoft code.  The reason why
 * we cloned this is so that we can control how the thread-id is
 * traced out. (we want some meaninful thread name).
 *
 * @author kenl
 */
class TFac(private val _pfx:String) extends ThreadFactory {
  private val _cl = Thread.currentThread().getContextClassLoader
  private val _seq= new AtomicInteger(0)

  private val _group = System.getSecurityManager() match {
    case sm:SecurityManager => sm.getThreadGroup()
    case _ => Thread.currentThread().getThreadGroup
  }

  def newThread(r:Runnable) = {
    val t = new Thread(_group, r, mkTname(), 0)
    t.setPriority(Thread.NORM_PRIORITY)
    t.setDaemon(false)
    t.setContextClassLoader(_cl)
    t
  }

  private def mkTname() = { _pfx + _seq.incrementAndGet }

}
