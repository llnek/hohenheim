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

package com.zotoh.hohenheim.io

import java.io.{File,InputStream}
import java.util.{Properties=>JPS}
import javax.servlet.ServletContext
import javax.servlet.ServletContextEvent
import javax.servlet.ServletContextListener
import org.slf4j._
import org.eclipse.jetty.webapp.WebAppContext


/**
 * @author kenl
 */
class WEBContextListener extends ServletContextListener {

  private var _log=LoggerFactory.getLogger(classOf[WEBContextListener])
  private var _src:Any = null
  def tlog() = _log

  override def contextInitialized(evt:ServletContextEvent) {

    tlog().debug("WEBContextListener: contextInitialized()")

    val x= evt.getServletContext()
    var ctx=""
    val m= x.getMajorVersion()
    val n= x.getMinorVersion()

    if (m > 2 || ( m==2 && n > 4)) {
      ctx= x.getContextPath()
    }

    try {
      inizAsJ2EE(x, ctx)
    } catch {
      case e:Throwable => 
        tlog().error("", e)
        throw e
    }
  }

  override def contextDestroyed(e:ServletContextEvent) {
    tlog().debug("WEBContextListener: contextDestroyed()")
    if (_src!=null) {
      //_src.container.dispose()
    }
    _src=null
  }

  private def inizAsJ2EE(ctx:ServletContext, ctxPath:String) {
  }

}
