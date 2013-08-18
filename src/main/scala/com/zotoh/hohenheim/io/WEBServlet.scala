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

package com.zotoh.hohenheim.io

import scala.collection.mutable
import java.io.IOException
import org.slf4j._
import javax.servlet.ServletConfig
import javax.servlet.ServletContext
import javax.servlet.ServletException
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.continuation.Continuation
import org.eclipse.jetty.continuation.ContinuationSupport
import com.zotoh.frwk.io.XData

/**
 * @author kenl
 *
 */
@SerialVersionUID(-3862652820921092885L) class WEBServlet(private var _src:ServletEmitter ) extends HttpServlet
with Serializable {

  private val _log = LoggerFactory.getLogger(classOf[WEBServlet])
  def tlog() = _log

  def this() {
    this(null)
  }
  
  override def destroy() {
    tlog().debug("WEBServlet: destroy()")
  }

  override def service(request:ServletRequest, response:ServletResponse) {
    val rsp= response.asInstanceOf[HttpServletResponse]
    val req= request.asInstanceOf[HttpServletRequest]

    tlog().debug("{}\n{}\n{}",
    "********************************************************************",
      req.getRequestURL(),
      "********************************************************************")

    val c = ContinuationSupport.getContinuation(req)
    if (c.isInitial ) try {
      _src.doService(req,rsp)
    } catch {
      case e:Throwable => tlog().error("",e)
    }
  }

  override def init(config:ServletConfig) {
    super.init(config)

    val ctx= config.getServletContext()
    ctx.getAttribute( "czchhhiojetty") match {
      case x:ServletEmitter => 
        _src = x
      case _ =>
    }

    try {
      tlog().debug("{}\n{}{}\n{}\n{}{}",
        "********************************************************************",
        "Servlet Container: ",
        ctx.getServerInfo(),
        "********************************************************************",
        "Servlet:iniz() - servlet:" ,
        getServletName())
    } catch {
      case e:Throwable =>
    }

  }

}
