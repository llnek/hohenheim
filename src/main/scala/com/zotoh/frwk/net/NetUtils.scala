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

package com.zotoh.frwk.net

import org.jboss.netty.channel.ChannelFutureListener
import org.jboss.netty.channel.{ChannelFuture,Channel}
import org.jboss.netty.buffer.ChannelBuffer
import java.io.{OutputStream}
import org.slf4j._
import com.zotoh.frwk.io.{IOUtils,XData}

/**
 * @author kenl
 */
object NetUtils {
  
  private val _log=LoggerFactory.getLogger(classOf[NetUtils])
  
  def dbgNettyDone(msg:String) = new ChannelFutureListener() {
      def operationComplete(fff:ChannelFuture) {  
          _log.debug("netty-op-complete: {}", msg)        
      }
  }

  def sockItDown(cbuf:ChannelBuffer, out:OutputStream, lastSum:Long ) = {
    val cnt= if (cbuf==null) 0 else cbuf.readableBytes()
    if (cnt > 0) {
      val bits= new Array[Byte](4096)
      var total=cnt
      while (total > 0) {
        val len = Math.min(4096, total)
        cbuf.readBytes(bits, 0, len)
        out.write(bits, 0, len)
        total -= len
      }
      out.flush()
    }
    lastSum + cnt
  }

  def swapFileBacked(x:XData, out:OutputStream, lastSum:Long) = {
    if (lastSum > IOUtils.streamLimit) {
      val (f,os) = IOUtils.newTempFile(true)
      x.resetContent(f)
      os
    } else {
      out
    }    
  }
  
  
}

sealed class NetUtils {}
