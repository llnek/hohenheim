/*??
*
* Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
*
* This library is distributed in the hope that it will be useful
* but without any warranty; without even the implied warranty of
* merchantability or fitness for a particular purpose.
*
* The use and distribution terms for this software are covered by the
* Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
* which can be found in the file epl-v10.html at the root of this distribution.
*
* By using this software in any fashion, you are agreeing to be bound by
* the terms of this license.
* You must not remove this notice, or any other, from this software.
*
 ??*/

package com.zotoh.frwk.net

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.math._

import io.netty.handler.codec.http.HttpHeaders._
import io.netty.handler.codec.http.HttpHeaders.Names.COOKIE

import java.io.{ByteArrayOutputStream=>ByteArrayOS,OutputStream}
import java.io.{IOException,File}
import java.util.{Set,Properties=>JPS}

import io.netty.handler.codec.http.HttpResponseStatus._
import io.netty.handler.codec.http.HttpVersion._

import io.netty.buffer.ByteBuf
import io.netty.channel.Channel
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.group.ChannelGroup
import io.netty.handler.codec.http.Cookie
import io.netty.handler.codec.http.CookieDecoder
import io.netty.handler.codec.http.ServerCookieEncoder
import io.netty.handler.codec.http.DefaultHttpResponse
import io.netty.handler.codec.http.{HttpObject,HttpContent,LastHttpContent}
import io.netty.handler.codec.http.HttpHeaders
import io.netty.handler.codec.http.HttpMessage
import io.netty.handler.codec.http.HttpRequest
import io.netty.handler.codec.http.HttpResponse
import io.netty.handler.codec.http.HttpResponseStatus
import io.netty.handler.codec.http.HttpVersion
import io.netty.buffer.ByteBufHolder

import org.apache.commons.lang3.StringUtils
import com.zotoh.frwk.io.XData
import org.json._
import org.slf4j._
import org.apache.commons.io.IOUtils
import com.zotoh.frwk.util.{CoreUtils=>CU}
import com.zotoh.frwk.io.{IOUtils=>IO}


/**
 * @author kenl
 *
 */
object BasicChannelHandler {
  private val _log= LoggerFactory.getLogger(classOf[BasicChannelHandler])
}

/**
 * @author kenl
 *
 */
class BasicChannelHandler( private var _grp:ChannelGroup) extends SimpleChannelInboundHandler[HttpObject] {

  private var _thold= com.zotoh.frwk.io.IOUtils.streamLimit()
  private var _props= new JSONObject()
  private var _clen=0L
  private var _keepAlive=false

  private var _cookies:List[String]= null
  private var _fOut:File = null
  private var _os:OutputStream = null

  def tlog() = BasicChannelHandler._log
  def isKeepAlive() = _keepAlive

  override def channelInactive(ctx:ChannelHandlerContext) {
    val c= maybeGetChannel(ctx)
    if (c != null) { _grp.remove(c) }
    super.channelInactive(ctx)
    tlog.debug("BasicChannelHandler: channelClosed - ctx {}, channel {}",  ctx, if(c==null) "?" else c , "")
  }

  override def channelActive(ctx:ChannelHandlerContext) {
    val c= maybeGetChannel(ctx)
    if (c != null) { _grp.add(c) }
    super.channelActive(ctx)
    tlog().debug("BasicChannelHandler: channelOpen - ctx {}, channel {}", ctx, if (c==null) "?" else c, "")
  }

  override def exceptionCaught(ctx:ChannelHandlerContext, ev:Throwable) {
    tlog().error("", ev)
    val c= maybeGetChannel(ctx)
    if (c != null) try {
        c.close()
    } finally {
        _grp.remove(c)
    }
//    super.exceptionCaught(ctx, e)
  }

  // false to stop further processing
  protected def onRecvRequest(msgInfo:JSONObject) = true

  override def channelRead0(ctx:ChannelHandlerContext, msg:HttpObject) {
    
    val msgType= if (msg==null) "???" else msg.getClass().getName()
    val ch = ctx.channel()
    
    msg match {
      case x:HttpMessage =>
        _os= new ByteArrayOS(4096)
        _props= new JSONObject()
        msg_recv_0(x)
      case _ =>
    }

    msg match {
      case res:HttpResponse =>
        val s= res.getStatus()
        val r= s.reasonPhrase()
        val c= s.code()
        tlog().debug("BasicChannelHandler: got a response: code {} {}", CU.asJObj(c), CU.asJObj(r), "")        
        _props.put("headers", iterHeaders(res) )
        _props.put("reason", r)
        _props.put("dir", -1)
        _props.put("code", c)
        if (c >= 200 && c < 300) {
          onRes(ctx,s,res)
        } else if (c >= 300 && c < 400) {
          // TODO: handle redirect
          handleResError(ctx, new IOException("redirect not supported."))
        } else {
          handleResError(ctx, new IOException("error code: " + c))
        }
      case req:HttpRequest =>
        tlog().debug("BasicChannelHandler: got a request: ")
        if (is100ContinueExpected(req)) {
          send100Continue(ch)
        }
        _keepAlive = HttpHeaders.isKeepAlive(req)
        onReqIniz(ctx, req)
        _props.put("method", req.getMethod.name )
        _props.put("uri", req.getUri)
        _props.put("headers", iterHeaders(req) )
        _props.put("dir", 1)
        if ( onRecvRequest(_props) ) {
          onReq(ctx,req)
        } else {
          send403(ch)
        }
      case x:HttpContent => onChunk(ctx,x)
      case _ =>
        throw new IOException( "BasicChannelHandler:  unexpected msg type: " + msgType)            
    }
  }

  private def send100Continue(ch:Channel) {
    ch.writeAndFlush( new DefaultHttpResponse(HTTP_1_1, CONTINUE))
  }

  private def send403(ch:Channel) {
    ch.writeAndFlush( new DefaultHttpResponse(HTTP_1_1, FORBIDDEN))
    throw new IOException("403 Forbidden")
  }

  protected def onReq(ctx:ChannelHandlerContext, msg:HttpRequest) {
    if (HttpHeaders.isTransferEncodingChunked(msg)) {
      tlog.debug("BasicChannelHandler: request is chunked")
    } else {
      msg match {
        case x:ByteBufHolder => sockBytes(x.content)
        case _ => throw new IOException("Unknown request type: " + msg.getClass)
      }
      onMsgFinal(ctx)
    }
  }

  private def onRes(ctx:ChannelHandlerContext, rc:HttpResponseStatus, msg:HttpResponse) {
    onResIniz(ctx,msg)
    if (HttpHeaders.isTransferEncodingChunked(msg)) {
      tlog.debug("BasicChannelHandler: response is chunked")
    } else {
      msg match {
        case x:ByteBufHolder => sockBytes(x.content)
        case _ => throw new IOException("Unknown response type: " + msg.getClass)
      }
      onMsgFinal(ctx)
    }
  }

  protected def onReqIniz(ctx:ChannelHandlerContext, msg:HttpRequest ) {
    onReqPreamble( _props )
  }

  protected def onResIniz(ctx:ChannelHandlerContext, msg:HttpResponse ) {
    onResPreamble( _props)
  }

  protected def onReqPreamble(msgInfo:JSONObject) {
    tlog.debug("BasicChannelHandler: onReqIniz: Method {}, Uri {}",
        CU.nsb( msgInfo.optString("method")),
        CU.nsb( msgInfo.optString("uri")),
        "")
  }

  protected def onResPreamble(msgInfo:JSONObject) {}

  protected def doReqFinal(msgInfo:JSONObject , out:XData) {}
  protected def doResFinal(msgInfo:JSONObject , out:XData) {}
  protected def onResError(code:Int, r:String) {}

  private def handleResError(ctx:ChannelHandlerContext, err:Throwable) {
    val cc= maybeGetChannel(ctx)
    onResError( _props.optInt("code"), _props.optString("reason"))
    if ( !isKeepAlive && cc != null) {
      cc.close()
    }
  }

  private def sockBytes(cb:ByteBuf) {
    var loop=true
    if (cb != null) while (loop) {
      loop = cb.readableBytes() match {
        case c if c > 0 =>
          sockit(cb,c)
          true
        case _ => false
      }
    }
  }

  private def sockit(cb:ByteBuf, count:Int) {

    val bits= new Array[Byte](4096)
    var total=count

    while (total > 0) {
      val len = min(4096, total)
      cb.readBytes(bits, 0, len)
      _os.write(bits, 0, len)
      total -= len
    }

    _os.flush()

    if (_clen >= 0L) { _clen += count }
    if (_clen > 0L && _clen > _thold) {
      swap()
    }
  }

  private def swap() {
    _os match {
      case baos:ByteArrayOS =>
        val t= IO.newTempFile(true)
        t._2.write(baos.toByteArray)
        t._2.flush()
        _fOut= t._1
        _os=t._2
        _clen= -1L
    }
  }

  protected def doReplyError(ctx:ChannelHandlerContext, err:HttpResponseStatus) {
    doReplyXXX(ctx, err)
  }

  private def doReplyXXX(ctx:ChannelHandlerContext,  s:HttpResponseStatus) {
    val res= new DefaultHttpResponse(HttpVersion.HTTP_1_1, s)
    val c= maybeGetChannel(ctx)
    //HttpHeaders.setTransferEncodingChunked(res,false)
    HttpHeaders.setHeader(res, "content-length", "0")
    c.write(res)
    if ( ! isKeepAlive && c != null ) {
      c.close()
    }
  }

  protected def replyRequest(ctx:ChannelHandlerContext,  data:XData) {
    doReplyXXX(ctx,HttpResponseStatus.OK)
  }

  protected def replyResponse(ctx:ChannelHandlerContext, data:XData) {
    val c= maybeGetChannel(ctx)
    if ( ! isKeepAlive && c != null ) {
      c.close()
    }
  }

  private def onMsgFinal(ctx:ChannelHandlerContext) {
    val dir = _props.optInt("dir")
    val out= on_msg_final(ctx)
    if ( dir > 0) {
      replyRequest(ctx,out)
      doReqFinal( _props, out)
    } else if (dir < 0) {
      doResFinal( _props, out)
    }
  }

  private def on_msg_final(ctx:ChannelHandlerContext) = {
    val data= new XData() 
    if (_fOut != null) {
      data.resetContent(_fOut)
    } else {
      data.resetContent(_os)
    }
    IOUtils.closeQuietly(_os)
    _fOut=null
    _os=null
    data
  }

  protected def maybeGetChannel(ctx:ChannelHandlerContext) = {
    ctx.channel()
  }

  private def msg_recv_0(msg:HttpMessage) {
    val s= HttpHeaders.getHeader(msg,COOKIE)
    if ( ! StringUtils.isEmpty(s)) {
      val cs = CookieDecoder.decode(s)
      if (cs.size() > 0) {
    	  _cookies= ServerCookieEncoder.encode(cs).toList
      }
    }
  }

  private def onChunk(ctx:ChannelHandlerContext, msg:HttpContent ) {
    sockBytes(msg.content)
    msg match {
      case x:LastHttpContent => onMsgFinal(ctx)
      case _ =>
    }
  }

  protected def iterHeaders(msg:HttpMessage) = {
    val hdrs= new JSONObject()
    val h= msg.headers
    h.names().foreach { (n) =>
      val arr=h.getAll(n).foldLeft(new JSONArray() ) { (arr, h) => arr.put(h) ; arr } 
      hdrs.put(n, arr)
    }
    tlog.debug("BasicChannelHandler: headers\n{}", hdrs.toString(2) )
    hdrs
  }

}

