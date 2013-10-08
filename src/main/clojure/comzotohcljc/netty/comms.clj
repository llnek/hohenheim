;;
;; Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
;;
;; This library is distributed in the hope that it will be useful
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.netty.comms )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(java.lang.reflect Field))
(import '(org.apache.commons.io IOUtils FileUtils))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io IOException ByteArrayOutputStream File OutputStream InputStream))
(import '(java.util HashMap))
(import '(java.net URI URL InetSocketAddress))
(import '(java.util.concurrent Executors))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(javax.net.ssl X509TrustManager TrustManager))

(import '(io.netty.bootstrap Bootstrap ServerBootstrap))
(import '(io.netty.util AttributeKey Attribute))
(import '(io.netty.util.concurrent GlobalEventExecutor))
(import '(io.netty.channel.nio NioEventLoopGroup))
(import '(io.netty.buffer ByteBuf ByteBufHolder Unpooled))

(import '(io.netty.handler.stream ChunkedWriteHandler ChunkedStream))
(import '(io.netty.channel.socket.nio
  NioServerSocketChannel NioSocketChannel))
(import '(io.netty.channel
  ChannelHandlerContext Channel SimpleChannelInboundHandler
  ChannelFutureListener ChannelFuture ChannelInitializer
  ChannelPipeline ChannelHandler ChannelOption))
(import '(io.netty.channel.socket SocketChannel))
(import '(io.netty.channel.group
  ChannelGroupFuture ChannelGroup ChannelGroupFutureListener
  DefaultChannelGroup))
(import '(io.netty.handler.codec.http 
  HttpHeaders HttpVersion HttpContent LastHttpContent
  HttpHeaders$Values HttpHeaders$Names
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse QueryStringDecoder HttpMethod
  DefaultHttpRequest HttpServerCodec HttpClientCodec
  HttpResponseEncoder))
(import '(io.netty.handler.ssl SslHandler))
(import '(io.netty.handler.codec.http.websocketx
  WebSocketFrame
  WebSocketServerHandshaker
  WebSocketServerHandshakerFactory
  ContinuationWebSocketFrame
  CloseWebSocketFrame
  BinaryWebSocketFrame
  TextWebSocketFrame
  PingWebSocketFrame
  PongWebSocketFrame))
(import '(com.zotoh.frwk.net NetUtils))
(import '(com.zotoh.frwk.io XData))


(require '[comzotohcljc.crypto.stores :as ST])
(require '[comzotohcljc.crypto.core :as CY])
(require '[comzotohcljc.crypto.ssl :as CS])
(require '[comzotohcljc.net.comms :as NU])

(use '[comzotohcljc.net.rts])

(require '[comzotohcljc.util.files :as FU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.util.core :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(comment
(def HTTP-CODES
  (let [fields (:fields (bean HttpResponseStatus))
        to-key (comp int (fn [^Field f] (.code ^HttpResponseStatus (.get f nil))))
        kkeys (map to-key fields)
        vvals (map (comp str (fn [^Field f] (.get f nil))) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map of { int (code) -> HttpResponseStatus }
(def HTTP-CODES
  (let [fields (:fields (bean HttpResponseStatus))
        to-key (fn [^Field f] (.code ^HttpResponseStatus (.get f nil)))
        kkeys (map to-key fields)
        vvals (map (fn [^Field f] (.get f nil)) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main netty classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private add-listener (fn [a & more ] (class a)))

(defprotocol NettyServiceIO
  ""
  (before-send [_ ^Channel ch msg] )
  (onerror [_ ^Channel ch msginfo ^Throwable err] )
  (onreq [_ ^Channel ch req msginfo ^XData xdata] )
  (onres [_ ^Channel ch rsp msginfo ^XData xdata] ))

(defprotocol RouteCracker
  ""
  (routable? [_ msgInfo] )
  (hasRoutes? [_])
  (crack [_ msgInfo] ))



(defn make-resp-status "Make a http response object."

  (^HttpResponse [] (make-resp-status 200))

  (^HttpResponse [status]
    (DefaultHttpResponse. HttpVersion/HTTP_1_1
                          (get HTTP-CODES status))))

(defn closeCF "Maybe close the channel." [doit ^ChannelFuture cf]
  (if (and doit (CU/notnil? cf))
    (.addListener cf ChannelFutureListener/CLOSE)))

(defn wflush [^Channel ch obj]
  (NetUtils/wrtFlush ch obj))

(defn sendRedirect "" [ ^Channel ch perm ^String targetUrl]
  (let [ rsp (make-resp-status (if perm 301 307)) ]
    (debug "redirecting to " targetUrl)
    (HttpHeaders/setHeader rsp  "location" targetUrl)
    (closeCF true (wflush ch rsp))))

(defn make-nilServiceIO "Reify a no-op service-io object."

  ^comzotohcljc.netty.comms.NettyServiceIO
  []

  (reify NettyServiceIO
    (before-send [_ ch msg] (debug "empty before-send." ))
    (onerror [_ ch msginfo evt] (debug "empty onerror." ))
    (onreq [_ ch req msginfo xdata] (debug "empty onreq." ))
    (onres [_ ch rsp msginfo xdata] (debug "empty onres." ))))

(defn server-bootstrap "Make a netty server bootstrap."

  ([] (server-bootstrap {} ))
  ([options]
    (let [ gp (NioEventLoopGroup.) gc (NioEventLoopGroup.)
           bs (doto (ServerBootstrap.)
                (.group gp gc)
                (.channel (class NioServerSocketChannel))
                (.option ChannelOption/SO_REUSEADDR true)
                (.childOption ChannelOption/SO_RCVBUF (int (* 2 1024 1024)))
                (.childOption ChannelOption/TCP_NODELAY true)) ]
      (doseq [ [k v] (seq options) ]
        (if (= :child k)
          (doseq [ [x y] (seq v) ]
            (.childOption bs x y))
          (.option bs k v)))
      bs)))

(defn client-bootstrap "Make a netty client bootstrap."

  ([] (client-bootstrap {} ))
  ([options]
   (let [ g (NioEventLoopGroup.)
          bs (doto (Bootstrap.)
               (.group g)
               (.channel (class NioSocketChannel))
               (.option ChannelOption/TCP_NODELAY true)
               (.option ChannelOption/SO_KEEPALIVE true)) ]
     (doseq [ [ k v] (seq options) ]
       (.option bs k v))
     bs)))

(defn channel-group "Make a channel group."

  ^ChannelGroup
  []
  (DefaultChannelGroup. (CU/uid) GlobalEventExecutor/INSTANCE))

(defn kill-9 "Clean up resources used by a netty server." [^ServerBootstrap bs]
  (let [ gc (.childGroup bs)
         gp (.group bs) ]
    (when-not (nil? gp) 
      (CU/Try! (.shutdownGracefully gp)))
    (when-not (nil? gc) 
      (CU/Try! (.shutdownGracefully gc))) ))

(defn finz-server "Bring down a netty server."

  [ {^ServerBootstrap server :server ^ChannelGroup cg :cgroup } ]
  (if (nil? cg)
    (kill-9 server)
    (-> (.close cg) (add-listener { :done (fn [_] (kill-9 server)) }))))

(defn send-100-cont "" [^ChannelHandlerContext ctx]
  (let [ ch (.channel ctx) ]
    (wflush ch (make-resp-status 100))))

(defrecord NettyServer [^ServerBootstrap server ^ChannelGroup cgroup ] )
(defrecord NettyClient [^Bootstrap client ^ChannelGroup cgroup ] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- opComplete "" [chOrGroup success error options]
  (cond
    (fn? (:done options))
    (apply (:done options) chOrGroup)

    (and (fn? (:nok options)) (not success))
    (apply (:nok options) chOrGroup error)

    (and (fn? (:ok options)) success)
    (apply (:ok options) chOrGroup)

    :else nil)
  nil)

(defmethod ^:private add-listener ChannelGroupFuture
  [^ChannelGroupFuture cf options]
  (let [ cg (.group cf) ]
    (-> cf (.addListener
              (reify ChannelGroupFutureListener
                (operationComplete [_ cff]
                  (opComplete cg
                               (.isSuccess ^ChannelGroupFuture cff)
                               nil
                               options )))))) )

(defmethod ^:private add-listener ChannelFuture
  [^ChannelFuture cf options]
  (let [ ch (.channel cf) ]
    (-> cf (.addListener
              (reify ChannelFutureListener
                (operationComplete [_ cff]
                  (opComplete ch
                               (.isSuccess ^ChannelFuture cff)
                               (.cause ^ChannelFuture cff)
                               options )))))) )

(defn- maybe-keepAlive "make channel-future listener to close the channel"
  [^ChannelFuture cf keepAlive]
  (when-not keepAlive
    (add-listener cf
                  { :done
                    (fn [^ChannelFuture cff] (NetUtils/closeChannel (.channel cff))) } )))

(defn- reply-xxx [^Channel ch status]
  (let [ info (.attr ch (AttributeKey. "info"))
         res (make-resp-status status)
         m (.get info)
         kalive (if (nil? m) false (:keep-alive m)) ]
    (HttpHeaders/setTransferEncodingChunked res)
    (HttpHeaders/setContentLength res 0)
    (-> (wflush ch res) (maybe-keepAlive kalive))))

(defn- nioMapHeaders "turn headers into a clj-map"
  [^HttpMessage msg]
  (let [ hds (.headers msg) ]
    (persistent! (reduce (fn [sum ^String n]
              (assoc! sum (.toLowerCase n) (vec (.getAll hds n))))
      (transient {}) (seq (.names hds)))) ))

(defn- nioExtractMsg "get info from http message"
  [^HttpMessage msg]
    { :protocol (-> msg (.getProtocolVersion) (.toString))
      :is-chunked (HttpHeaders/isTransferEncodingChunked msg)
      :keep-alive (HttpHeaders/isKeepAlive msg)
      :clen (HttpHeaders/getContentLength msg)
      :headers (nioMapHeaders msg) } )

(defn- nioExtractReq "extract info from request"
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpRequest req]
  (let [ ws (.toLowerCase (SU/strim (HttpHeaders/getHeader req "upgrade")))
         decr (QueryStringDecoder. (.getUri req))
         md (.toUpperCase (-> req (.getMethod) (.name)))
         uri (.path decr)
         m1 (nioExtractMsg req)
         params (persistent! (reduce (fn [sum en]
                          (assoc! sum (.toLowerCase (SU/nsb (first en))) (vec (nth en 1))))
                      (transient {}) (seq (.parameters decr)))) ]

    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1)
      (if (= "websocket" ws) "WS" md)
      uri
      (:is-chunked m1)
      (:keep-alive m1) (:clen m1) (:headers m1) params)))

(defn- nioExtractRes "extract info from response"
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpResponse res]
  (let [ m1 (nioExtractMsg res) ]
    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1) "" "" (:is-chunked m1)
      (:keep-alive m1) (:clen m1) (:headers m1) {})))

(defn- nioComplete "" [^ChannelHandlerContext ctx msg]
  (let [ ch (.channel ctx)
         ^XData xdata (-> (.attr ch (AttributeKey. "xs")) (.get))
         info (-> (.attr ch (AttributeKey. "info")) (.get))
         dir (-> (.attr ch (AttributeKey. "dir")) (.get))

         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (-> (.attr ch (AttributeKey. "cb")) (.get))

         os (-> (.attr ch (AttributeKey. "os")) (.get))
         clen (-> (.attr ch (AttributeKey. "clen")) (.get)) ]

    (when (and (> clen 0) (instance? ByteArrayOutputStream os))
      (.resetContent xdata os))
    (cond
      (instance? HttpResponse dir) (do
                   (CU/TryC (NetUtils/closeChannel ch))
                   (.onres usercb ch dir info xdata))
      (instance? HttpRequest dir) (do
                  (.onreq usercb ch dir info xdata))
      (instance? WebSocketFrame msg) (do
                  (.onreq usercb ch msg info xdata))
      :else nil)
    ))

(defn- nioSockitDown

  [^ChannelHandlerContext ctx ^ByteBuf cbuf]
  (let [ ch (.channel ctx)
         ^XData xdata (-> (.attr ch (AttributeKey. "xs")) (.get))
         ^OutputStream cout (-> (.attr ch (AttributeKey. "os")) (.get))
         csum (-> (.attr ch (AttributeKey. "clen")) (.get))
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile xdata)
                  cout
                  (NetUtils/swapFileBacked xdata cout nsum)) ]
    (-> (.attr ch (AttributeKey. "clen")) (.set nsum))
    (-> (.attr ch (AttributeKey. "os")) (.set nout)) ))

(defn- nioCfgCtx [^ChannelHandlerContext ctx usercb]
  (let [ ch (.channel ctx) ]
    (-> (.attr ch (AttributeKey. "os")) (.set (IO/make-baos)))
    (-> (.attr ch (AttributeKey. "xs")) (.set (XData.)))
    (-> (.attr ch (AttributeKey. "clen")) (.set 0))
    (-> (.attr ch (AttributeKey. "cb")) (.set usercb)) ))

(defn- nioPError [^ChannelHandlerContext ctx err]
  (let [ ch (.channel ctx)
         info (-> (.attr ch (AttributeKey. "info")) (.get))
         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (-> (.attr ch (AttributeKey. "cb")) (.get)) ]
    (.onerror usercb ch info err) ))

(defn- nioFinz "close any output stream created during the message handling"
  [^ChannelHandlerContext ctx]
  (let [ ch (.channel ctx)
         ^OutputStream
         os (-> (.attr ch (AttributeKey. "os")) (.get)) ]
    (IOUtils/closeQuietly os)))

(defn- nioFrameChunk [^ChannelHandlerContext ctx ^ContinuationWebSocketFrame frame]
  (let [ cbuf (.content frame) ]
    (when-not (nil? cbuf) (nioSockitDown ctx cbuf))
    (when (.isFinalFragment frame)
      (do
        (nioFinz ctx)
        (when (nil? cbuf)
          (let [s (SU/nsb (.aggregatedText frame))
                ch (.channel ctx)
                ^XData
                xs (-> (.attr ch (AttributeKey. "xs")) (.get)) ]
            (-> (.attr ch (AttributeKey. "clen")) (.set (.length s)))
            (.resetContent xs s)))
        (nioComplete ctx frame) )) ))

(defn- getBits [^ChannelHandlerContext ctx ^WebSocketFrame frame]
  (when-let [ buf (.content frame) ]
    (nioSockitDown ctx buf)))

(defn- nioWSFrame [^ChannelHandlerContext ctx ^WebSocketFrame frame ]
  (let [ ch (.channel ctx)
         ^XData
         xs (-> (.attr ch (AttributeKey. "xs")) (.get))
         ^WebSocketServerHandshaker 
         hs (-> (.attr ch (AttributeKey. "hs")) (.get)) ]
    (debug "nio-wsframe: received a " (class frame) )
    (cond
      (instance? CloseWebSocketFrame frame)
      (.close hs ch ^CloseWebSocketFrame frame)

      (instance? PingWebSocketFrame frame)
      (wflush ch (PongWebSocketFrame. (.content frame)))

      (instance? BinaryWebSocketFrame frame)
      (do
        (getBits ctx frame)
        (nioComplete ctx frame))

      (instance? TextWebSocketFrame frame)
      (let [ s (SU/nsb (.text ^TextWebSocketFrame frame)) ]
        (.resetContent xs s)
        (-> (.attr ch (AttributeKey. "clen")) (.set (.length s)))
        (nioComplete ctx frame))

      (instance? ContinuationWebSocketFrame frame)
      (nioFrameChunk ctx frame)

      :else ;; what else can this be ????
      nil) ))


(defn- maybeSSL "" [^ChannelHandlerContext ctx]
  (let [ ssl (-> (NetUtils/getPipeline ctx) (.get (class SslHandler))) ]
    (when-not (nil? ssl)
      (let [ cf (.handshakeFuture ^SslHandler ssl) ]
        (add-listener cf { :nok (fn [^Channel c] (NetUtils/closeChannel c)) } )))))

(defn- nioWSock "" [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ ch (.channel ctx)
         rts (-> (.attr ch (AttributeKey. "rts")) (.get)) ]
    (if (false? rts)
      (reply-xxx ch 404)
      (let [ wf (WebSocketServerHandshakerFactory.
                  (str "ws://"
                       (HttpHeaders/getHeader req "host")
                       (.getUri req)) nil false)
             hs (.newHandshaker wf req) ]
        (if (nil? hs)
          (do
            (WebSocketServerHandshakerFactory/sendUnsupportedWebSocketVersionResponse ch)
            (CU/Try! (NetUtils/closeChannel ch)))
          (do
            (-> (.attr ch (AttributeKey. "hs")) (.set hs))
            (add-listener (.handshake hs ch req)
                          { :nok (fn [^Channel c ^Throwable e]
                                   (-> (NetUtils/getPipeline c) (.fireExceptionCaught e)))
                            :ok (fn [c] (maybeSSL ctx)) } ))))) ))

(defn- nioWReq [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ ch (.channel ctx)
         msginfo (-> (.attr ch (AttributeKey. "info")) (.get))
         rts (-> (.attr ch (AttributeKey. "rts")) (.get)) ]
    (debug "nioWReq: received a " (:method msginfo ) " request from " (:uri msginfo))
    ;; if it's a valid route, continue as usual
    (if (true? rts)
      (do
        (when (HttpHeaders/is100ContinueExpected req) (send-100-cont ctx))
        (if (:is-chunked msginfo)
          (debug "nioWReq: request is chunked.")
          ;; msg not chunked, suck all data from the request
          (do
            (try
                (nioSockitDown ctx (.content ^ByteBufHolder req))
              (finally (nioFinz ctx)))
            (nioComplete ctx req))))
      ;; bad route, so just pass it down and let it handle the error.
      (nioComplete ctx req))))

(defn- nioPRequest "handle a request"
  [^ChannelHandlerContext ctx ^HttpRequest req usercb
   ^comzotohcljc.netty.comms.RouteCracker rtcObj]
  (let [ msginfo (nioExtractReq req)
         ch (.channel ctx)
         rts (if (and (not (nil? rtcObj))
                      (.hasRoutes? rtcObj))
                (.routable? rtcObj msginfo)
                true) ]
    (-> (.attr ch (AttributeKey. "info")) (.set msginfo))
    (-> (.attr ch (AttributeKey. "rts")) (.set rts))
    (-> (.attr ch (AttributeKey. "dir")) (.set req))
    (nioCfgCtx ctx usercb)
    (if (= "WS" (:method msginfo))
        (nioWSock ctx req)
        (nioWReq ctx req))))

(defn- nioRedirect "" [^ChannelHandlerContext ctx msg]
  ;; TODO: handle redirect properly, for now, same as error
  (nioPError ctx (IOException. "Unsupported redirect.")))

(defn- nioPResBody "" [^ChannelHandlerContext ctx ^HttpResponse res]
  (if (HttpHeaders/isTransferEncodingChunked res)
    (debug "nioPResBody: response is chunked.")
    (try
        (nioSockitDown ctx (.content ^ByteBufHolder res))
      (finally (nioFinz ctx)))))

(defn- nioPRes "handle a response"
  [^ChannelHandlerContext ctx ^HttpResponse res usercb]
  (let [ msginfo (nioExtractRes res)
         ch (.channel ctx)
         s (.getStatus res)
         r (.reasonPhrase s)
         c (.code s) ]
    (debug "nioPRes: got a response: code " c " reason: " r)
    (-> (.attr ch (AttributeKey. "info")) (.set msginfo))
    (-> (.attr ch (AttributeKey. "dir")) (.set res))
    (-> (.attr ch (AttributeKey. "rts")) (.set false))
    (nioCfgCtx ctx usercb)
    (cond
      (and (>= c 200) (< c 300)) (nioPResBody ctx res)
      (and (>= c 300) (< c 400)) (nioRedirect ctx)
      :else (nioPError ctx (IOException. (str "error code: " c))))))

(defn- nioChunk "handle a chunk - part of a message"
  [^ChannelHandlerContext ctx ^HttpContent msg]
  (let [ done (try
                  (nioSockitDown ctx (.content msg))
                  (instance? LastHttpContent msg)
                (catch Throwable e#
                    (do (nioFinz ctx) (throw e#)))) ]
    (if done (nioComplete ctx msg) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make our channel handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn netty-pipe-handler "Make a generic Netty 4.x Pipeline Handler."
  ^ChannelHandler
  [^comzotohcljc.netty.comms.NettyServiceIO usercb
   ^comzotohcljc.netty.comms.RouteCracker rtcObj ]
  (proxy [SimpleChannelInboundHandler] []

    (exceptionCaught [ctx err]
      (let [ ch (.channel ^ChannelHandlerContext ctx)
             msginfo (-> (.attr ch (AttributeKey. "info")) (.get))
             keepAlive (if (nil? msginfo) false (:keep-alive msginfo)) ]
        (error err "")
        (.onerror usercb ch msginfo err)
        (when-not keepAlive (CU/Try! (NetUtils/closeChannel ch)))))

    (channelReadComplete [ctx]
      (.flush ^ChannelHandlerContext ctx))

    (channelRead0 [ctx msg]
      (let []
        (cond
          (instance? HttpRequest msg) (nioPRequest ctx msg usercb rtcObj)
          (instance? WebSocketFrame msg) (nioWSFrame ctx msg)
          (instance? HttpResponse msg) (nioPRes ctx msg usercb)
          (instance? HttpContent msg) (nioChunk ctx msg)
          :else (throw (IOException. "Received some unknown object." ) ))))

    ))

(defn make-pipeServer "Make a Serverside PipelineFactory." ^ChannelInitializer [^SSLContext sslctx usercb rtcObj]
  (proxy [ChannelInitializer][]
    (initChannel [^SocketChannel ch]
      (let [ ^ChannelPipeline pl (NetUtils/getPipeline ch)
             eg (if (nil? sslctx)
                    nil
                    (doto (.createSSLEngine sslctx)(.setUseClientMode false))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
        (.addLast pl "codec" (HttpServerCodec.))
        (.addLast pl "chunker" (ChunkedWriteHandler.))
        (.addLast pl "handler" (netty-pipe-handler usercb rtcObj))
        pl))))

(defn make-pipeClient "Make a Clientside PipelineFactory" ^ChannelInitializer [^SSLContext sslctx usercb]
  (proxy [ChannelInitializer][]
    (initChannel [^SocketChannel ch]
      (let [ ^ChannelPipeline pl (NetUtils/getPipeline ch)
             eg (if (nil? sslctx)
                    nil
                    (doto (.createSSLEngine sslctx)(.setUseClientMode true))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
        (.addLast pl "codec" (HttpClientCodec.))
        (.addLast pl "chunker" (ChunkedWriteHandler.))
        (.addLast pl "handler" (netty-pipe-handler usercb nil))
        pl))))

(defn netty-xxx-server "Make a Netty server."
  ^NettyServer
  [^String host
   port
   ^comzotohcljc.netty.comms.NettyServiceIO
   usercb
   options]

  (let [ ^URL keyUrl (:serverkey options)
         pwdObj (:passwd options)
         ssl (if (nil? keyUrl) nil (CS/make-sslContext keyUrl pwdObj))
         ^ServerBootstrap bs (server-bootstrap options)
         cg (channel-group)
         pf (make-pipeServer ssl usercb nil) ]
     (.childHandler bs pf)
     (.add cg (-> bs 
                (.bind (InetSocketAddress. host (int port)))
                (.sync)
                (.channel)))
     (debug "netty-xxx-server: running on host " host ", port " port)
     (NettyServer. bs cg) ))

(defn make-mem-httpd "Make an in-memory http server."

  [^String host
   port
   ^comzotohcljc.netty.comms.NettyServiceIO
   usercb
   options ]

  (netty-xxx-server host port usercb options ))

(defn- reply-get-vfile [^Channel ch ^XData xdata]
  (let [ res (make-resp-status)
         clen (.size xdata)
         info (-> (.attr ch (AttributeKey. "info")) (.get))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (HttpHeaders/setHeader res "content-type" "application/octet-stream")
    (HttpHeaders/setContentLength res clen)
    (HttpHeaders/setTransferEncodingChunked res)
    (wflush ch res)
    (-> (wflush ch (ChunkedStream. (.stream xdata)))
      (maybe-keepAlive kalive))))

(defn- filer-handler [^File vdir]
  (let [ putter (fn [^Channel ch ^String fname ^XData xdata]
                  (try
                      (FU/save-file vdir fname xdata)
                      (reply-xxx ch 200)
                    (catch Throwable e#
                      (reply-xxx ch 500))))
         getter (fn [^Channel ch ^String fname]
                  (let [ xdata (FU/get-file vdir fname) ]
                    (if (.hasContent xdata)
                      (reply-get-vfile ch xdata)
                      (reply-xxx ch 204)))) ]
    (reify NettyServiceIO
      (before-send [_ ch msg] nil)
      (onerror [_ ch msginfo err]
        (do
          (when-not (nil? err) (error err ""))
          (reply-xxx ch 500)))
      (onres [_ ch rsp msginfo xdata] nil)
      (onreq [_ ch req msginfo xdata]
        (let [ mtd (SU/nsb (:method msginfo))
               uri (SU/nsb (:uri msginfo))
               pos (.lastIndexOf uri (int \/))
               p (if (< pos 0) uri (.substring uri (inc pos))) ]
          (debug "Method = " mtd ", Uri = " uri ", File = " p)
          (cond
            (or (= mtd "POST")(= mtd "PUT")) (putter ^Channel ch p xdata)
            (= mtd "GET") (getter ^Channel ch p)
            :else (reply-xxx ch 405)))))
      ))

(defn make-mem-filer ""
  [^String host port options]
  (netty-xxx-server host port (filer-handler (:vdir options)) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http client functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-httpClient "Make a Netty 3.x client."

  (^NettyClient [] (make-httpClient {} ))

  (^NettyClient [options]
    (let [ bs (client-bootstrap options)
           cg (channel-group) ]
      (NettyClient. bs cg ))))

(defn- iniz-httpClient

  ([clientr ^URL targetUrl]
   (iniz-httpClient clientr targetUrl (make-nilServiceIO) ))

  ([clientr ^URL targetUrl serviceIO]
    (let [ ctx (CS/make-sslClientCtx (= "https" (.getScheme (.toURI targetUrl))))
           ^String host (.getHost targetUrl)
           pnum (.getPort targetUrl)
           ^long port (if (< pnum 0) (if (nil? ctx) 80 443) pnum)
           pl (make-pipeClient ctx serviceIO)
           ^Bootstrap cli (:client clientr)
           ^ChannelGroup cg (:cgroup clientr) ]
      (debug "Netty client connecting to " host ":" port)
      (.handler cli pl)
      (let [ ^ChannelFuture cf (doto (.connect cli
                                               (InetSocketAddress. host port))
                  (.awaitUninterruptibly))
             ok (.isSuccess cf)
             e (if (not ok) (.cause cf) nil) ]
        (when-not ok
          (if (nil? e)
            (throw (IOException. "Failed to connect to URL: " targetUrl))
            (throw e)))
        (let [ ch (.channel cf) ]
          (.add cg ch)
          (debug "Netty client connected to " host ":" port " - OK.")
          ch)))))

(defn- send-httpClient

  [^Channel ch ^NettyClient clientr ^XData xdata options]

  (let [ req (DefaultHttpRequest. (HttpVersion/HTTP_1_1)
                                  (if (nil? xdata) (HttpMethod/GET) (HttpMethod/POST))
                                  (:url options))
         clen (if (nil? xdata) 0 (.size xdata))
         before-send (:before-send options)
         ka (:keepAlive (:options clientr))
         uri (.getUri req) ]
    (HttpHeaders/setHeader req "Connection" (if ka "keep-alive" "close"))
    (HttpHeaders/setHeader req "host" (.getHost (URL. uri)))
    (when (fn? before-send) (before-send req))
    (let [ ct (HttpHeaders/getHeader req "content-type") ]
      (when (and (StringUtils/isEmpty ct) (not (nil? xdata)))
        (HttpHeaders/setHeader req "content-type" "application/octet-stream")) )
    (when (> clen 0)
      (do
        (debug "Netty client: content has length " clen)
        (HttpHeaders/setHeader req "content-length" (str "" clen))))
    (debug "Netty client: about to flush out request (headers)")
    (let [ ^ChannelFuture cf (doto (wflush ch req)
                (add-listener { :ok #(do (debug "Netty client: headers flushed")) })) ]
      (when (> clen 0)
        (doto
          (if (> clen (com.zotoh.frwk.io.IOUtils/streamLimit))
            (wflush ch (ChunkedStream. (.stream xdata)))
            (wflush ch (Unpooled/copiedBuffer (.javaBytes xdata))))
          (add-listener { :ok #(do (debug "Netty client: payload flushed")) }))))
    ))

(defn async-post ""

  ([clientr ^URL targetUrl ^XData xdata]
   (async-post clientr targetUrl xdata (make-nilServiceIO)))

  ([clientr ^URL targetUrl ^XData xdata serviceIO]
   (send-httpClient
     (iniz-httpClient clientr targetUrl serviceIO)
     clientr
     xdata
     { :url (.toString targetUrl)
       :before-send (:before-send serviceIO) } )))

(defn async-get ""

  ([clientr ^URL targetUrl]
   (async-get clientr targetUrl (make-nilServiceIO)))

  ([clientr ^URL targetUrl serviceIO]
   (send-httpClient
     (iniz-httpClient clientr targetUrl serviceIO)
     clientr
     nil
     { :url (.toString targetUrl)
       :before-send (:before-send serviceIO) } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; router cracker
;;

;; [ri mc] routeinfo matcher
(defn- seekRoute [mtd uri rts]
  (if-not (nil? rts)
    (some (fn [^comzotohcljc.net.rts.RouteInfo ri]
            (let [ m (.resemble? ri mtd uri) ]
              (if (nil? m) nil [ri m])))
          (seq rts)) ))

(defn make-routeCracker ^comzotohcljc.netty.comms.RouteCracker [routes]
  (reify RouteCracker
    (hasRoutes? [_] (> (count routes) 0))
    (routable? [this msgInfo]
      (first (crack this msgInfo)))
    (crack [_ msgInfo]
      (let [ ^String mtd (:method msgInfo)
             ^String uri (:uri msgInfo)
             rc (seekRoute mtd uri routes)
             rt (if (nil? rc)
                  [false nil nil ""]
                  [true (first rc)(last rc) ""] ) ]
        (if (and (not (nth rt 0))
                 (not (.endsWith uri "/"))
                 (seekRoute mtd (str uri "/") routes))
          [true nil nil (str uri "/")]
          rt)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private comms-eof nil)


