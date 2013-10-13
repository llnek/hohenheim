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

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(java.lang.reflect Field))
(import '(org.apache.commons.io IOUtils FileUtils))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io IOException ByteArrayOutputStream
  File OutputStream InputStream))
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

(import '(io.netty.handler.stream
  ChunkedWriteHandler ChunkedStream))
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
  DefaultFullHttpResponse DefaultHttpResponse QueryStringDecoder
  HttpMethod
  DefaultFullHttpRequest HttpServerCodec HttpClientCodec
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


(use '[comzotohcljc.crypto.ssl :only [make-sslContext make-sslClientCtx] ])
(use '[comzotohcljc.net.rts])
(use '[comzotohcljc.util.files :only [save-file get-file] ])
(use '[comzotohcljc.util.core :only [uid notnil? Try! TryC] ])
(use '[comzotohcljc.util.str :only [strim nsb hgl?] ])
(use '[comzotohcljc.util.io :only [make-baos] ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(comment
(def HTTP-CODES
  (let [ to-key (comp int (fn [^Field f] (.code ^HttpResponseStatus (.get f nil))))
         fields (:fields (bean HttpResponseStatus))
         kkeys (map to-key fields)
         vvals (map (comp str (fn [^Field f] (.get f nil))) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))
)

;; map of { int (code) -> HttpResponseStatus }
(def HTTP-CODES
  (let [ to-key (fn [^Field f] (.code ^HttpResponseStatus (.get f nil)))
         fields (:fields (bean HttpResponseStatus))
         kkeys (map to-key fields)
         vvals (map (fn [^Field f] (.get f nil)) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main netty classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord NettyServer  [^ServerBootstrap server ^ChannelGroup cgroup ] )
(defrecord NettyClient  [^Bootstrap client ^ChannelGroup cgroup ] )

(defmulti ^:private addListener "" (fn [a & more ] (class a)))

(defprotocol NettyServiceIO
  ""
  (onreq [_ ch req msginfo xdata] )
  (before-send [_ ch msg] )
  (onerror [_ ch msginfo err] )
  (onres [_ ch rsp msginfo xdata] ))

(defprotocol RouteCracker
  ""
  (routable? [_ msgInfo] )
  (hasRoutes? [_])
  (crack [_ msgInfo] ))

(def ^:private ATTR-KEY (AttributeKey. "attObj"))

(defn sa-map! "Set attachment object from the channel."
  [^Channel ch obj]
  (-> (.attr ch ATTR-KEY) (.set obj)))

(defn ga-map "Get attachment object from the channel."
  [^Channel ch]
  (let [ rc (-> (.attr ch ATTR-KEY) (.get)) ]
  (if (nil? rc) {} rc)))

(defn kill9 "Clean up resources used by a netty server."
  [^ServerBootstrap bs]
  (let [ gc (.childGroup bs)
         gp (.group bs) ]
    (when-not (nil? gp)
      (Try! (.shutdownGracefully gp)))
    (when-not (nil? gc)
      (Try! (.shutdownGracefully gc))) ))

(defn finzServer "Bring down a netty server."
  [ { server :server cg :cgroup } ]
  (if (nil? cg)
    (kill9 server)
    (-> (.close ^ChannelGroup cg)
        (addListener { :done (fn [_] (kill9 server)) }))))

(defn makeFullHttpReply "Make a netty http-response object."
  (^HttpResponse [status ^ByteBuf obj]
    (DefaultFullHttpResponse. HttpVersion/HTTP_1_1
                              (get HTTP-CODES status)
                              obj))
  (^HttpResponse [] (makeFullHttpReply 200))
  (^HttpResponse [status]
    (DefaultFullHttpResponse. HttpVersion/HTTP_1_1
                          (get HTTP-CODES status))))

(defn makeHttpReply "Make a netty http-response object."
  (^HttpResponse [] (makeHttpReply 200))
  (^HttpResponse [status]
    (DefaultHttpResponse. HttpVersion/HTTP_1_1
                          (get HTTP-CODES status))))

(defn closeCF "Maybe close the channel."
  [doit ^ChannelFuture cf]
  (if (and doit (notnil? cf))
    (.addListener cf ChannelFutureListener/CLOSE)))

(defn wwrite "Write object." ^ChannelFuture [^Channel ch obj]
  ;; had to do this to work-around reflection warnings :(
  (NetUtils/writeOnly ch obj))

(defn wflush "Write object and then flush." ^ChannelFuture [^Channel ch obj]
  ;; had to do this to work-around reflection warnings :(
  (NetUtils/wrtFlush ch obj))

(defn sendRedirect "Redirect a request."
  [^Channel ch perm ^String targetUrl]
  (let [ rsp (makeFullHttpReply (if perm 301 307)) ]
    (debug "redirecting to -> " targetUrl)
    (HttpHeaders/setHeader rsp  "location" targetUrl)
    (closeCF true (wflush ch rsp))))

(defn makeNilServiceIO "Reify a no-op service-io object."
  ^comzotohcljc.netty.comms.NettyServiceIO
  []
  (reify NettyServiceIO
    (before-send [_ ch msg] (debug "empty before-send." ))
    (onerror [_ ch msginfo err] (debug "empty onerror." ))
    (onreq [_ ch req msginfo xdata] (debug "empty onreq." ))
    (onres [_ ch rsp msginfo xdata] (debug "empty onres." ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server side netty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- genericNettyHandler "Make a generic Netty 4.x Pipeline Handler."
  ^ChannelHandler
  [options]
  (proxy [SimpleChannelInboundHandler] []

    (exceptionCaught [ctx err]
      (let [ ch (.channel ^ChannelHandlerContext ctx)
             attObj (ga-map ch)
             msginfo (:info attObj)
             ucb (:usercb options)
             keepAlive (if (nil? msginfo)
                         false
                         (:keep-alive msginfo)) ]
        (error err "")
        (when-not (nil? ucb)
          (.onerror ^comzotohcljc.netty.comms.NettyServiceIO
                    ucb ch msginfo err))
        (when-not keepAlive (Try! (NetUtils/closeChannel ch)))))

    (channelReadComplete [ctx]
      (.flush ^ChannelHandlerContext ctx))

    (channelRead0 [ctx msg]
      (debug "pipeline-entering with msg: " (type msg))
      (cond
        (instance? LastHttpContent msg)
        (nioChunk ctx msg)

        (instance? HttpRequest msg)
        (nioPRequest ctx
                     msg
                     (:usercb options)
                     (:rtcObj options))

        (instance? WebSocketFrame msg)
        (nioWSFrame ctx msg)

        (instance? HttpResponse msg)
        (nioPRes ctx msg usercb)

        (instance? HttpContent msg)
        (nioChunk ctx msg)

        (nil? msg)
        (throw (IOException. "Got null object."))

        :else
        (throw (IOException. (str "Got object: " (class msg))) )))

    ))

(defn- inizServer "" ^ChannelHandler [options]
  (proxy [ChannelInitializer][]
    (initChannel [^SocketChannel ch]
      (let [ ^ChannelPipeline pl (NetUtils/getPipeline ch)
             kf (:serverKey options)
             pw (:passwd options)
             ssl (if (nil? kf)
                     nil
                     (make-sslContext kf pw))
             eg (if (nil? ssl)
                    nil
                    (doto (.createSSLEngine ssl)
                          (.setUseClientMode false))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
        (.addLast pl "codec" (HttpServerCodec.))
        (.addLast pl "chunker" (ChunkedWriteHandler.))
        (.addLast pl "handler" (genericNettyHandler options))
        pl))))

(defn makeServerNetty ""
  ([] (makeServerNetty {} ))
  ([options]
    (let [ gp (NioEventLoopGroup.) gc (NioEventLoopGroup.)
           bs (doto (ServerBootstrap.)
                    (.group gp gc)
                    (.channel io.netty.channel.socket.nio.NioServerSocketChannel)
                    (.option ChannelOption/SO_REUSEADDR true)
                    (.childOption ChannelOption/SO_RCVBUF (int (* 2 1024 1024)))
                    (.childOption ChannelOption/TCP_NODELAY true))
           cg (DefaultChannelGroup. (uid) GlobalEventExecutor/INSTANCE) ]
      (doseq [ [k v] (seq options) ]
        (if (= :child k)
          (doseq [ [x y] (seq v) ]
            (.childOption bs x y))
          (.option bs k v)))
      (.childHandler bs (inizServer options))
      (comzotohcljc.netty.comms.NettyServer. bs cg)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; client side netty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- inizClient ^ChannelHandler [options]
  (proxy [ChannelInitializer][]
    (initChannel [^SocketChannel ch]
      (let [ ^ChannelPipeline pl (NetUtils/getPipeline ch)
             ssl (make-sslClientCtx  (true? (:ssl options)))
             eg (if (nil? ssl)
                    nil
                    (doto (.createSSLEngine ssl)
                          (.setUseClientMode true))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
        (.addLast pl "codec" (HttpClientCodec.))
        (.addLast pl "chunker" (ChunkedWriteHandler.))
        (.addLast pl "handler" (genericNettyHandler options))
        pl))))

(defn makeClientNetty ""
  ([] (makeClientNetty {}))
  ([options]
   (let [ g (NioEventLoopGroup.)
          bs (doto (Bootstrap.)
                   (.group g)
                   (.channel io.netty.channel.socket.nio.NioSocketChannel)
                   (.option ChannelOption/TCP_NODELAY true)
                   (.option ChannelOption/SO_KEEPALIVE true))
          cg (DefaultChannelGroup. (uid) GlobalEventExecutor/INSTANCE) ]
     (doseq [ [ k v] (seq options) ]
       (.option bs k v))
     (.handler (inizClient options))
     (comzotohcljc.netty.comms.NettyClient bs cg)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contWith100 "Send back 100-continue."
  [^ChannelHandlerContext ctx]
  (-> (.channel ctx) (wflush (makeFullHttpReply 100))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- opDone "" [chOrGroup success error options]
  (cond
    (and (fn? (:nok options)) (not success))
    (apply (:nok options) chOrGroup error)

    (and (fn? (:ok options)) success)
    (apply (:ok options) chOrGroup)

    (fn? (:done options))
    (apply (:done options) chOrGroup)

    :else nil)
  nil)

(defmethod addListener ChannelGroupFuture
  [^ChannelGroupFuture cf options]
  (-> cf (.addListener
           (reify ChannelGroupFutureListener
             (operationComplete [_ cff]
               (opDone (.group cf)
                       (.isSuccess ^ChannelGroupFuture cff)
                       nil
                       options ))))))

(defmethod addListener ChannelFuture
  [^ChannelFuture cf options]
  (-> cf (.addListener
           (reify ChannelFutureListener
             (operationComplete [_ cff]
               (opDone (.channel cf)
                       (.isSuccess ^ChannelFuture cff)
                       (.cause ^ChannelFuture cff)
                       options ))))))

(defn- maybe-keepAlive "Make channel-future listener to close the channel."
  [^ChannelFuture cf keepAlive]
  (when-not keepAlive
    (addListener cf ChannelFutureListener/CLOSE)))

(defn- reply-xxx "" [^Channel ch status]
  (let [ rsp (makeFullHttpReply status)
         info (:info (ga-map ch))
         kalive (if (nil? info) false (:keep-alive info)) ]
    ;;(HttpHeaders/setTransferEncodingChunked res)
    (HttpHeaders/setContentLength rsp 0)
    (-> (wflush ch rsp) (maybe-keepAlive kalive))))

(defn- nioMapHeaders "Turn headers into a clj-map."
  [^HttpMessage msg]
  (let [ hds (.headers msg) ]
    (persistent! (reduce (fn [sum ^String n]
                           (assoc! sum (.toLowerCase n) (vec (.getAll hds n))))
                         (transient {}) (.names hds))) ))

(defn- nioExtractMsg "Get info from http message."
  [^HttpMessage msg]
    { :protocol (-> msg (.getProtocolVersion) (.toString))
      :is-chunked (HttpHeaders/isTransferEncodingChunked msg)
      :keep-alive (HttpHeaders/isKeepAlive msg)
      :clen (HttpHeaders/getContentLength msg 0)
      :headers (nioMapHeaders msg) } )

(defn- nioExtractReq "Extract info from request."
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpRequest req]
  (let [ ws (.toLowerCase (strim (HttpHeaders/getHeader req "upgrade")))
         decr (QueryStringDecoder. (.getUri req))
         md (-> req (.getMethod) (.name))
         uri (.path decr)
         m1 (nioExtractMsg req)
         mo (strim (HttpHeaders/getHeader req "X-HTTP-Method-Override"))
         params (persistent! (reduce (fn [sum en]
                                       (assoc! sum (.toLowerCase (nsb (first en))) (vec (nth en 1))))
                                     (transient {}) (.parameters decr))) ]
    (debug "basic request info: " m1)
    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1)
      (-> (if (= "websocket" ws) "WS" (if (hgl? mo) mo md))
        (.toUpperCase))
      uri
      (:is-chunked m1)
      (:keep-alive m1)
      (:clen m1) (:headers m1) params)))

(defn- nioExtractRes "Extract info from response."
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpResponse res]
  (let [ m1 (nioExtractMsg res) ]
    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1) "" "" (:is-chunked m1)
      (:keep-alive m1) (:clen m1) (:headers m1) {})))

(defn- nioFinz "Close any output stream created during the message handling."
  [^ChannelHandlerContext ctx]
  (let [ ch (.channel ctx) attObj (ga-map ch)
         os (:os attObj) ]
    (IOUtils/closeQuietly ^OutputStream os) ))

(defn- nioComplete "" [^ChannelHandlerContext ctx msg]
  (nioFinz ctx)
  (let [ ch (.channel ctx) attObj (ga-map ch)
         ^XData xdata (:xs attObj)
         info (:info attObj)
         dir (:dir attObj)

         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj)

         os (:os attObj)
         clen (:clen attObj) ]
    (when (and (instance? ByteArrayOutputStream os)
               (> clen 0))
      (.resetContent xdata os))
    (cond
      (instance? HttpResponse dir)
      (do
        (TryC (NetUtils/closeChannel ch))
        (.onres usercb ch dir info xdata))

      (instance? HttpRequest dir)
      (do (.onreq usercb ch dir info xdata))

      (instance? WebSocketFrame msg)
      (do (.onreq usercb ch msg info xdata))

      :else nil)
    ))

(defn- nioSockitDown [^ChannelHandlerContext ctx ^ByteBuf cbuf]
  (let [ ch (.channel ctx) attObj (ga-map ch)
         ^OutputStream cout (:os attObj)
         ^XData xdata (:xs attObj)
         csum (:clen attObj)
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile xdata)
                  cout
                  (NetUtils/swapFileBacked xdata cout nsum)) ]
    (sa-map! ch (-> attObj
                    (assoc :clen nsum)
                    (assoc :os nout))) ))

(defn- nioCfgCtx [^ChannelHandlerContext ctx usercb]
  (let [ att { :os (make-baos)
               :xs (XData.)
               :clen 0
               :cb usercb
               :dir nil
               :info nil } ]
    (sa-map! (.channel ctx) att)
    att))

(defn- nioPError [^ChannelHandlerContext ctx err]
  (let [ ch (.channel ctx) attObj (ga-map ch)
         info (:info attObj)
         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj) ]
    (.onerror usercb ch info err) ))

(defn- nioFrameChunk [^ChannelHandlerContext ctx
                      ^ContinuationWebSocketFrame frame]
  (let [ cbuf (.content frame)
         ch (.channel ctx) ]
    (when-not (nil? cbuf) (nioSockitDown ctx cbuf))
    (when (.isFinalFragment frame)
      (do
        (nioFinz ctx)
        (when (nil? cbuf)
          (let [ s (nsb (.aggregatedText frame))
                 attObj (ga-map ch)
                 xs (:xs attObj) ]
            (sa-map! ch (assoc attObj :clen (.length s)))
            (.resetContent ^XData xs s)))
        (nioComplete ctx frame) )) ))

(defn- getBits [^ChannelHandlerContext ctx ^WebSocketFrame frame]
  (when-let [ buf (.content frame) ]
    (nioSockitDown ctx buf)))

(defn- nioWSFrame [^ChannelHandlerContext ctx ^WebSocketFrame frame ]
  (let [ ch (.channel ctx) attObj (ga-map ch)
         ^XData xs (:xs attObj)
         ^WebSocketServerHandshaker hs (:hs attObj) ]
    (debug "nio-wsframe: received a " (type frame) )
    (cond
      (instance? CloseWebSocketFrame frame)
      (.close hs ch ^CloseWebSocketFrame frame)

      (instance? PingWebSocketFrame frame)
      (wflush ch (PongWebSocketFrame. (-> (.content frame)(.retain))))

      (instance? BinaryWebSocketFrame frame)
      (do
        (getBits ctx frame)
        (nioComplete ctx frame))

      (instance? TextWebSocketFrame frame)
      (let [ s (nsb (.text ^TextWebSocketFrame frame)) ]
        (.resetContent xs s)
        (sa-map! ch (assoc attObj :clen (.length s)))
        (nioComplete ctx frame))

      (instance? ContinuationWebSocketFrame frame)
      (nioFrameChunk ctx frame)

      :else ;; what else can this be ????
      nil) ))

(defn- maybeSSL "" [^ChannelHandlerContext ctx]
  (let [ ssl (-> (NetUtils/getPipeline ctx)
                 (.get (class SslHandler))) ]
    (when-not (nil? ssl)
      (let [ cf (.handshakeFuture ^SslHandler ssl) ]
        (addListener cf
                     { :nok
                       (fn [^Channel c] (NetUtils/closeChannel c)) } )))))

(defn- nioWSock "" [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ ch (.channel ctx) attObj (ga-map ch)
         rts (:rts attObj) ]
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
            (Try! (NetUtils/closeChannel ch)))
          (do
            (sa-map! ch (assoc attObj :hs hs))
            (addListener (.handshake hs ch req)
                          { :nok (fn [^Channel c ^Throwable e]
                                   (-> (NetUtils/getPipeline c) (.fireExceptionCaught e)))
                            :ok (fn [c] (maybeSSL ctx)) } ))))) ))

(defn- nioWReq [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ ch (.channel ctx) attObj (ga-map ch)
         msginfo (:info attObj)
         rts (:rts attObj) ]
    (debug "nioWReq: received a " (:method msginfo ) " request from " (:uri msginfo))
    ;; if it's a valid route, continue as usual
    (if (true? rts)
      (do
        (when (HttpHeaders/is100ContinueExpected req) (contWith100 ctx))
        (if (:is-chunked msginfo)
          (debug "nioWReq: request is chunked.")
          ;; msg not chunked, suck all data from the request
          (when (instance? ByteBufHolder  req)
            (nioSockitDown ctx (.content ^ByteBufHolder req)))))
      ;; bad route, so just pass it down and let it handle the error.
      (nioComplete ctx req))))

(defn- nioPRequest "handle a request"
  [^ChannelHandlerContext ctx
   ^HttpRequest req
   usercb
   ^comzotohcljc.netty.comms.RouteCracker rtcObj]
  (nioCfgCtx ctx usercb)
  (let [ msginfo (nioExtractReq req)
         ch (.channel ctx)
         attObj (ga-map ch)
         rts (if (and (not (nil? rtcObj))
                      (.hasRoutes? rtcObj))
                (.routable? rtcObj msginfo)
                true) ]
    (sa-map! ch (-> attObj
                    (assoc :info msginfo)
                    (assoc :rts rts)
                    (assoc :dir req)))
    (if (= "WS" (:method msginfo))
        (nioWSock ctx req)
        (nioWReq ctx req))))

(defn- nioRedirect "" [^ChannelHandlerContext ctx msg]
  ;; TODO: handle redirect properly, for now, same as error
  (nioPError ctx (IOException. "Unsupported redirect.")))

(defn- nioPResBody "" [^ChannelHandlerContext ctx ^HttpResponse res]
  (if (HttpHeaders/isTransferEncodingChunked res)
    (debug "nioPResBody: response is chunked.")
    (when (instance? ByteBufHolder  res)
      (nioSockitDown ctx (.content ^ByteBufHolder res))) ))

(defn- nioPRes "handle a response"
  [^ChannelHandlerContext ctx ^HttpResponse res usercb]
  (nioCfgCtx ctx usercb)
  (let [ msginfo (nioExtractRes res)
         ch (.channel ctx)
         attObj (ga-map ch)
         s (.getStatus res)
         r (.reasonPhrase s)
         c (.code s) ]
    (debug "nioPRes: got a response: code " c " reason: " r)
    (sa-map! ch (-> attObj
                    (assoc :info msginfo)
                    (assoc :dir res)
                    (assoc :rts false)))
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

(defn nettyXXXServer "Make a Netty server."
  ^NettyServer
  [^String host
   port
   ^comzotohcljc.netty.comms.NettyServiceIO
   usercb
   options]

  (let [ ^URL keyUrl (:serverkey options)
         pwdObj (:passwd options)
         ssl (if (nil? keyUrl) nil (make-sslContext keyUrl pwdObj))
         ^ServerBootstrap bs (nioServerBootstrap options)
         cg (makeChannelGroup) ]
     (.childHandler bs (makeServerPipe ssl usercb nil))
     (.add cg (-> bs
                (.bind (InetSocketAddress. host (int port)))
                (.sync)
                (.channel)))
     (debug "netty-xxx-server: running on host " host ", port " port)
     (NettyServer. bs cg) ))

(defn makeMemHttpd "Make an in-memory http server."

  [^String host
   port
   ^comzotohcljc.netty.comms.NettyServiceIO
   usercb
   options ]

  (nettyXXXServer host port usercb options ))

(defn- reply-get-vfile [^Channel ch ^XData xdata]
  (let [ res (makeHttpReply 200)
         clen (.size xdata)
         attObj (ga-map ch)
         info (:info attObj)
         kalive (if (nil? info) false (:keep-alive info)) ]
    (HttpHeaders/setHeader res "content-type" "application/octet-stream")
    (HttpHeaders/setContentLength res clen)
    (HttpHeaders/setTransferEncodingChunked res)
    (wwrite ch res)
    (wwrite ch (ChunkedStream. (.stream xdata)))
    (-> (wflush ch LastHttpContent/EMPTY_LAST_CONTENT)
      (maybe-keepAlive kalive))))

(defn- filer-handler [^File vdir]
  (let [ putter (fn [^Channel ch ^String fname ^XData xdata]
                  (try
                      (save-file vdir fname xdata)
                      (reply-xxx ch 200)
                    (catch Throwable e#
                      (reply-xxx ch 500))))
         getter (fn [^Channel ch ^String fname]
                  (let [ xdata (get-file vdir fname) ]
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
        (let [ mtd (nsb (:method msginfo))
               uri (nsb (:uri msginfo))
               pos (.lastIndexOf uri (int \/))
               p (if (< pos 0) uri (.substring uri (inc pos))) ]
          (debug "Method = " mtd ", Uri = " uri ", File = " p)
          (cond
            (or (= mtd "POST")(= mtd "PUT")) (putter ^Channel ch p xdata)
            (= mtd "GET") (getter ^Channel ch p)
            :else (reply-xxx ch 405)))))
      ))

(defn makeMemFileSvr "A file server which can get/put files."
  [^String host port options]
  (nettyXXXServer host port (filer-handler (:vdir options)) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http client functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- connectClient "Make a Netty 4.x client."

  ([^URL targetUrl] (connectClient {}))
  ([^URL targetUrl options]
   (let [ opts (merge { :usercb (makeNilServiceIO)
                        :targetUrl targetUrl }
                      options)
          ssl (= (.getProtocol targetUrl) "https")
          pnum (.getPort targetUrl)
          port (if (< pnum 0) (if ssl 443 80) pnum)
          host (.getHost targetUrl)
          sock (InetSocketAddress. host port)
          ucb (:usercb opts)
          nc (makeClientNetty opts)
          ^ChannelFuture cf (-> (.client nc)
                                (.connect sock)
                                (.sync))
          ok (.isSuccess cf)
          ch (if ok (.channel cf))
          e (if (not ok) (.cause cf)) ]
     (when-not ok
       (if (nil? e)
           (throw (IOException. "Failed to connect to URL: " targetUrl))
           (throw e)))
     (.add (.cgroup nc) (.channel cf))
     (debug "Netty client connected to " host ":" port " - OK.")
     [nc ch opts] )))

(defn- send-httpClient

  [^NettyClient clientr ^Channel ch ^XData xdata options]

  (let [ clen (if (nil? xdata) 0 (.size xdata))
         ^URL url (:taretUrl options)
         req (DefaultHttpRequest.  (HttpVersion/HTTP_1_1)
               (if (> clen 0) HttpMethod/POST HttpMethod/GET)
               (.toString url))
         ka (:keepAlive options)
         ucb (:usercb options) ]
    (HttpHeaders/setHeader req "Connection" (if ka "keep-alive" "close"))
    (HttpHeaders/setHeader req "host" (.getHost url))
    (.before-send ucb req)
    (let [ ct (HttpHeaders/getHeader req "content-type") ]
      (when (and (StringUtils/isEmpty ct)
                 (> clen 0))
        (HttpHeaders/setHeader req "content-type" "application/octet-stream")) )
    (when (> clen 0)
      (do
        (debug "Netty client: content has length " clen)
        (HttpHeaders/setContentLength req clen)))
    (debug "Netty client: about to flush out request (headers)")
    (wwrite ch req)
    (when (> clen 0)
      (if (> clen (com.zotoh.frwk.io.IOUtils/streamLimit))
          (wwrite ch (ChunkedStream. (.stream xdata)))
          (wwrite ch (Unpooled/wrappedBuffer (.javaBytes xdata)))))
    (let [ ^ChannelFuture wf (wflush ch LastHttpContent/EMPTY_LAST_CONTENT) ]
      (closeCF (:keepAlive options) wf))

    ))

(defn asyncPost ""
  ([^URL targetUrl ^XData xdata] (asyncPost targetUrl xdata {}))
  ([^URL targetUrl ^XData xdata options]
   (send-httpClient
     (iniz-httpClient clientr targetUrl serviceIO)
     clientr
     xdata
     { :url (.toString targetUrl)
       :before-send (:before-send serviceIO) } )))

(defn asyncGet ""
  ([^URL targetUrl] (asyncGet targetUrl {}))
  ([^URL targetUrl options]
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
(defn- seek-route [mtd uri rts]
  (if-not (nil? rts)
    (some (fn [^comzotohcljc.net.rts.RouteInfo ri]
            (let [ m (.resemble? ri mtd uri) ]
              (if (nil? m) nil [ri m])))
          (seq rts)) ))

(defn makeRouteCracker "Create a url route cracker."
  ^comzotohcljc.netty.comms.RouteCracker
  [routes]
  (reify RouteCracker
    (hasRoutes? [_] (> (count routes) 0))
    (routable? [this msgInfo]
      (first (crack this msgInfo)))
    (crack [_ msgInfo]
      (let [ ^String mtd (:method msgInfo)
             ^String uri (:uri msgInfo)
             rc (seek-route mtd uri routes)
             rt (if (nil? rc)
                  [false nil nil ""]
                  [true (first rc)(last rc) ""] ) ]
        (if (and (false? (nth rt 0))
                 (not (.endsWith uri "/"))
                 (seekRoute mtd (str uri "/") routes))
          [true nil nil (str uri "/")]
          rt)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private comms-eof nil)


