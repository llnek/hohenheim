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
(import '(java.nio ByteBuffer))
(import '(java.util HashMap))
(import '(java.net URI URL InetSocketAddress))
(import '(java.util.concurrent Executors))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(javax.net.ssl X509TrustManager TrustManager))
(import '(org.jboss.netty.bootstrap Bootstrap ClientBootstrap ServerBootstrap))
(import '(org.jboss.netty.buffer ChannelBufferInputStream ChannelBuffer))
(import '(org.jboss.netty.channel
  ChannelHandlerContext Channel ExceptionEvent MessageEvent
  ChannelFutureListener Channels SimpleChannelHandler ChannelFuture
  ChannelPipelineFactory ChannelPipeline))
(import '(org.jboss.netty.buffer ByteBufferBackedChannelBuffer))
(import '(org.jboss.netty.channel.group
  ChannelGroupFuture ChannelGroup ChannelGroupFutureListener
  DefaultChannelGroup))
(import '(org.jboss.netty.channel.socket.nio
  NioClientSocketChannelFactory NioServerSocketChannelFactory))
(import '(org.jboss.netty.handler.codec.http HttpChunkAggregator DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion HttpChunk
  HttpHeaders$Values HttpHeaders$Names
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse QueryStringDecoder HttpMethod
  HttpRequestDecoder HttpServerCodec HttpClientCodec
  HttpResponseEncoder))
(import '(org.jboss.netty.handler.ssl SslHandler))
(import '(org.jboss.netty.handler.stream ChunkedStream ChunkedWriteHandler))
(import '(org.jboss.netty.handler.codec.http.websocketx
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
(require '[comzotohcljc.util.files :as FU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.util.core :as CU])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(comment
(def HTTP-CODES
  (let [fields (:fields (bean HttpResponseStatus))
        to-key (comp int (fn [^Field f] (.getCode ^HttpResponseStatus (.get f nil))))
        kkeys (map to-key fields)
        vvals (map (comp str (fn [^Field f] (.get f nil))) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map of { int (code) -> HttpResponseStatus }
(def HTTP-CODES
  (let [fields (:fields (bean HttpResponseStatus))
        to-key (fn [^Field f] (.getCode ^HttpResponseStatus (.get f nil)))
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
  (onerror [_ ^Channel ch msginfo ^MessageEvent evt] )
  (onreq [_ ^Channel ch req msginfo ^XData xdata] )
  (onres [_ ^Channel ch rsp msginfo ^XData xdata] ))

(defn make-resp-status "Make a http response object."

  (^HttpResponse [] (make-resp-status 200))

  (^HttpResponse [status]
    (DefaultHttpResponse. HttpVersion/HTTP_1_1
                          (get HTTP-CODES status))))

(defn chKeepAlive?  "True if keep connection alive." [^HttpMessage msg]
  (HttpHeaders/isKeepAlive msg))

(defn msgSize "Get message's content-length." [^HttpMessage msg]
  (HttpHeaders/getContentLength msg))

(defn closeCF "Maybe close the channel." [doit ^ChannelFuture cf]
  (if (and doit (CU/notnil? cf))
    (.addListener cf ChannelFutureListener/CLOSE)))

(defn sendRedirect "" [ ^Channel ch perm ^String targetUrl]
  (let [ rsp (make-resp-status (if perm 301 307)) ]
    (debug "redirecting to " targetUrl)
    (.setHeader rsp "location" targetUrl)
    (closeCF true (.write ch rsp))))

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
    (let [ bs (ServerBootstrap. (NioServerSocketChannelFactory.
                (Executors/newCachedThreadPool)
                (Executors/newCachedThreadPool)))
           dft { :child.receiveBufferSize (int (* 2 1024 1024))
                 :child.tcpNoDelay true
                 :reuseAddress true }
           opts (merge dft options) ]
      (doseq [ [k v] (seq opts) ]
        (.setOption bs (name k) v))
      [bs opts])) )

(defn client-bootstrap "Make a netty client bootstrap."

  ([] (client-bootstrap {} ))

  ([options]
    (let [ bs (ClientBootstrap. (NioClientSocketChannelFactory.
                (Executors/newCachedThreadPool)
                (Executors/newCachedThreadPool)))
           dft { :tcpNoDelay true
                 :keepAlive true }
           opts (merge dft options) ]
      (doseq [ [ k v] (seq opts) ]
        (.setOption bs (name k) v))
      [bs opts])) )


(defn channel-group "Make a channel group."

  ^ChannelGroup
  []
  (DefaultChannelGroup. (CU/uid)))

(defn kill-9 "Clean up resources used by a netty server." [^ServerBootstrap bs]
    (CU/Try! (.releaseExternalResources bs)))

(defn finz-server "Bring down a netty server."

  [ {^ServerBootstrap server :server ^ChannelGroup cg :cgroup } ]
  (if (nil? cg)
    (kill-9 server)
    (-> (.close cg) (add-listener { :done (fn [_] (kill-9 server)) }))))

(defn send-100-cont "" [^ChannelHandlerContext ctx]
  (let [ ch (.getChannel ctx) ]
    (.write ch (make-resp-status 100))))

(defrecord NettyServer [^ServerBootstrap server ^ChannelGroup cgroup options] )
(defrecord NettyClient [^ClientBootstrap client ^ChannelGroup cgroup options] )

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
  (let [ cg (.getGroup cf) ]
    (-> cf (.addListener
              (reify ChannelGroupFutureListener
                (operationComplete [_ cff]
                  (opComplete cg
                               (.isCompleteSuccess ^ChannelGroupFuture cff)
                               nil
                               options )))))) )

(defmethod ^:private add-listener ChannelFuture
  [^ChannelFuture cf options]
  (let [ ch (.getChannel cf) ]
    (-> cf (.addListener
              (reify ChannelFutureListener
                (operationComplete [_ cff]
                  (opComplete ch
                               (.isSuccess ^ChannelFuture cff)
                               (.getCause ^ChannelFuture cff)
                               options )))))) )

(defn- maybe-keepAlive "make channel-future listener to close the channel"
  [^ChannelFuture cf keepAlive]
  (when-not keepAlive
    (add-listener cf
                  { :done
                    (fn [^ChannelFuture cff] (.close (.getChannel cff))) } )))

(defn- nioMapHeaders "turn headers into a clj-map"
  [^HttpMessage msg]
  (persistent! (reduce (fn [sum ^String n]
            (assoc! sum (.toLowerCase n) (vec (.getHeaders msg n))))
    (transient {}) (seq (.getHeaderNames msg)))) )

(defn- nioExtractMsg "get info from http message"
  [^HttpMessage msg]
    { :protocol (-> msg (.getProtocolVersion) (.toString))
      :is-chunked (.isChunked msg)
      :keep-alive (chKeepAlive? msg)
      :clen (msgSize msg)
      :headers (nioMapHeaders msg) } )

(defn- nioExtractReq "extract info from request"
  ^comzotohcljc.net.comms.HTTPMsgInfo
  [^HttpRequest req]
  (let [ ws (.toLowerCase (SU/strim (.getHeader req "upgrade")))
         decr (QueryStringDecoder. (.getUri req))
         md (.toUpperCase (-> req (.getMethod) (.getName)))
         uri (.getPath decr)
         m1 (nioExtractMsg req)
         params (persistent! (reduce (fn [sum en]
                          (assoc! sum (.toLowerCase (SU/nsb (first en))) (vec (nth en 1))))
                      (transient {}) (seq (.getParameters decr)))) ]

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
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         ^XData xdata (:xs attObj)
         info (:info attObj)
         dir (:dir attObj)

         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj)

         os (:os attObj)
         clen (:clen attObj) ]
    (when (and (> clen 0) (instance? ByteArrayOutputStream os))
      (.resetContent xdata os))
    (cond
      (instance? HttpResponse dir) (do
                   (CU/TryC (.close ch))
                   (.onres usercb ch dir info xdata))
      (instance? HttpRequest dir) (do
                  (.onreq usercb ch dir info xdata))
      (instance? WebSocketFrame msg) (do
                  (.onreq usercb ch msg info xdata))
      :else nil)
    ))

(defn- nioSockitDown

  [^ChannelHandlerContext ctx ^ChannelBuffer cbuf]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         ^XData xdata (:xs attObj)
         ^OutputStream cout (:os attObj)
         csum (:clen attObj)
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile xdata)
                  cout
                  (NetUtils/swapFileBacked xdata cout nsum)) ]
    (.setAttachment ch (merge attObj { :os nout :clen nsum } ))))

(defn- nioCfgCtx [^ChannelHandlerContext ctx atts usercb]
  (let [ os (IO/make-baos)
         x (XData.)
         clen 0
         m (merge { :xs x :os os :clen clen :cb usercb } atts) ]
    (.setAttachment (.getChannel ctx) m)
    m))

(defn- nioPError [^ChannelHandlerContext ctx ^MessageEvent ev]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj) ]
    (.onerror usercb ch (:info attObj) ev) ))

(defn- nioFinz "close any output stream created during the message handling"
  [^ChannelHandlerContext ctx]
  (let [ ch (.getChannel ctx)
         att (.getAttachment ch)
         ^OutputStream os (:os att) ]
    (IOUtils/closeQuietly os)))

(defn- nioFrameChunk [^ChannelHandlerContext ctx ^ContinuationWebSocketFrame frame]
  (let [ cbuf (.getBinaryData frame) ]
    (when-not (nil? cbuf) (nioSockitDown ctx cbuf))
    (when (.isFinalFragment frame)
      (do
        (nioFinz ctx)
        (when (nil? cbuf)
          (let [s (SU/nsb (.getAggregatedText frame))
                ch (.getChannel ctx)
                attObj (.getAttachment ch)
                ^XData xs (:xs attObj) ]
            (.setAttachment ch (merge attObj { :clen (.length s) } ))
            (.resetContent xs s)))
        (nioComplete ctx frame) )) ))

(defn- getBits [^ChannelHandlerContext ctx ^WebSocketFrame frame]
  (when-let [ buf (.getBinaryData frame) ]
    (nioSockitDown ctx buf)))

(defn- nioWSFrame [^ChannelHandlerContext ctx ^WebSocketFrame frame ]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         ^XData xs (:xs attObj)
         ^WebSocketServerHandshaker hs (:hs attObj) ]
    (debug "nio-wsframe: received a " (class frame) )
    (cond
      (instance? CloseWebSocketFrame frame)
      (.close hs ch ^CloseWebSocketFrame frame)

      (instance? PingWebSocketFrame frame)
      (.write ch (PongWebSocketFrame. (.getBinaryData frame)))

      (instance? BinaryWebSocketFrame frame)
      (do
        (getBits ctx frame)
        (nioComplete ctx frame))

      (instance? TextWebSocketFrame frame)
      (let [ s (SU/nsb (.getText ^TextWebSocketFrame frame)) ]
        (.resetContent xs s)
        (.setAttachment ch (merge attObj { :clen (.length s) }))
        (nioComplete ctx frame))

      (instance? ContinuationWebSocketFrame frame)
      (nioFrameChunk ctx frame)

      :else ;; what else can this be ????
      nil) ))


(defn- maybeSSL "" [^ChannelHandlerContext ctx]
  (let [ ssl (-> ctx (.getPipeline) (.get (class SslHandler))) ]
    (when-not (nil? ssl)
      (let [ cf (.handshake ^SslHandler ssl) ]
        (add-listener cf { :nok (fn [^Channel c] (.close c)) } )))))

(defn- nioWSock "" [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ wf (WebSocketServerHandshakerFactory.
                     (str "ws://" (.getHeader req "host") (.getUri req)) nil false)
         ch (.getChannel ctx)
         attObj (.getAttachment ch)
         hs (.newHandshaker wf req) ]
    (if (nil? hs)
      (do
        (.sendUnsupportedWebSocketVersionResponse wf ch)
        (CU/Try! (.close ch)))
      (do
        (.setAttachment ch (merge attObj { :hs hs }))
        (add-listener (.handshake hs ch req)
                      { :nok (fn [^Channel c ^Throwable e]
                               (Channels/fireExceptionCaught c e))
                        :ok (fn [c] (maybeSSL ctx)) } )))))

(defn- nioWReq [^ChannelHandlerContext ctx ^HttpRequest req]
  (let [ attObj (.getAttachment (.getChannel ctx))
         msginfo (:info attObj) ]
    (debug "nioWReq: received a " (:method msginfo ) " request from " (:uri msginfo))
    (when (HttpHeaders/is100ContinueExpected req) (send-100-cont ctx))
    (if (:is-chunked msginfo)
      (debug "nioWReq: request is chunked.")
      ;; msg not chunked, suck all data from the request
      (do (try
              (nioSockitDown ctx (.getContent req))
            (finally (nioFinz ctx)))
        (nioComplete ctx req)))))

(defn- nioPRequest "handle a request" [^ChannelHandlerContext ctx ^HttpRequest req usercb]
  (let [ msginfo (nioExtractReq req)
         attObj (nioCfgCtx ctx { :dir req :info msginfo } usercb) ]
    (if (= "WS" (:method msginfo))
      (nioWSock ctx req)
      (nioWReq ctx req))))

(defn- nioRedirect "" [^ChannelHandlerContext ctx ^MessageEvent ev]
  ;; TODO: handle redirect properly, for now, same as error
  (nioPError ctx ev))

(defn- nioPResBody "" [^ChannelHandlerContext ctx ^HttpResponse res]
  (if (.isChunked res)
    (debug "nioPResBody: response is chunked.")
    (try
        (nioSockitDown ctx (.getContent res))
      (finally (nioFinz ctx)))))

(defn- nioPRes "handle a response"
  [^ChannelHandlerContext ctx ^MessageEvent ev ^HttpResponse res usercb]
  (let [ msginfo (nioExtractRes res)
         s (.getStatus res)
         r (.getReasonPhrase s)
         c (.getCode s) ]
    (debug "nioPRes: got a response: code " c " reason: " r)
    (nioCfgCtx ctx { :dir res :info msginfo } usercb)
    (cond
      (and (>= c 200) (< c 300)) (nioPResBody ctx res)
      (and (>= c 300) (< c 400)) (nioRedirect ctx ev)
      :else (nioPError ctx ev))))

(defn- nioChunk "handle a chunk - part of a message"
  [^ChannelHandlerContext ctx ^HttpChunk msg]
  (let [ done (try
                  (nioSockitDown ctx (.getContent msg))
                  (.isLast msg)
                (catch Throwable e#
                    (do (nioFinz ctx) (throw e#)))) ]
    (if done (nioComplete ctx msg) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make our channel handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn netty-pipe-handler "Make a generic Netty 3.x Pipeline Handler."
  [^comzotohcljc.netty.comms.NettyServiceIO usercb]
  (proxy [SimpleChannelHandler] []

    (exceptionCaught [ctx ev]
      (let [ ch (.getChannel ^ChannelHandlerContext ctx)
             attObj (.getAttachment ch)
             msginfo (:info attObj)
             keepAlive (if (nil? msginfo) false (:keep-alive msginfo)) ]
        (error (.getCause ^ExceptionEvent ev) "")
        (.onerror usercb ch msginfo ev)
        (when-not keepAlive (CU/Try! (.close ch)))))

    (messageReceived [ctx ev]
      (let [ msg (.getMessage ^MessageEvent ev) ]
        (cond
          (instance? HttpRequest msg) (nioPRequest ctx msg usercb)
          (instance? WebSocketFrame msg) (nioWSFrame ctx msg)
          (instance? HttpResponse msg) (nioPRes ctx ev msg usercb)
          (instance? HttpChunk msg) (nioChunk ctx msg)
          :else (throw (IOException. "Received some unknown object." ) ))))

    ))

(defn make-pipeServer "Make a Serverside PipelineFactory." ^ChannelPipelineFactory [^SSLContext sslctx usercb]
  (reify ChannelPipelineFactory
    (getPipeline [_]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline)
             eg (if (nil? sslctx)
                    nil
                    (doto (.createSSLEngine sslctx)(.setUseClientMode false))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
        (doto pl
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
          (.addLast "codec" (HttpServerCodec.))
          (.addLast "chunker" (ChunkedWriteHandler.))
          (.addLast "handler" (netty-pipe-handler usercb))) ))))

(defn make-pipeClient "Make a Clientside PipelineFactory" ^ChannelPipelineFactory [^SSLContext sslctx usercb]
  (reify ChannelPipelineFactory
    (getPipeline [_]
      (let [ pl (org.jboss.netty.channel.Channels/pipeline)
             eg (if (nil? sslctx)
                    nil
                    (doto (.createSSLEngine sslctx)(.setUseClientMode true))) ]
        (when-not (nil? eg) (.addLast pl "ssl" (SslHandler. eg)))
        (doto pl
          ;;(.addLast "decoder" (HttpRequestDecoder.))
          ;;(.addLast "encoder" (HttpResponseEncoder.))
          (.addLast "codec" (HttpClientCodec.))
          (.addLast "chunker" (ChunkedWriteHandler.))
          (.addLast "handler" (netty-pipe-handler usercb))) ))))

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
         rc (server-bootstrap options)
         ^ServerBootstrap bs (first rc)
         cg (channel-group)
         pf (make-pipeServer ssl usercb) ]
     (.setPipelineFactory bs pf)
     (.add cg (.bind bs (InetSocketAddress. host (int port))))
     (debug "netty-xxx-server: running on host " host ", port " port)
     (NettyServer. bs cg (last rc)) ))

(defn make-mem-httpd "Make an in-memory http server."

  [^String host
   port
   ^comzotohcljc.netty.comms.NettyServiceIO
   usercb
   options ]

  (netty-xxx-server host port usercb options ))

(defn- reply-xxx [^Channel ch status]
  (let [ res (make-resp-status status)
         attObj (.getAttachment ch)
         info (if (nil? attObj) nil (:info attObj))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (doto res
      (.setChunked false)
      (.setHeader "content-length", "0"))
    (-> (.write ch res) (maybe-keepAlive kalive))))

(defn- reply-get-vfile [^Channel ch ^XData xdata]
  (let [ res (make-resp-status)
         clen (.size xdata)
         attObj (.getAttachment ch)
         info (if (nil? attObj) nil (:info attObj))
         kalive (if (nil? info) false (:keep-alive info)) ]
    (doto res
      (.setHeader "content-type" "application/octet-stream")
      (.setHeader "content-length" (SU/nsb clen)))
    (.write ch res)
    (-> (.write ch (ChunkedStream. (.stream xdata)))
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
      (onerror [_ ch msginfo evt]
        (do
          (if (instance? ExceptionEvent evt) (error (.getCause ^ExceptionEvent evt) ""))
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
    (let [ rc (client-bootstrap options)
           cg (channel-group) ]
      (NettyClient. (first rc) cg (last rc)))) )

(defn- iniz-httpClient

  ([clientr ^URL targetUrl]
   (iniz-httpClient clientr targetUrl (make-nilServiceIO) ))

  ([clientr ^URL targetUrl serviceIO]
    (let [ ctx (CS/make-sslClientCtx (= "https" (.getScheme (.toURI targetUrl))))
           ^String host (.getHost targetUrl)
           pnum (.getPort targetUrl)
           ^long port (if (< pnum 0) (if (nil? ctx) 80 443) pnum)
           pl (make-pipeClient ctx serviceIO)
           ^ClientBootstrap cli (:client clientr)
           ^ChannelGroup cg (:cgroup clientr) ]
      (debug "Netty client connecting to " host ":" port)
      (.setPipelineFactory cli pl)
      (let [ ^ChannelFuture cf (doto (.connect cli
                                               (InetSocketAddress. host port))
                  (.awaitUninterruptibly))
             ok (.isSuccess cf)
             e (if (not ok) (.getCause cf) nil) ]
        (when-not ok
          (if (nil? e)
            (throw (IOException. "Failed to connect to URL: " targetUrl))
            (throw e)))
        (let [ ch (.getChannel cf) ]
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
    (doto req
      (.setHeader "Connection" (if ka "keep-alive" "close"))
      (.setHeader "host" (.getHost (URL. uri))))
    (when (fn? before-send) (before-send req))
    (let [ ct (.getHeader req "content-type") ]
      (when (and (StringUtils/isEmpty ct) (not (nil? xdata)))
        (.setHeader req "content-type" "application/octet-stream")) )
    (when (> clen 0)
      (do
        (debug "Netty client: content has length " clen)
        (.setHeader req "content-length" (str "" clen))))
    (debug "Netty client: about to flush out request (headers)")
    (let [ ^ChannelFuture cf (doto (.write ch req)
                (add-listener { :ok #(do (debug "Netty client: headers flushed")) })) ]
      (when (> clen 0)
        (doto
          (if (> clen (com.zotoh.frwk.io.IOUtils/streamLimit))
            (.write ch (ChunkedStream. (.stream xdata)))
            (.write ch (ByteBufferBackedChannelBuffer.
                         (ByteBuffer/wrap (.javaBytes xdata)))))
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


(def ^:private comms-eof nil)


