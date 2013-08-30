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
(import '(org.apache.commons.io FileUtils))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io IOException ByteArrayOutputStream File OutputStream InputStream))
(import '(java.nio ByteBuffer))
(import '(java.util HashMap))
(import '(java.net URI URL InetSocketAddress))
(import '(java.util.concurrent Executors))
(import '(javax.net.ssl SSLEngine SSLContext))
(import '(javax.net.ssl X509TrustManager TrustManager))
(import '(org.jboss.netty.bootstrap Bootstrap ClientBootstrap ServerBootstrap))
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
(import '(org.apache.commons.io IOUtils))

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
(def ^:dynamic *HTTP-CODES*
  (let [fields (:fields (bean HttpResponseStatus))
        to-key (comp int (fn [^Field f] (.getCode ^HttpResponseStatus (.get f nil))))
        kkeys (map to-key fields)
        vvals (map (comp str (fn [^Field f] (.get f nil))) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map of { int (code) -> HttpResponseStatus }
(def ^:dynamic *HTTP-CODES*
  (let [fields (:fields (bean HttpResponseStatus))
        to-key (fn [^Field f] (.getCode ^HttpResponseStatus (.get f nil)))
        kkeys (map to-key fields)
        vvals (map (fn [^Field f] (.get f nil)) fields) ]
    (into {} (map vec (partition 2 (interleave kkeys vvals))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main netty classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private add-listener (fn [a & more ] (class a)))

(defprotocol NettyServiceIO ""
  (before-send [_ ^Channel ch msg] )
  (onerror [_ ^Channel ch msginfo ^MessageEvent evt] )
  (onreq [_ ^Channel ch req msginfo ^XData xdata] )
  (onres [_ ^Channel ch rsp msginfo ^XData xdata] ))

(defn chKeepAlive [^HttpMessage msg]
  (HttpHeaders/isKeepAlive msg))

(defn closeCF [doit ^ChannelFuture cf]
  (if (and doit (CU/notnil? cf))
    (.addListener cf ChannelFutureListener/CLOSE)))

(defn sendRedirect [ ^Channel ch perm ^String targetUrl]
  (let [ rsp (DefaultHttpResponse. HttpVersion/HTTP_1_1
                                   (if perm HttpResponseStatus/MOVED_PERMANENTLY
                                     HttpResponseStatus/TEMPORARY_REDIRECT)) ]
    (debug "redirecting to " targetUrl)
    (.setHeader rsp "location" targetUrl)
    (closeCF true (.write ch rsp))))

(defn make-nilServiceIO ""

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


(defn kill-9 [^ServerBootstrap bs]
    (CU/Try! (.releaseExternalResources bs)))


(defn finz-server "Bring down a netty server."

  [ {^ServerBootstrap server :server ^ChannelGroup cg :cgroup } ]
  (if (nil? cg)
    (kill-9 server)
    (-> (.close cg) (add-listener { :done (fn [_] (kill-9 server)) }))))

(defn make-resp-status ""

  (^HttpResponse [] (make-resp-status 200))

  (^HttpResponse [status]
    (DefaultHttpResponse. HttpVersion/HTTP_1_1
                          (get *HTTP-CODES* status))))

(defn send-100-cont [^ChannelHandlerContext ctx]
  (let [ ch (.getChannel ctx) ]
    (.write ch (make-resp-status 100))))

(defrecord NettyServer [^ServerBootstrap server ^ChannelGroup cgroup options] )
(defrecord NettyClient [^ClientBootstrap client ^ChannelGroup cgroup options] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- op-complete [chOrGroup success options]
  (cond
    (fn? (:done options))
    (apply (:done options) chOrGroup)

    (and (fn? (:nok options)) (not success ))
    (apply (:nok options) chOrGroup)

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
                  (op-complete cg (.isCompleteSuccess ^ChannelGroupFuture cff) options )))))) )

(defmethod ^:private add-listener ChannelFuture

  [^ChannelFuture cf options]
  (let [ ch (.getChannel cf) ]
    (-> cf (.addListener
              (reify ChannelFutureListener
                (operationComplete [_ cff]
                  (op-complete ch (.isSuccess ^ChannelFuture cff) options )))))) )

;; make channel-future listener to close the channel
(defn- maybe-keepAlive [^ChannelFuture cf keepAlive]
  (when-not keepAlive
    (add-listener cf
                  { :done
                    (fn [^ChannelFuture cff] (.close (.getChannel cff))) } )))

;; turn headers into a clj-map
(defn- nio-mapheaders [^HttpMessage msg]
  (persistent! (reduce (fn [sum ^String n]
            (assoc! sum (.toLowerCase n) (vec (.getHeaders msg n))))
    (transient {}) (seq (.getHeaderNames msg)))) )

;; get info from http message
(defn- nio-extract-msg [^HttpMessage msg]
  (let [ pn (-> msg (.getProtocolVersion) (.toString))
         clen (HttpHeaders/getContentLength msg)
         kalive (HttpHeaders/isKeepAlive msg)
         chunked (.isChunked msg)
         headers (nio-mapheaders msg) ]

    { :protocol pn
      :is-chunked chunked
      :keep-alive kalive
      :clen clen
      :headers headers } ))

;; extract info from request
(defn- nio-extract-req [^HttpRequest req]
  (let [ decr (QueryStringDecoder. (.getUri req))
         md (-> req (.getMethod) (.getName))
         uri (.getPath decr)
         m1 (nio-extract-msg req)
         params (persistent! (reduce (fn [sum en]
                          (assoc! sum (.toLowerCase (SU/nsb (first en))) (vec (nth en 1))))
                      (transient {}) (seq (.getParameters decr)))) ]

    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1) md uri (:is-chunked m1)
      (:keep-alive m1) (:clen m1) (:headers m1) params)))

;; extract info from response
(defn- nio-extract-res [^HttpResponse res]
  (let [ m1 (nio-extract-msg res) ]
    (comzotohcljc.net.comms.HTTPMsgInfo.
      (:protocol m1) "" "" (:is-chunked m1)
      (:keep-alive m1) (:clen m1) (:headers m1) {})))

(defn- nio-pcomplete
  [^ChannelHandlerContext ctx ^HttpMessage msg]
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
      :else nil)
    ))

(defn- nio-sockit-down

  [^ChannelHandlerContext ctx ^HttpMessage msg]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         ^ChannelBuffer cbuf (.getContent msg)
         ^XData xdata (:xs attObj)
         ^OutputStream cout (:os attObj)
         csum (:clen attObj)
         nsum (NetUtils/sockItDown cbuf cout csum)
         nout (if (.isDiskFile xdata)
                  cout
                  (NetUtils/swapFileBacked xdata cout nsum)) ]
    (.setAttachment ch (merge attObj { :os nout :clen nsum } ))))

(defn- nio-cfgctx [^ChannelHandlerContext ctx atts usercb]
  (let [ os (IO/make-baos)
         x (XData.)
         clen 0
         m (merge { :xs x :os os :clen clen :cb usercb } atts) ]
    (.setAttachment (.getChannel ctx) m)
    m))

(defn- nio-perror [^ChannelHandlerContext ctx ^MessageEvent ev]
  (let [ ch (.getChannel ctx)
         attObj (.getAttachment ch)
         ^comzotohcljc.netty.comms.NettyServiceIO
         usercb (:cb attObj) ]
    (.onerror usercb ch (:info attObj) ev) ))

;; close any output stream created during the message handling
(defn- nio-finz [^ChannelHandlerContext ctx]
  (let [ att (-> ctx (.getChannel) (.getAttachment))
         ^OutputStream os (:os att) ]
    (IOUtils/closeQuietly os)))

(defn- wsframe [^ChannelHandlerContext ctx ^WebSocketFrame frame]
  (let [ cc (.getChannel ctx) ]
    if (frame instanceof CloseWebSocketFrame) {
      _handshaker.close(cc, (CloseWebSocketFrame) frame);
      return; 
    }
    else if (frame instanceof PingWebSocketFrame) {
      cc.write(new PongWebSocketFrame(frame.getBinaryData()));
      return;
    }
    else if ( frame instanceof BinaryWebSocketFrame) {
      data=getBits(frame);
    }
    else if ( frame instanceof TextWebSocketFrame) {
      data=((TextWebSocketFrame)frame).getText();
    }
    else if ( frame instanceof ContinuationWebSocketFrame) {
      ContinuationWebSocketFrame cf=(ContinuationWebSocketFrame )frame;
      if (cf.isFinalFragment()) {
        data=cf.getAggregatedText();
      } else {
        return; 
      }
    }
    
    WaitEvent w= new AsyncWaitEvent( new WebSockEvent(_dev, data),                
            new WebSockTrigger(_dev, ctx) );
    final Event ev = w.getInnerEvent();
    
    w.timeoutMillis(_dev.getWaitMillis());
    _dev.holdEvent(w) ;
    
    _dev.getDeviceManager().getEngine()
    .getScheduler().run( new Runnable(){
        public void run() {                
            _dev.dispatch(ev) ;
        }            
    });

  }


(defn- maybeSSL [^ChannelHandlerContext ctx]
  (let [ ssl (-> ctx (.getPipeline) (.get (class SslHandler))) ]
    (if-not (nil? ssl)
      (let [ cf (.handshake ^SslHandler ssl) ]
        (.addListener cf 
                      (reify ChannelFutureListener 
                        (operationComplete [_ fff] 
                          (if-not (.isSuccess fff) (-> fff (.getChannel)(.close)))))))
      )))

(defn- nio-wsock [^ChannelHandlerContext ctx evt ^HttpRequest req usercb]
  (let [ wf (WebSocketServerHandshakerFactory.
                     (str "ws://" (.getHeader req "host") (.getUri req)) null false)
         ch (.getChannel ctx)
         hs (.newHandshaker wf req) ]
    (if (nil? hs)
      (.sendUnsupportedWebSocketVersionResponse wf ch)
      (do
        (.addListener
          (.handshake hs ch req)
          (reify ChannelFutureListener
            (operationComplete [_ fff]
              (if (.isSuccess fff)
                (maybeSSL ctx)
                (Channels/fireExceptionCaught (.getChannel fff) (.getCause fff)))))))
  )))

(defn- nio-preq [^ChannelHandlerContext ctx ^HttpRequest req usercb]
  (let [ msginfo (nio-extract-req req)
         attObj (nio-cfgctx ctx { :dir req :info msginfo } usercb) ]
    (debug "nio-preq: received a " (:method msginfo ) " request from " (:uri msginfo))
    (when (HttpHeaders/is100ContinueExpected req) (send-100-cont ctx))
    (if (:is-chunked msginfo)
      (debug "nio-preq: request is chunked.")
      ;; msg not chunked, suck all data from the request
      (do (try
              (nio-sockit-down ctx req)
            (finally (nio-finz ctx)))
        (nio-pcomplete ctx req)))))

;; handle a request
(defn- nio-prequest [^ChannelHandlerContext ctx evt ^HttpRequest req usercb]
  (let [ x (.getHeader req "upgrade") ]
    (if (.equalsIgnoreCase ^String x "websocket")
      (nio-wsock ctx evt req usercb)
      (nio-preq ctx req usercb))))

(defn- nio-redirect [^ChannelHandlerContext ctx ^MessageEvent ev]
  ;; TODO: handle redirect properly, for now, same as error
  (nio-perror ctx ev))

(defn- nio-presbody [^ChannelHandlerContext ctx ^HttpResponse res]
    (if (.isChunked res)
      (debug "nio-presbody: response is chunked.")
      (try
          (nio-sockit-down ctx res)
        (finally (nio-finz ctx)))))

;; handle a response
(defn- nio-pres [^ChannelHandlerContext ctx ^MessageEvent ev ^HttpResponse res usercb]
  (let [ msginfo (nio-extract-res res)
         s (.getStatus res)
         r (.getReasonPhrase s)
         c (.getCode s) ]
    (debug "nio-pres: got a response: code " c " reason: " r)
    (nio-cfgctx ctx { :dir res :info msginfo } usercb)
    (cond
      (and (>= c 200) (< c 300)) (nio-presbody ctx res)
      (and (>= c 300) (< c 400)) (nio-redirect ctx ev)
      :else (nio-perror ctx ev))))

;; handle a chunk - part of a message
(defn- nio-chunk [^ChannelHandlerContext ctx ^HttpChunk msg]
  (let [ done (try
                  (nio-sockit-down ctx msg)
                  (.isLast msg)
                (catch Throwable e#
                    (do (nio-finz ctx) (throw e#)))) ]
    (if done (nio-pcomplete ctx msg) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make our channel handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn netty-pipe-handler

  [^comzotohcljc.netty.comms.NettyServiceIO usercb]
  (proxy [SimpleChannelHandler] []

    (exceptionCaught [ctx ev]
      (let [ ch (.getChannel ^ChannelHandlerContext ctx)
             attObj (.getAttachment ch)
             msginfo (:info attObj)
             keepAlive (if (nil? msginfo) false (:keep-alive msginfo)) ]
        (error (.getCause ^ExceptionEvent ev) "")
        (.onerror usercb ch msginfo ev)
        (when-not keepAlive (CU/TryC (.close ch)))))

    (messageReceived [ctx ev]
      (let [ msg (.getMessage ^MessageEvent ev) ]
        ;;(debug "typeof of USERCB ===== " (type usercb))
        (cond
          (instance? HttpRequest msg) (nio-prequest ctx ev msg usercb)
          (instance? WebSocketFrame) (nio-wsframe ctx msg)
          (instance? HttpResponse msg) (nio-pres ctx ev msg usercb)
          (instance? HttpChunk msg) (nio-chunk ctx msg)
          :else (throw (IOException. "Received some unknown object." ) ))))

    ))

(defn make-pipeServer

  ^ChannelPipelineFactory [^SSLContext sslctx usercb]

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

(defn make-pipeClient

  ^ChannelPipelineFactory [^SSLContext sslctx usercb]

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

(defn netty-xxx-server

  ^NettyServer
  [^String host port
   ^comzotohcljc.netty.comms.NettyServiceIO usercb options]

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

  [^String host port
   ^comzotohcljc.netty.comms.NettyServiceIO usercb options ]

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
        (let [ mtd (.toUpperCase (SU/nsb (:method msginfo)))
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

(defn make-httpClient ""

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
            (throw (IOException. ""))
            (throw e)))
        (let [ ch (.getChannel cf) ]
          (.add cg ch)
          (debug "Netty client connected to " host ":" port " - OK.")
          ch)))))

(defn- send-httpClient

  [^Channel ch ^NettyClient clientr ^HttpRequest req ^XData xdata serviceIO]

  (let [ clen (if (nil? xdata) 0 (.size xdata))
         before-send (:before-send serviceIO)
         options (:options clientr)
         uri (.getUri req) ]
    (doto req
      (.setHeader (org.jboss.netty.handler.codec.http.HttpHeaders$Names/CONNECTION)
          (if (:keepAlive options)
            org.jboss.netty.handler.codec.http.HttpHeaders$Values/KEEP_ALIVE
            org.jboss.netty.handler.codec.http.HttpHeaders$Values/CLOSE))
      (.setHeader
        (org.jboss.netty.handler.codec.http.HttpHeaders$Names/HOST)
        (.getHost (URL. uri))))
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
    (let [ req (DefaultHttpRequest. (HttpVersion/HTTP_1_1)
                                    (HttpMethod/POST) (.toString targetUrl))
           ch (iniz-httpClient clientr targetUrl serviceIO) ]
      (send-httpClient ch clientr req xdata serviceIO))))

(defn async-get ""

  ([clientr ^URL targetUrl]
   (async-get clientr targetUrl (make-nilServiceIO)))

  ([clientr ^URL targetUrl serviceIO]
    (let [ req (DefaultHttpRequest. (HttpVersion/HTTP_1_1)
                                    (HttpMethod/GET) (.toString targetUrl))
           ch (iniz-httpClient clientr targetUrl serviceIO) ]
      (send-httpClient ch clientr req nil serviceIO))))













(def ^:private comms-eof nil)


