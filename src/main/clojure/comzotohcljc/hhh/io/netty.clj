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

  comzotohcljc.hhh.io.netty)

(import '(java.net HttpCookie URI URL InetSocketAddress))
(import '(java.net SocketAddress InetAddress))
(import '(java.util ArrayList List))
(import '(java.io IOException))

(import '(com.zotoh.hohenheim.io HTTPEvent))
(import '(javax.net.ssl SSLContext))
(import '(org.jboss.netty.handler.codec.http
  HttpRequest HttpResponse CookieDecoder CookieEncoder
  DefaultHttpResponse HttpVersion HttpHeaders
  HttpHeaders Cookie QueryStringDecoder))
(import '(org.jboss.netty.channel Channel))
(import '(org.jboss.netty.channel.group
  ChannelGroup))
(import '(org.jboss.netty.bootstrap ServerBootstrap))
(import '(com.zotoh.frwk.core Hierarchial Identifiable))
(import '(com.zotoh.hohenheim.io WebSockEvent WebSockResult))
(import '(com.zotoh.frwk.io XData))
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



(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.util.core :only (MuObj) ])

(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.io.core])
(use '[comzotohcljc.hhh.io.http])
(use '[comzotohcljc.hhh.io.triggers])

(require '[comzotohcljc.crypto.ssl :as SS])
(require '[comzotohcljc.netty.comms :as NE])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.mime :as MM])
(require '[comzotohcljc.util.str :as SU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defmulti netty-service-req "" (fn [a & args] (:typeid (meta a))))

(defn- make-wsock-result []
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :binary false)
    (.mm-s impl :text false)
    (.mm-s impl :data nil)
    (reify
      MuObj
      (setf! [_ k v] (.mm-s impl k v) )
      (seq* [_] (seq (.mm-m* impl)))
      (getf [_ k] (.mm-g impl k) )
      (clrf! [_ k] (.mm-r impl k) )
      (clear! [_] (.mm-c impl))

      WebSockResult
      (isBinary [_] (.mm-g impl :binary))
      (isText [_] (.mm-g impl :text))
      (getData [_] (XData. (.mm-g impl :data)))
      )))

(defn- cookieToJava [^Cookie c]
  (doto (HttpCookie. (.getName c)(.getValue c))
    (.setComment (.getComment c))
    (.setDomain (.getDomain c))
    (.setMaxAge (.getMaxAge c))
    (.setPath (.getPath c))
    (.setVersion (.getVersion c))
    (.setHttpOnly (.isHttpOnly c)) ))


(defn- make-wsock-event
  [^comzotohcljc.hhh.io.core.EmitterAPI co ^Channel ch ^XData xdata]
  (let [ ^WebSockResult res (make-wsock-result)
         ssl (CU/notnil? (.get (.getPipeline ch) "ssl"))
         ^InetSocketAddress laddr (.getLocalAddress ch)
         impl (CU/make-mmap)
         eeid (SN/next-long) ]
    (with-meta
      (reify
        MuObj

        (setf! [_ k v] (.mm-s impl k v) )
        (seq* [_] (seq (.mm-m* impl)))
        (getf [_ k] (.mm-g impl k) )
        (clrf! [_ k] (.mm-r impl k) )
        (clear! [_] (.mm-c impl))

        Identifiable
        (id [_] eeid)

        WebSockEvent
        (bindSession [_ s] (.mm-s impl :ios s))
        (getSession [_] (.mm-g impl :ios))
        (getId [_] eeid)
        (isSSL [_] ssl)
        (isText [_] (instance? String (.content xdata)))
        (isBinary [this] (not (.isText this)))
        (getData [_] xdata)
        (getResultObj [_] res)
        (replyResult [this]
          (let [ ^comzotohcljc.hhh.io.core.WaitEventHolder
                 wevt (.release co this) ]
            (when-not (nil? wevt)
              (.resumeOnResult wevt res))))
        (emitter [_] co))

      { :typeid :czc.hhh.io/WebSockEvent } )))

(defmethod ioes-reify-event :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.io.core.EmitterAPI co & args]
  (let [ ^HTTPResult res (make-http-result)
         ^HttpRequest req (nth args 1)
         ^XData xdata (nth args 2)
         ^Channel ch (nth args 0)
         ssl (CU/notnil? (.get (.getPipeline ch) "ssl"))
         ^InetSocketAddress laddr (.getLocalAddress ch)
         impl (CU/make-mmap)
         eeid (SN/next-long) ]
    (with-meta
      (reify

        MuObj

        (setf! [_ k v] (.mm-s impl k v) )
        (seq* [_] (seq (.mm-m* impl)))
        (getf [_ k] (.mm-g impl k) )
        (clrf! [_ k] (.mm-r impl k) )
        (clear! [_] (.mm-c impl))

        Identifiable
        (id [_] eeid)

        HTTPEvent
        (bindSession [_ s] (.mm-s impl :ios s))
        (getSession [_] (.mm-g impl :ios))
        (getId [_] eeid)
        (emitter [_] co)
        (getCookies [_]
          (let [ v (SU/nsb (.getHeader req "Cookie"))
                 rc (ArrayList.)
                 cdc (CookieDecoder.)
                 cks (if (SU/hgl? v) (.decode cdc v) []) ]
            (doseq [ ^Cookie c (seq cks) ]
              (.add rc (cookieToJava c)))
            rc))
        (getCookie [_ nm]
          (let [ v (SU/nsb (.getHeader req "Cookie"))
                 lnm (.toLowerCase nm)
                 cdc (CookieDecoder.)
                 cks (if (SU/hgl? v) (.decode cdc v) []) ]
            (some (fn [^Cookie c]
                    (if (= (.toLowerCase (.getName c)) lnm)
                      (cookieToJava c)
                      nil))
                    (seq cks))))

        (isKeepAlive [_] (HttpHeaders/isKeepAlive req))

        (data [_] xdata)
        (hasData [_] (CU/notnil? xdata))

        (contentLength [_] (HttpHeaders/getContentLength req))
        (contentType [_] (.getHeader req "content-type"))

        (encoding [this]  (MM/get-charset (.contentType this)))
        (contextPath [_] "")

        (getHeaderValues [_ nm] (.getHeaders req nm))
        (getHeaders [_] (.getHeaderNames req))
        (getHeaderValue [_ nm] (.getHeader req nm))
        (getParameterValues [_ nm]
          (let [ dc (QueryStringDecoder. (.getUri req))
                 rc (.get (.getParameters dc) nm) ]
            (if (nil? rc) (ArrayList.) rc)))
        (getParameters [_]
          (let [ dc (QueryStringDecoder. (.getUri req))
                 m (.getParameters dc) ]
            (.keySet m)))
        (getParameterValue [_ nm]
          (let [ dc (QueryStringDecoder. (.getUri req))
                 ^List rc (.get (.getParameters dc) nm) ]
            (if (and (CU/notnil? rc) (> (.size rc) 0))
              (.get rc 0)
              nil)))

        (localAddr [_] (.getHostAddress (.getAddress laddr)))
        (localHost [_] (.getHostName laddr))
        (localPort [_] (.getPort laddr))

        (protocol [_] (.toString (.getProtocolVersion req)))
        (method [_] (.toString (.getMethod req)))

        (host [_] (HttpHeaders/getHost req))

        (queryString [_]
          (let [ s (SU/nsb (.getUri req))
                 pos (.indexOf s "?") ]
            (if (>= pos 0)
              (.substring s pos)
              "")))

        (remoteAddr [_] (SU/nsb (.getHeader req "REMOTE_ADDR")))
        (remoteHost [_] (SU/nsb (.getHeader req "")))
        (remotePort [_] (CU/conv-long (.getHeader req "REMOTE_PORT") 0))

        (scheme [_] (if ssl "https" "http"))

        (serverName [_] (SU/nsb (.getHeader req "SERVER_NAME")))
        (serverPort [_] (CU/conv-long (.getHeader req "SERVER_PORT") 0))

        (isSSL [_] ssl)

        (getUri [_]
          (let [ dc (QueryStringDecoder. (.getUri req)) ]
            (.getPath dc)))

        (getRequestURL [_] (throw (IOException. "not implemented")))

        (getResultObj [_] res)
        (replyResult [this]
          (let [ ^comzotohcljc.hhh.io.core.WaitEventHolder
                 wevt (.release co this) ]
            (when-not (nil? wevt)
              (.resumeOnResult wevt res))))

      )

      { :typeid :czc.hhh.io/HTTPEvent } )) )

(defmethod comp-configure :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Element co cfg]
  (let [ c (SU/nsb (:context cfg)) ]
    (.setAttr! co :contextPath (SU/strim c))
    (http-basic-config co cfg) ))

(defmethod netty-service-req :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.io.core.EmitterAPI co
   ch req msginfo xdata]
  (let [ mtd (:method msginfo)
         evt (cond
                (= "WS" mtd)
                (make-wsock-event co ch xdata)
                :else
                (ioes-reify-event co ch req xdata))
         ^comzotohcljc.hhh.io.core.WaitEventHolder
         w (make-async-wait-holder (make-netty-trigger ch evt co) evt) ]
    (.timeoutMillis w
                    (.getAttr
                      ^comzotohcljc.hhh.core.sys.Element co
                      :waitMillis))
    (.hold co w)
    (.dispatch co evt)))

(defn- make-service-io [^comzotohcljc.hhh.io.core.EmitterAPI co reqcb]
  (reify comzotohcljc.netty.comms.NettyServiceIO
    (before-send [_ ch msg] nil)
    (onerror [_ ch msginfo exp]  nil)
    (onreq [_ ch req msginfo xdata]
      (reqcb co ch req msginfo xdata))
    (onres [_ ch rsp msginfo xdata] nil)) )

(defn- init-netty
  [^comzotohcljc.hhh.core.sys.Element co reqcb]
  (let [ [^ServerBootstrap bs opts] (NE/server-bootstrap)
         file (.getAttr co :serverKey)
         ssl (CU/notnil? file)
         pwd (.getAttr co :pwd)
         ^comzotohcljc.hhh.core.sys.Element
         ctr (.parent ^Hierarchial co)
         routes (.getAttr ctr :routes)
         ctx (if ssl (SS/make-sslContext file pwd)) ]
    (doto bs
      (.setPipelineFactory
        (NE/make-pipeServer ctx
                            (make-service-io co reqcb)
                            (NE/make-routeCracker routes) )))
    (.setAttr! co :netty
      (comzotohcljc.netty.comms.NettyServer. bs nil opts))
    co))

(defmethod ioes-start :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ host (SU/nsb (.getAttr co :host))
         ^long port (.getAttr co :port)
         ^comzotohcljc.netty.comms.NettyServer
         nes (.getAttr co :netty)
         ^ServerBootstrap bs (.server nes)
         ^InetAddress ip (if (SU/nichts? host)
              (InetAddress/getLocalHost)
              (InetAddress/getByName host))
         c (.bind bs (InetSocketAddress. ip port))
         cg (NE/channel-group) ]
;;    c.getConfig().setConnectTimeoutMillis(millis)
    (.add cg c)
    (.setAttr! co :netty
      (comzotohcljc.netty.comms.NettyServer. (.server nes)
                                           cg
                                           (.options nes)) )
    (debug "netty-io running on port: " port ", host: " host ", ip: " ip)
    (ioes-started co)))

(defmethod ioes-stop :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ nes (.getAttr co :netty) ]
    (NE/finz-server nes)
    (ioes-stopped co)))

(defmethod comp-initialize :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (init-netty co netty-service-req))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private netty-eof nil)

