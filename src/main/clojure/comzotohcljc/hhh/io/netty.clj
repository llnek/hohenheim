;;
;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
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
  HttpHeaders Cookie QueryStringDecoder))
(import '(org.jboss.netty.channel Channel))
(import '(org.jboss.netty.channel.group
  ChannelGroup))
(import '(org.jboss.netty.bootstrap ServerBootstrap))

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

(defn- cookieToJava [^Cookie c]
  (doto (HttpCookie. (.getName c)(.getValue c))
    (.setComment (.getComment c))
    (.setDomain (.getDomain c))
    (.setMaxAge (.getMaxAge c))
    (.setPath (.getPath c))
    (.setVersion (.getVersion c))
    (.setHttpOnly (.isHttpOnly c)) ))

(defmethod ioes-reify-event :czc.hhh.io/NettyIO
  [co & args]
  (let [ ^HttpRequest req (nth args 1)
         ^XData xdata (nth args 2)
         ^Channel ch (nth args 0)
         ssl (CU/notnil? (.get (.getPipeline ch) "ssl"))
         ^InetSocketAddress laddr (.getLocalAddress ch)
         eeid (SN/next-long) ]
    (with-meta
      (reify HTTPEvent
        (getSession [_] nil)
        (emitter [_] co)
        (getId [_] eeid)
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

        (getResultObj [_] (make-http-result))
        (replyResult [_] )

      )
      { :typeid :czc.hhh.io/HTTPEvent } )) )

(defmethod comp-configure :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Thingy co cfg]
  (let [ c (SU/nsb (:context cfg)) ]
    (.setAttr! co :contextPath (SU/strim c))
    (http-basic-config co cfg) ))

(defn- make-service-io [^comzotohcljc.hhh.io.core.EmitterAPI co]
  (reify comzotohcljc.netty.comms.NettyServiceIO
    (before-send [_ ch msg] nil)
    (onerror [_ ch msginfo exp] )
    (onreq [_ ch req msginfo xdata]
      (let [ evt (ioes-reify-event co ch req xdata)
             ^comzotohcljc.hhh.io.core.WaitEventHolder
             w (make-async-wait-holder evt
                 (make-netty-trigger ch evt co)) ]
        (.timeoutMillis w (.getAttr ^comzotohcljc.hhh.core.sys.Thingy co :waitMillis))
        (.hold co w)
        (.dispatch co evt)))
    (onres [_ ch rsp msginfo xdata] nil)) )

(defn- make-handler [co]
    (NE/netty-pipe-handler (make-service-io co)))

(defmethod comp-initialize :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ [^ServerBootstrap bs opts] (NE/server-bootstrap)
         file (.getAttr co :serverKey)
         ssl (CU/notnil? file)
         pwd (.getAttr co :pwd)
         ctx (if ssl (SS/make-sslContext file pwd)) ]
    (doto bs
      (.setPipelineFactory
        (NE/make-pipeServer ctx  (make-handler co))))
    (.setAttr! co :netty
      (comzotohcljc.netty.comms.NettyServer. bs nil opts))
    co))

(defmethod ioes-start :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Thingy co]
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
    (ioes-started co)))

(defmethod ioes-stop :czc.hhh.io/NettyIO
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ nes (.getAttr co :netty) ]
    (NE/finz-server nes)
    (ioes-stopped co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private netty-eof nil)

