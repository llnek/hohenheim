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

  comzotohcljc.hhh.io.triggers )

(import '(io.netty.handler.codec.http HttpResponseStatus))
(import '(org.eclipse.jetty.continuation ContinuationSupport))
(import '(org.eclipse.jetty.continuation Continuation))
(import '(io.netty.channel Channel ChannelFuture ChannelFutureListener))
(import '(io.netty.handler.codec.http
  HttpResponse HttpHeaders HttpHeaders$Names HttpVersion
  ServerCookieEncoder DefaultHttpResponse))
(import '(java.nio.channels ClosedChannelException))
(import '(java.io OutputStream IOException))
(import '(io.netty.handler.stream ChunkedStream))
(import '(java.util List Timer TimerTask))
(import '(java.net HttpCookie))
(import '(javax.servlet.http Cookie HttpServletRequest HttpServletResponse))
(import '(io.netty.buffer ByteBuf Unpooled ByteBufHolder))
(import '(java.nio ByteBuffer))

(import '(org.apache.commons.io IOUtils))
(import '(com.zotoh.frwk.util NCMap))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.frwk.core Identifiable))
(import '(com.zotoh.hohenheim.io WebSockEvent WebSockResult HTTPEvent))
(import '(io.netty.handler.codec.http.websocketx
  WebSocketFrame
  BinaryWebSocketFrame
  TextWebSocketFrame))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.netty.comms :only [HTTP-CODES wflush makeHttpReply] ])
(use '[comzotohcljc.util.core :only [make-mmap stringify notnil? Try!] ])
(use '[comzotohcljc.util.str :only [nsb] ])
(use '[comzotohcljc.hhh.io.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)



(defn isServletKeepAlive [^HttpServletRequest req]
  (let [ v (.getHeader req "connection") ]
    (= "keep-alive" (.toLowerCase (nsb v)))))

(defn- cookieToServlet ^Cookie [^HttpCookie c]
  (doto (Cookie. (.getName c) (.getValue c))
        (.setDomain (.getDomain c))
        (.setHttpOnly (.isHttpOnly c))
        (.setMaxAge (.getMaxAge c))
        (.setPath (.getPath c))
        (.setSecure (.getSecure c))
        (.setVersion (.getVersion c))) )

(defn- replyServlet [^comzotohcljc.util.core.MuObj res
                     ^HttpServletRequest req
                     ^HttpServletResponse rsp
                     src]
  (let [ ^OutputStream os (.getOutputStream rsp)
         ^NCMap hds (.getf res :hds)
         ^List cks (.getf res :cookies)
         ^URL url (.getf res :redirect)
         code (.getf res :code)
         data (.getf res :data)
         ^HttpResponseStatus
         status (get HTTP-CODES code) ]

    (when (nil? status) (throw (IOException. (str "Bad HTTP Status code: " code))))
    (try
      (.setStatus rsp code)
      (doseq [[^String nm vs] (seq hds)]
        (when-not (= "content-length" (.toLowerCase nm))
          (doseq [vv (seq vs) ]
            (.addHeader rsp nm vv))))
      (doseq [ c (seq cks) ]
        (.addCookie rsp (cookieToServlet c)))
      (cond
        (and (>= code 300)(< code 400))
        (.sendRedirect rsp (.encodeRedirectURL rsp (nsb url)))
        :else
        (let [ ^XData dd (cond
                            (instance? XData data) data
                            (notnil? data) (XData. data)
                            :else nil)
               clen (if (and (notnil? dd) (.hasContent dd))
                        (.size dd)
                        0) ]
            (.setContentLength rsp clen)
            (.flushBuffer rsp)
            (when (> clen 0)
                (IOUtils/copyLarge (.stream dd) os 0 clen)
                (.flush os) )))
      (catch Throwable e#
        (error e# ""))
      (finally
        (Try! (when-not (isServletKeepAlive req) (.close os)))
        (-> (ContinuationSupport/getContinuation req)
          (.complete))) ) ) )


(defn make-servlet-trigger ""

  [^HttpServletRequest req ^HttpServletResponse rsp src]

  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (replyServlet res req rsp src) )

    (resumeWithError [_]
        (try
            (.sendError rsp 500)
          (catch Throwable e#
            (error e# ""))
          (finally
            (-> (ContinuationSupport/getContinuation req)
              (.complete)))) )) )

(defn- maybeClose [^HTTPEvent evt ^ChannelFuture cf]
  (when-not (.isKeepAlive evt)
    (when-not (nil? cf)
      (.addListener cf ChannelFutureListener/CLOSE ))))

(defn- cookiesToNetty ^String [^List cookies]
  (persistent! (reduce (fn [sum ^HttpCookie c]
                         (conj! sum
                                (ServerCookieEncoder/encode
                                  (.getName c)(.getValue c))))
                       (transient [])
                       (seq cookies)) ))

(defn- netty-ws-reply [^WebSockResult res ^Channel ch ^WebSockEvent evt src]
  (let [ ^XData xs (.getData res)
         bits (.javaBytes xs)
         ^WebSocketFrame
         f (cond
              (.isBinary res)
              (BinaryWebSocketFrame. (Unpooled/wrappedBuffer bits))

              :else
              (TextWebSocketFrame. (nsb (stringify bits)))) ]
    (wflush ch f)))

(defn- netty-reply [^comzotohcljc.util.core.MuObj res
                    ^Channel ch
                    ^HTTPEvent evt
                    src]
  (let [ cks (cookiesToNetty (.getf res :cookies))
         code (.getf res :code)
         rsp (makeHttpReply code)
         data (.getf res :data)
         hdrs (.getf res :hds) ]
    (with-local-vars [ clen 0 xd nil ]
      (doseq [[^String nm vs] (seq hdrs)]
        (when-not (= "content-length" (.toLowerCase nm))
          (doseq [vv (seq vs)]
            (HttpHeaders/addHeader rsp nm vv))))
      (doseq [s cks]
        (HttpHeaders/addHeader rsp HttpHeaders$Names/SET_COOKIE cks) )
      (cond
        (and (>= code 300)(< code 400))
        (HttpHeaders/setHeader rsp "Location" (nsb (.getf res :redirect)))
        :else
        (let [ ^XData dd (if (nil? data)
                            (XData.)
                            (cond
                              (instance? XData data) data
                              :else (XData. data))) ]
          (var-set clen (if (.hasContent dd) (.size dd) 0))
          (var-set xd dd)
          (HttpHeaders/setContentLength rsp @clen)) )
      (try
          (let [ cf (wflush ch rsp) ]
            (when (= @clen 0) (maybeClose evt cf)))
        (catch ClosedChannelException e#
          (warn "ClosedChannelException thrown while flushing headers"))
        (catch Throwable t# (error t# "") ))
      (when (> @clen 0)
        (try
          (maybeClose evt (wflush ch (ChunkedStream. (.stream ^XData @xd))))
          (catch ClosedChannelException e#
            (warn "ClosedChannelException thrown while flushing body"))
          (catch Throwable t# (error t# "") )))
      )))


(defn make-netty-trigger [^Channel ch evt src]
  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (cond
        (instance? WebSockEvent evt)
        (Try! (netty-ws-reply res ch evt src) )
        :else
        (Try! (netty-reply res ch evt src) ) ))

    (resumeWithError [_]
      (let [ rsp (makeHttpReply 500) ]
        (try
          (maybeClose evt (wflush ch rsp))
          (catch ClosedChannelException e#
            (warn "ClosedChannelException thrown while flushing headers"))
          (catch Throwable t# (error t# "") )) ))

    ))


(defn make-async-wait-holder

  [ ^comzotohcljc.hhh.io.core.AsyncWaitTrigger trigger
    ^HTTPEvent event ]

  (let [ impl (make-mmap) ]
    (reify

      Identifiable
      (id [_] (.getId event))

      WaitEventHolder

      (resumeOnResult [this res]
        (let [ ^Timer tm (.mm-g impl :timer)
               ^comzotohcljc.hhh.io.core.EmitterAPI  src (.emitter event) ]
          (when-not (nil? tm) (.cancel tm))
          (.release src this)
          ;;(.mm-s impl :result res)
          (.resumeWithResult trigger res)
          ))

      (timeoutMillis [me millis]
        (let [ tm (Timer. true) ]
          (.mm-s impl :timer tm)
          (.schedule tm (proxy [TimerTask][]
            (run [] (.onExpiry me))) ^long millis)))

      (timeoutSecs [this secs]
        (timeoutMillis this (* 1000 secs)))

      (onExpiry [this]
        (let [ ^comzotohcljc.hhh.io.core.EmitterAPI
               src (.emitter event) ]
          (.release src this)
          (.mm-s impl :timer nil)
          (.resumeWithError trigger) ))

      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private triggers-eof nil)

