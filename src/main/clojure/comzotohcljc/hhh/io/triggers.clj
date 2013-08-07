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

  comzotohcljc.hhh.io.triggers )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.jboss.netty.handler.codec.http HttpResponseStatus))
(import '(org.eclipse.jetty.continuation ContinuationSupport))
(import '(org.eclipse.jetty.continuation Continuation))
(import '(org.jboss.netty.channel Channel ChannelFuture ChannelFutureListener))
(import '(org.jboss.netty.handler.codec.http
  HttpHeaders HttpHeaders$Names HttpVersion CookieEncoder DefaultHttpResponse))
(import '(java.nio.channels ClosedChannelException))
(import '(java.io OutputStream IOException))
(import '(org.jboss.netty.handler.stream ChunkedStream))
(import '(java.util List Timer TimerTask))
(import '(java.net HttpCookie))
(import '(javax.servlet.http Cookie HttpServletRequest HttpServletResponse))

(import '(org.apache.commons.io IOUtils))
(import '(com.zotoh.frwk.util NCMap))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.hohenheim.core Identifiable))
(import '(com.zotoh.hohenheim.io HTTPEvent))

(use '[comzotohcljc.netty.comms :only (*HTTP-CODES*) ])
(require '[comzotohcljc.net.comms :as NU])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hhh.io.events :rename { emitter evt-emitter } ])
(use '[comzotohcljc.hhh.io.core])
(use '[comzotohcljc.hhh.io.http :only (isServletKeepAlive) ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn- cookieToServlet ^Cookie [^HttpCookie c]
  (doto (Cookie. (.getName c) (.getValue c))
      (.setDomain (.getDomain c))
      (.setHttpOnly (.isHttpOnly c))
      (.setMaxAge (.getMaxAge c))
      (.setPath (.getPath c))
      (.setSecure (.getSecure c))
      (.setVersion (.getVersion c))) )

(defn- replyServlet [ ^comzotohcljc.util.core.MutableObj res
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
         status (get *HTTP-CODES* code) ]

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
        (.sendRedirect rsp (.encodeRedirectURL rsp (SU/nsb url)))
        :else
        (let [ ^XData dd (cond
                            (instance? XData data) data
                            (CU/notnil? data) (XData. data)
                            :else nil)
               clen (if (and (CU/notnil? dd) (.hasContent dd))
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
        (CU/Try! (when-not (isServletKeepAlive req) (.close os)))
        (-> (ContinuationSupport/getContinuation req)
          (.complete))) ) ) )


(defn make-servlet-trigger "" [^HttpServletRequest req ^HttpServletResponse rsp src]
  (reify AsyncWaitTrigger

    (resumeWithResult [this res]
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
  (let [ cke (CookieEncoder. true) ]
    (doseq [ ^HttpCookie c (seq cookies) ]
      (.addCookie cke (.getName c)(.getValue c)))
    (.encode cke)))

(defn- netty-reply [^comzotohcljc.util.core.MutableObj res
                    ^Channel ch
                    ^HTTPEvent evt
                    src]
  (let [ code (.getf res :code)
         rsp (DefaultHttpResponse. HttpVersion/HTTP_1_1
                                   (HttpResponseStatus/valueOf code))
         cks (cookiesToNetty (.getf res :cookies))
         data (.getf res :data)
         hdrs (.getf res :hds) ]
    (with-local-vars [ clen 0 xd nil ]
      (doseq [[^String nm vs] (seq hdrs)]
        (when-not (= "content-length" (.toLowerCase nm))
          (doseq [vv (seq vs)]
            (.addHeader rsp nm vv))))
      (when (SU/hgl? cks)
        (.addHeader rsp HttpHeaders$Names/SET_COOKIE2 cks) )
      (cond
        (and (>= code 300)(< code 400))
        (.setHeader rsp "Location" (SU/nsb (.getf res :redirect)))
        :else
        (let [ ^XData dd (if (nil? data)
                            (XData.)
                            (cond
                              (instance? XData data) data
                              :else (XData. data))) ]
          (var-set clen (if (.hasContent dd) (.size dd) 0))
          (var-set xd dd)
          (.setHeader rsp "content-length" (str "" clen)) ) )
      (try
          (let [ cf (.write ch rsp) ]
            (when (= @clen 0) (maybeClose evt cf)))
        (catch ClosedChannelException e#
          (warn "ClosedChannelException thrown while flushing headers"))
        (catch Throwable t# (error t# "") ))
      (when (> @clen 0)
        (try
          (maybeClose evt (.write ch (ChunkedStream. (.stream ^XData @xd))))
          (catch ClosedChannelException e#
            (warn "ClosedChannelException thrown while flushing body"))
          (catch Throwable t# (error t# "") )))
      )))


(defn make-netty-trigger [^Channel ch evt src]
  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (CU/Try! (netty-reply ch evt res) ))

    (resumeWithError [_]
      (let [ rsp (DefaultHttpResponse. HttpVersion/HTTP_1_1
                                   (HttpResponseStatus/valueOf 500)) ]
        (try
          (maybeClose evt (.write ch rsp))
          (catch ClosedChannelException e#
            (warn "ClosedChannelException thrown while flushing headers"))
          (catch Throwable t# (error t# "") )) ))

    ))


(defn make-async-wait-holder 
  
  [ ^comzotohcljc.hhh.io.core.AsyncWaitTrigger trigger
    ^HTTPEvent event ]

  (let [ impl (CU/make-mmap) ]
    (reify

      Identifiable
      (id [_] (.getId event))

      WaitEventHolder

      (resumeOnResult [this res]
        (let [ ^Timer tm (.mm-g impl :timer) 
               ^comzotohcljc.hhh.io.core.EmitterAPI src (.emitter event) ]
          (when-not (nil? tm) (.cancel tm))
          (.release src this)
          (.mm-s impl :result res)
          (eve-unbind event)
          (.resumeWithResult trigger res)
          ))

      (timeoutMillis [me millis]
        (let [ tm (Timer. true) ]
          (.mm-s impl :timer tm)
          (eve-bind event me)
          (.schedule tm (proxy [TimerTask][]
            (run [_] (.onExpiry me))) ^long millis)))

      (timeoutSecs [this secs] 
        (timeoutMillis this (* 1000 secs)))

      (onExpiry [this]
        (let [ ^comzotohcljc.hhh.io.core.EmitterAPI
               src (.emitter event) ]
          (.release src this)
          (.mm-s impl :timer nil)
          (eve-unbind event)
          (.resumeWithError trigger) ))

      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private triggers-eof nil)

