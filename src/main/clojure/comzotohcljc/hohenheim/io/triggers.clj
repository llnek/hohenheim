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

  comzotohcljc.hohenheim.io.triggers )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.jboss.netty.handler.codec.http HttpResponseStatus))
(import '(org.eclipse.jetty.continuation ContinuationSupport))
(import '(org.eclipse.jetty.continuation Continuation))
(import '(org.jboss.netty.channel ChannelFutureListener))
(import '(org.jboss.netty.handler.codec.http 
  HttpHeaders HttpHeaders$Names HttpVersion CookieEncoder DefaultHttpResponse))
(import '(java.nio.channels ClosedChannelException))
(import '(org.jboss.netty.handler.stream ChunkedStream))
(import '(java.util Timer TimerTask))

(import '(org.apache.commons.io IOUtils))
(import '(com.zotoh.frwk.io XData))

(require '[comzotohcljc.net.comms :as NU])
(require '[comzotohcljc.util.core :as CU])

(use '[comzotohcljc.hohenheim.io.events :rename { emitter evt-emitter } ])
(use '[comzotohcljc.hohenheim.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol WaitEventHolder ""
  (resumeOnResult [_ res] )
  (timeoutMillis [_ millis] )
  (onExpiry [_])
  (timeoutSecs [_ secs] ) )

(defprotocol AsyncWaitTrigger ""
  (resumeWithResult [_ res] )
  (resumeWithError [_] )
  (emitter [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-servlet-trigger "" [req rsp src]
  (reify AsyncWaitTrigger

    (resumeWithResult [this res]
      (let [ code (.statusCode res)
             hdrs (.headers res) ]
        (try
          (doseq [[n v] (seq hdrs)]
            (when-not (= "content-length" (.toLowerCase n))
              (.setHeader rsp n v)))

          (if (.hasError res)
            (.sendError rsp code (.errorMsg res))
            (let [ data (.data res)
                   clen (if (and (instance? XData data) (.hasContent data))
                          (.size data)
                          0) ]
              (.setStatus rsp code)
              (.setContentLength clen)
              (when (> clen 0)
                (IOUtils/copyLarge (.stream data)
                                   (.getOutputStream rsp)
                                   clen))))
          (catch Throwable e#
            (error e# ""))
          (finally
            (-> (ContinuationSupport/getContinuation req)
              (.complete))) )))


    (resumeWithError [_]
      (let [ s HttpResponseStatus/INTERNAL_SERVER_ERROR ]
        (try
            (.sendError rsp (.getCode s) (.getReasonPhrase s))
          (catch Throwable e#
            (error e# ""))
          (finally
            (-> (ContinuationSupport/getContinuation req)
              (.complete)))) )) ) )


(defn- maybeClose [evt cf]
  (when-not (.isKeepAlive evt)
    (when-not (nil? cf)
      (.addListener cf ChannelFutureListener/CLOSE ))))

(defn- netty-reply [ch evt res]
  (let [ rsp (DefaultHttpResponse. HttpVersion/HTTP_1_1 (.getStatus res))
         data (.getData res)
         clen (if (and (instance? XData data) (.hasContent data))
                (.size data)
                0)
         cke (CookieEncoder. true)
         hdrs (.getHeaders res) ]

    (doseq [[n v] (seq hdrs)]
      (when-not (= "content-length" (.toLowerCase n))
        (.setHeader rsp n v)))

    (.setHeader rsp "content-length" (str "" clen))

    (doseq [ c (seq (.getCookies res)) ]
      (.addCookie cke c))
    (.addHeader rsp HttpHeaders$Names/SET_COOKIE (.encode cke))

    ;; this throw NPE some times !
    (let [ cf
              (try
                  (.write ch rsp)
                (catch ClosedChannelException e#
                  (warn "ClosedChannelException thrown: NettyTrigger @line 86")
                  nil)
                (catch Throwable t# (error t# "") nil))
           cf2
              (try
                  (if (> clen 0)
                    (.write ch (ChunkedStream. (.stream data)))
                    nil)
                (catch ClosedChannelException e#
                  (warn "ClosedChannelException thrown: NettyTrigger @line 94")
                  nil)
                (catch Throwable t# (error t# "") nil)) ]

        (maybeClose evt (if (nil? cf2) cf cf2)))) )


(defn make-netty-trigger [ch evt src]
  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (CU/TryC
        (netty-reply ch evt res) ))

    (resumeWithError [this]
      (resumeWithResult this
        (NU/http-response HttpResponseStatus/INTERNAL_SERVER_ERROR) ) )

    (emitter [_] src) ))


(defn make-async-wait-holder [event trigger]
  (let [ impl (CU/make-mmap) ]
    (reify WaitEventHolder

      (resumeOnResult [this res]
        (let [ tm (.mm-g impl :timer) ]
          (when-not (nil? tm) (.cancel tm))
          (-> (.emitter event) (.release this))
          (.mm-s impl :result res)
          (eve-unbind event)
          (.resumeWithResult trigger res)
          ))

      (timeoutMillis [this millis]
        (let [ tm (Timer. true) ]
          (.mm-s impl :timer tm)
          (eve-bind event this)
          (.schedule tm (proxy [TimerTask][]
            (run [_] (onExpiry this))) millis)))

      (timeoutSecs [this secs] (timeoutMillis this (* 1000 secs)))

      (onExpiry [this]
        (do
          (-> (.emitter event) (.release this))
          (.mm-s impl :timer nil)
          (eve-unbind event)
          (.resumeWithError trigger)
          ))

      )))




















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private triggers-eof nil)

