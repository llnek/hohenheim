
(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hohenheim.io.triggers )


(require '[comzotohcljc.util.coreutils :as CU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AsyncWaitTrigger
  (resumeWithResult [_ res] )
  (resumeWithError [_] )
  (emitter [_] ))


(defprotocol IOEvent
  )

(defn make-servlet-trigger [req rsp src]
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
                   clen (if (and (isa? XData data) (.hasContent data))
                          (.size data)
                          0) ]
              (.setStatus rsp code)
              (.setContentLength clen)
              (when (> clen 0)
                (IOUtils/copyLarge (.stream data)
                                   (.getOutputStream rsp)
                                   clen))))
          (catch Throwable e#
            (error e#))
          (finally
            (-> (ContinuationSupport/getContinuation req)
              (.complete))) )))


    (resumeWithError [_]
      (let [ s HTTPStatus/INTERNAL_SERVER_ERROR ]
        (try
            (.sendError rsp (.code s) (.reasonPhrase s))
          (catch Throwable e#
            (error e#))
          (finally
            (-> (ContinuationSupport/getContinuation req)
              (.complete)))) )) ) )


(defn- netty-reply [ch evt res]
  (let [ rsp (DefaultHttpResponse. HTTP_1_1
          (HttpResponseStatus. (.statusCode res) (.statusText res)))
         data (.data res)
         clen (if (and (isa? XData data) (.hasContent data))
                (.size data)
                0)
         cke (CookieEncoder. true)
         hdrs (.headers res) ]

    (doseq [[n v] (seq hdrs)]
      (when-not (= "content-length" (.toLowerCase n))
        (.setHeader rsp n v)))

    (.setHeader rsp "content-length" (str "" clen))

    (doseq [ c (seq (.getCookies res)) ]
      (.addCookie cke c))
    (.addHeader rsp Names/SET_COOKIE (.encode cke))

    ;; this throw NPE some times !
    (let [ cf
              (try
                  (.write ch rsp)
                (catch ClosedChannelException e#
                  (warn "ClosedChannelException thrown: NettyTrigger @line 86")
                  nil)
                (catch Throwable t# (error t#) nil))
           cf2
              (try
                  (if (> clen 0)
                    (.write ch (ChunkedStream. (.stream data)))
                    nil)
                (catch ClosedChannelException e#
                  (warn "ClosedChannelException thrown: NettyTrigger @line 94")
                  nil)
                (catch Throwable t# (error t#) nil)) ]

        (maybeClose evt (if (nil? cf2) cf cf2))))


(defn make-netty-trigger [ch evt src]
  (reify AsyncWaitTrigger

    (resumeWithResult [_ res]
      (try
        (netty-reply ch evt res)
        (catch Throwable e# (error e#))))

    (resumeWithError [this]
      (resumeWithResult this
        (HTTPResult. HTTPStatus/INTERNAL_SERVER_ERROR) ) )


    ))


(defn make-async-wait-holder [event]
  (let [ impl (CU/make-mmap) ]
    (reify WaitEventHolder

      (resumeOnResult [this res]
        (let [ tm (.mm-g impl :timer) ]
          (when-not (nil? tm) (.cancel tm))
          (-> (.emitter event) (.release this))
          (.mm-s impl :result res)
          (.resumeWithResult trigger res)
          ;;event=nil
          ))

      (timeoutMillis [this millis]
        (let [ tm (Timer. true) ]
          (.mm-s impl :timer tm)
          (.bind event this)
          (.schedule tm (proxy [TimerTask][]
            (run [_] (onExpiry this))) millis)))

      (timeoutSecs [this secs] (timeoutMillis this (* 1000 secs)))

      (onExpiry [this]
        (do
          (-> (.emitter event) (.release this))
          (.resumeWithError trigger)
          (.mm-s impl :timer nil)
          ;; event=nil
          ))

      )))




















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private triggers-eof nil)

