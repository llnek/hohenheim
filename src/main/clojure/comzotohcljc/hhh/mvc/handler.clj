(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.mvc.handler)

(import '(org.jboss.netty.handler.codec.http HttpChunkAggregator DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion HttpChunk
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse QueryStringDecoder HttpMethod
  HttpRequestDecoder HttpServerCodec HttpClientCodec
  HttpResponseEncoder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- serveWelcomeFile [co ^HttpRequest req]
  (if (not (-> req (.getUri)(.matches "/?")))
    nil
    (let [ fs (.getWelcomeFiles co) ]
      (some (fn [f]
              (let [ fp (File. (.appDir (.container co))
                               (str DN_PUBLIC "/" f)) ]
                (if (and (.exists f)
                         (.canRead f)) fp nil)))
            (seq fs)))))

(defn- doStatic [src ri ^Matcher mc ctx req evt]
  (let [ appDir (->  src (.container)(.appDir))
         uri (.uri evt)
         gc (.groupCount mc) ]
    (with-local-vars [ mp "" ]
      (var-set mp (StringUtils/replace (.mountPoint ri)
                                 "${app.dir}"
                                 (CU/nice-fpath appDir)))
      (when (> gc 1)
        (for [ x (range 1 gc) ]
          (var-set mp (StringUtils/replace @mp "{}" 
                                           (.group mc i) 1))))
      ;; serve all static assets from *public folder*
    (let [ ps (CU/nice-fpath (File. appDir DN_PUBLIC)) ]
      (var-set mp (CU/nice-fpath (File. @mp)))
      (if (.startsWith @mp ps)
        (handleStatic src ctx req (File. @mp))
        (do
          (warn "Attempt to access non public file-system: " @mp)
          (serve403 ctx)))))))

(defn- doRoute [ src ri ^Matcher mc ctx evt]
    val pms = ri.resolveMatched(mc)
    pms.foreach { (en) =>      evt.addParam(en._1, en._2) }
    w.timeoutMillis( _src.waitMillis)
    evt.addAttr( PF_ROUTE_INFO, ri)
    evt.routerClass = ri.pipeline()
    _src.hold(w)
    _src.dispatch(evt)
  }

(defn make-mvc-handler []
  (let []
    (reify
      NettyServiceIO

      (before-send [_ ch msg] nil)
      (onerror [_ ch msginfo evt] )
      (onreq [_ ch req msginfo xdata]
        (let [ rc (routeCracker evt) ]
          (cond
            (and (nth rc 0)
                 (SU/hgl? (nth rc 3)))
            (sendRedirect ctx false  (nth rc 3))

            (nth rc 0)
            (do
              (debug "Matched one Route: {}" rc._2.path)
              (if (-> (nth rc 1) (.isStatic)) 
                (doStatic (nth rc 1) (nth rc 2) ctx req evt)
                (doRoute (nth rc 1) (nth rc 2) ctx evt)))

            :else
            (let [ fp (serveWelcomeFile req) ]
              (if (nil? fp)
                (do
                  (debug "Failed to match Uri: " (.getUri req))
                  (serve404 ctx))
                (handleStatic  _src ctx req fp)))) ))

      (onres [_ ch rsp msginfo xdata] ))

    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private handler-eof nil)

