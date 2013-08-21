(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hhh.mvc.handler)

(import '(com.zotoh.hohenheim.io HTTPEvent Emitter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- routeCracker [^HTTPEvent evt]
  (let [ ^Emitter src (.emitter evt)
         ^comzotohcljc.hhh.core.sys.Thingy ctr (.container src)
         rts (.getAttr ctr :routes)
         cpath (.contextPath evt)
         mtd (.method evt)
         uri (.getUri evt)
         rc (if-not (nil? rts) 
              (some (fn [^comzotohcljc.hhh.mvc.ri.RouteInfo ri]
                      (let [ m (.resemble? ri mtd cpath uri) ]
                        (if (nil? m) nil [ri m])))
                    (seq rts)) )
         rt (if (nil? rc) 
              [false nil nil ""]
              [true (first rc)(last rc) ""] ) ]

    (if (and (not (nth rt 0))
             (not (.endsWith uri "/"))
             (maybeRedirect mtd (str uri "/")))
      [true nil nil (str uri "/")]
      rt)))



(defn- serveWelcomeFile [^HTTPEvent evt]
  (if (not (.matches (.getUri evt) "/?"))
    nil
    (let [ src (.emitter evt)
           ctr (.container src)
           appDir (.appDir ctr)
           fs (.getAttr src :welcome-files) ]
      (some (fn [f]
              (let [ file (File. appDir (str DN_PUBLIC "/" f)) ]
                (if (and (.exists file)
                         (.canRead file)) file nil)))
            (seq fs)) )))

(defn- serveStatic [src ri ^Matcher mc ch req evt]
  (let [ appDir (-> src (.container)(.appDir))
         mpt (SU/nsb (.getf ri :mountPoint))
         ps (CU/nice-fpath (File. appDir DN_PUBLIC))
         uri (.getUri evt)
         gc (.groupCount mc) ]
    (with-local-vars [ mp (StringUtils/replace mpt
                                               "${app.dir}"
                                               (CU/nice-fpath appDir)) ]
      (for [i (range 1 gc)]
        (var-set mp (StringUtils/replace @mp "{}" (.group mc i) 1)))

      ;; serve all static assets from *public folder*
      (var-set mp (CU/nice-fpath (File @mp)))
      (if (.startsWith @mp ps)
        (handleStatic src ch req (File. @mp))
        (do
          (warn "attempt to access non public file-system: " @mp)
          (serve-403 ch))))))

(defn- serveRoute [src ri ^Matcher mc ch evt]
    val w= new AsyncWaitEvent( evt, new NettyTrigger(evt, ctx.getChannel) )
    val pms = ri.resolveMatched(mc)
    pms.foreach { (en) => evt.addParam(en._1, en._2) }
    w.timeoutMillis( _src.waitMillis)
    evt.addAttr( PF_ROUTE_INFO, ri)
    evt.routerClass = ri.pipeline()
    _src.hold(w)
    _src.dispatch(evt)
  }
  (let [ evt (ioes-reify-event co ch req xdata)
         pms  (.resolveMatched ri mc)
         ^comzotohcljc.hhh.io.core.WaitEventHolder
         w (make-async-wait-holder (make-netty-trigger ch evt co) evt) ]
    (.timeoutMillis w (.getAttr ^comzotohcljc.hhh.core.sys.Thingy co :waitMillis))
    (.hold co w)
    (.dispatch co evt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn onNettyREQ [ co ch req msginfo xdata]
  (let [ evt (ioes-reify-event co ch req xdata)
         [r1 r2 r3 r4] (routeCracker evt) ]
    (cond
      (and r1 (SU/hgl? r4))
      (sendRedirect ch false r4)

      (= r1 true)
      (do
        (debug "matched one route: " (.path r2))
        (if (.isStatic r2)
          (serveStatic r2 r3 ch req evt)
          (serveRoute r2 r3 ch evt)))

      :else
      (let [ fp (serveWelcomeFile req) ]
        (if (nil? fp) 
          (handleStatic src ch req fp)
          (do
            (debug "failed to match uri: " (.getUri req))
            (serve404 ch)))) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private handler-eof nil)

