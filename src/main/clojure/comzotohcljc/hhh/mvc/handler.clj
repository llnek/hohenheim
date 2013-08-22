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

  comzotohcljc.hhh.mvc.handler)

(import'(java.io File))
(import'(com.zotoh.frwk.io XData))

(import '(com.zotoh.hohenheim.io HTTPEvent Emitter))
(import '(com.zotoh.hohenheim.mvc MVCUtils))
(import '(org.jboss.netty.channel Channel))
(import '(org.jboss.netty.buffer ChannelBuffers))
(import '(org.jboss.netty.handler.codec.http
  DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse HttpMethod))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.util.core :only (MuObj) ])
(use '[comzotohcljc.hhh.io.triggers])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.core.constants])
(require '[comzotohcljc.hhh.mvc.tpls :as WP])
(require '[comzotohcljc.netty.comms :as NE])
(require '[comzotohcljc.util.core :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- isModified [^String eTag lastTm ^HttpRequest req]
  (with-local-vars [ modd true ]
    (cond
      (.containsHeader req "If-None-Match")
      (var-set modd (not= eTag (.getHeader req "If-None-Match")))

      (.containsHeader req "If-Unmodified-Since")
      (let [ s (.getHeader req "If-Unmodified-Since") ]
        (when (SU/hgl? s)
          (CU/Try! (when (>= (.getTime (.parse MVCUtils/getSDF s)) lastTm)
                     (var-set modd false)))))
      :else nil)
    @modd))


(defn- addETag
  [^comzotohcljc.hhh.core.sys.Thingy src
   ^HTTPEvent evt ^HttpRequest req ^HttpResponse rsp ^File file ]

    (let [ maxAge (.getAttr src :cacheMaxAgeSecs)
           lastTm (.lastModified file)
           eTag  (str "\""  lastTm  "-"  (.hashCode file)  "\"") ]
      (if (isModified eTag lastTm req)
        (.setHeader rsp "Last-Modified" (.format MVCUtils/getSDF (Date. lastTm)))
        (if (= "GET" (.method evt))
          (.setStatus rsp HttpResponseStatus/NOT_MODIFIED)))
      (.setHeader rsp "Cache-Control"
        (if (= maxAge 0) "no-cache" (str "max-age=" maxAge)))

      (when (.getAttr src :useETag)
        (.setHeader rsp "ETag" eTag)) ))

(defn- serve-error
  [^comzotohcljc.hhh.core.sys.Thingy src
   ^Channel ch
   code]
  (let [ rsp (NE/make-resp-status code) ]
    (try
      (let [ tps (.getAttr src :templates)
             ^String fp ((keyword code) tps)
             ^WebPage tp (WP/getTemplate
                  (if (nil? fp) (str "" code ".html") fp)) ]
        (when-not (nil? tp)
          (.setHeader rsp "content-type" (.contentType tp))
          (let [ bits (CU/bytesify (.body tp)) ]
            (HttpHeaders/setContentLength rsp (alength bits))
            (.setContent rsp (ChannelBuffers/copiedBuffer bits)))))
      (NE/closeCF true (.write ch rsp))
      (catch Throwable e#
        (warn e# "")
        (.close ch)))))

(defn- handleStatic [src ^Channel ch req ^HTTPEvent evt ^File file]
  (let [ rsp (NE/make-resp-status) ]
    (try
      (if (or (nil? file)
              (not (.exists file)))
        (serve-error src ch 404)
        (do
          (debug "serving static file: " (CU/nice-fpath file))
          (addETag src evt req rsp file)
          (if (= (-> rsp (.getStatus)(.getCode)) 304)
            (NE/closeCF (not (.isKeepAlive evt)) (.write ch rsp))
            (WP/getFileAsset src ch req rsp file))))
      (catch Throwable e#
        (error "failed to get static resource " (.getUri evt) e)
        (CU/Try!  (serve-error src ch 500)))) ))

(defn- seekRoute [mtd uri rts]
  (if-not (nil? rts)
    (some (fn [^comzotohcljc.hhh.mvc.ri.RouteInfo ri]
            (let [ m (.resemble? ri mtd uri) ]
              (if (nil? m) nil [ri m])))
          (seq rts)) ))

(defn- routeCracker [^HTTPEvent evt]
  (let [ ^comzotohcljc.hhh.core.sys.Thingy
         ctr (.container ^Emitter (.emitter evt))
         rts (.getAttr ctr :routes)
         ;;cpath (.contextPath evt)
         mtd (.method evt)
         uri (.getUri evt)
         rc (seekRoute mtd uri rts)
         rt (if (nil? rc)
              [false nil nil ""]
              [true (first rc)(last rc) ""] ) ]

    (if (and (not (nth rt 0))
             (not (.endsWith uri "/"))
             (seekRoute mtd (str uri "/") rts))
      [true nil nil (str uri "/")]
      rt)))

(defn- serveWelcomeFile [^HTTPEvent evt]
  (if (not (.matches (.getUri evt) "/?"))
    nil
    (let [ ^Emitter src (.emitter evt)
           ctr (.container  src)
           appDir (.getAppDir ctr)
           fs (.getAttr ^comzotohcljc.hhh.core.sys.Thingy src :welcome-files) ]
      (some (fn [f]
              (let [ file (File. appDir (str DN_PUBLIC "/" f)) ]
                (if (and (.exists file)
                         (.canRead file)) file nil)))
            (seq fs)) )))

(defn- serveStatic
  [^Emitter src ^RouteInfo ri ^Matcher mc ch req evt]
  (let [ appDir (-> src (.container)(.getAppDir))
         mpt (SU/nsb (.getf ^comzotohcljc.util.core.MuObj ri :mountPoint))
         ps (CU/nice-fpath (File. appDir DN_PUBLIC))
         uri (.getUri evt)
         gc (.groupCount mc) ]
    (with-local-vars [ mp (StringUtils/replace mpt
                                               "${app.dir}"
                                               (CU/nice-fpath appDir)) ]
      (for [i (range 1 (+ gc 1)) ]
        (var-set mp (StringUtils/replace @mp "{}" (.group mc i) 1)))

      ;; ONLY serve static assets from *public folder*
      (var-set mp (CU/nice-fpath (File. @mp)))
      (if (.startsWith @mp ps)
        (handleStatic src ch req evt (File. @mp))
        (do
          (warn "attempt to access non public file-system: " @mp)
          (serve-error src ch 403)
          )))))

(defn- serveRoute
  [^comzotohcljc.hhh.core.sys.Thingy src
   ^comzotohcljc.hhh.mvc.rts.RouteInfo ri
   ^Matcher mc
   ^Channel ch
   ^HTTPEvent evt]
  (let [ wms (.getAttr src :waitMillis)
         pms (.collect ri mc) ]
    (.setf! evt :params (merge {} pms))
    (.setf! evt :route ri)
    (.setf! evt :router (.pipeline ri))
    (let [ ^comzotohcljc.hhh.io.core.EmitterAPI co src
           ^comzotohcljc.hhh.io.core.WaitEventHolder
           w (make-async-wait-holder (make-netty-trigger ch evt co) evt) ]
      (.timeoutMillis w wms)
      (.hold co w)
      (.dispatch co evt))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn onNettyREQ
  [ ^Emitter co ^Channel ch ^HttpRequest req
    ^comzotohcljc.net.comms.HTTPMsgInfo msginfo
   ^XData xdata]
  (let [ evt (ioes-reify-event co ch req xdata)
         [r1 r2 r3 r4] (routeCracker evt) ]
    (cond
      (and r1 (SU/hgl? r4))
      (NE/sendRedirect ch false r4)

      (= r1 true)
      (do
        (debug "matched one route: " (.getPath r2))
        (if (.isStatic? r2)
          (serveStatic co r2 r3 ch req evt)
          (serveRoute co r2 r3 ch evt)))

      :else
      (let [ fp (serveWelcomeFile evt) ]
        (if (nil? fp)
          (handleStatic src ch req evt fp)
          (do
            (debug "failed to match uri: " (.getUri evt))
            (serve-error src ch 404)
            ))) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private handler-eof nil)

