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

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.util Date))
(import '(java.io File))
(import '(com.zotoh.frwk.io XData))

(import '(com.zotoh.hohenheim.mvc
  HTTPErrorHandler MVCUtils WebAsset WebContent ))
(import '(com.zotoh.hohenheim.io HTTPEvent Emitter))
(import '(org.jboss.netty.channel Channel))
(import '(org.jboss.netty.buffer ChannelBuffers))
(import '(org.jboss.netty.handler.codec.http
  HttpHeaders$Values HttpHeaders$Names
  DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse HttpMethod))
(import '(jregex Matcher Pattern))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.util.core :only (MuObj) ])
(use '[comzotohcljc.hhh.io.triggers])
(use '[comzotohcljc.hhh.io.netty])
(use '[comzotohcljc.hhh.io.core])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.core.constants])

(require '[comzotohcljc.hhh.mvc.tpls :as WP])
(require '[comzotohcljc.netty.comms :as NE])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.meta :as MU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- isModified [^String eTag lastTm ^HttpRequest req]
  (with-local-vars [ modd true ]
    (cond
      (.containsHeader req HttpHeaders$Names/IF_NONE_MATCH)
      (var-set modd (not= eTag (.getHeader req HttpHeaders$Names/IF_NONE_MATCH)))

      (.containsHeader req HttpHeaders$Names/IF_UNMODIFIED_SINCE)
      (let [ s (.getHeader req HttpHeaders$Names/IF_UNMODIFIED_SINCE) ]
        (when (SU/hgl? s)
          (CU/Try! (when (>= (.getTime (.parse (MVCUtils/getSDF) s)) lastTm)
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
        (.setHeader rsp HttpHeaders$Names/LAST_MODIFIED
                    (.format (MVCUtils/getSDF) (Date. lastTm)))
        (if (= (.getMethod req) HttpMethod/GET)
          (.setStatus rsp HttpResponseStatus/NOT_MODIFIED)))
      (.setHeader rsp HttpHeaders$Names/CACHE_CONTROL
                  (if (= maxAge 0) "no-cache" (str "max-age=" maxAge)))
      (when (.getAttr src :useETag)
        (.setHeader rsp HttpHeaders$Names/ETAG eTag)) ))

(defn- reply-error [^Emitter src code]
  (let [ ctr (.container src)
         appDir (.getAppDir ctr) ]
    (WP/getLocalFile appDir (str "pages/errors/" code ".html"))))

(defn- serve-error
  [^comzotohcljc.hhh.core.sys.Thingy src
   ^Channel ch
   code]
  (let [ rsp (NE/make-resp-status code) ]
    (try
      (let [ h (.getAttr src :errorHandler)
             ^HTTPErrorHandler
             cb (if (SU/hgl? h) (MU/make-obj h) nil)
             ^WebContent
             rc (if (nil? cb)
                  (reply-error src code)
                  (.getErrorResponse cb code)) ]
        (when-not (nil? rc)
          (.setHeader rsp "content-type" (.contentType rc))
          (let [ bits (.body rc) ]
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
          ;; 304 not-modified
          (if (= (-> rsp (.getStatus)(.getCode)) 304)
            (NE/closeCF (not (.isKeepAlive evt)) (.write ch rsp))
            (WP/replyFileAsset src ch req rsp file))))
      (catch Throwable e#
        (error "failed to get static resource " (.getUri evt) e#)
        (CU/Try!  (serve-error src ch 500)))) ))

(defn- seekRoute [mtd uri rts]
  (if-not (nil? rts)
    (some (fn [^comzotohcljc.hhh.mvc.rts.RouteInfo ri]
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
         ;; [ri mc] routeinfo matcher
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
           ctr (.container src)
           appDir (.getAppDir ctr)
           fs (.getAttr ^comzotohcljc.hhh.core.sys.Thingy src :welcomeFiles) ]
      (some (fn [^String f]
              (let [ file (File. appDir (str DN_PUBLIC "/" f)) ]
                (if (and (.exists file)
                         (.canRead file)) file nil)))
            (seq fs)) )))

(defn- serveStatic
  [^Emitter src
   ^comzotohcljc.hhh.mvc.rts.RouteInfo ri
   ^Matcher mc ^Channel ch req ^HTTPEvent evt]
  (let [ ^File appDir (-> src (.container)(.getAppDir))
         mpt (SU/nsb (.getf ^comzotohcljc.util.core.MuObj ri :mountPoint))
         ps (CU/nice-fpath (File. appDir ^String DN_PUBLIC))
         uri (.getUri evt)
         gc (.groupCount mc) ]
    (with-local-vars [ mp (StringUtils/replace mpt
                                               "${app.dir}"
                                               (CU/nice-fpath appDir)) ]
      (for [i (range 1 (+ gc 1)) ]
        (var-set mp (StringUtils/replace @mp "{}" (.group mc ^long i) 1)))

      ;; ONLY serve static assets from *public folder*
      (var-set mp (CU/nice-fpath (File. ^String @mp)))
      (if (.startsWith ^String @mp ps)
        (handleStatic src ch req evt (File. ^String @mp))
        (do
          (warn "attempt to access non public file-system: " @mp)
          (serve-error src ch 403)
          )))))

(defn- serveRoute
  [^comzotohcljc.hhh.core.sys.Thingy src
   ^comzotohcljc.hhh.mvc.rts.RouteInfo ri
   ^Matcher mc
   ^Channel ch
   ^comzotohcljc.util.core.MuObj evt]
  (let [ wms (.getAttr src :waitMillis)
         pms (.collect ri mc) ]
    (.setf! evt :router (.getHandler ri))
    (.setf! evt :params (merge {} pms))
    (.setf! evt :route ri)
    (let [ ^comzotohcljc.hhh.io.core.EmitterAPI co src
           ^comzotohcljc.hhh.io.core.WaitEventHolder
           w (make-async-wait-holder (make-netty-trigger ch evt co) evt) ]
      (.timeoutMillis w wms)
      (.hold co w)
      (.dispatch co evt))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handleOneNettyREQ
  [ ^Emitter co
    ^Channel ch
    ^HttpRequest req
    ;;^comzotohcljc.net.comms.HTTPMsgInfo msginfo
    ^XData xdata]
  (let [ ^HTTPEvent evt (ioes-reify-event co ch req xdata)
         [r1 ^comzotohcljc.hhh.mvc.rts.RouteInfo r2 r3 r4] (routeCracker evt) ]
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
          (handleStatic co ch req evt fp)
          (do
            (debug "failed to match uri: " (.getUri evt))
            (serve-error co ch 404)
            ))) )))


(defmethod netty-service-req :czc.hhh.io/NettyMVC
  [^comzotohcljc.hhh.io.core.EmitterAPI co
   ch req msginfo xdata]
  (handleOneNettyREQ co ch req xdata))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private handler-eof nil)

