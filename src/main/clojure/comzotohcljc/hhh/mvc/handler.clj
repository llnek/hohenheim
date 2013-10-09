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

  comzotohcljc.hhh.mvc.handler)

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.util Date))
(import '(java.io File))
(import '(com.zotoh.frwk.io XData))

(import '(com.zotoh.hohenheim.mvc
  HTTPErrorHandler MVCUtils WebAsset WebContent ))
(import '(com.zotoh.hohenheim.io HTTPEvent Emitter))
(import '(io.netty.channel Channel))
(import '(io.netty.buffer ByteBuf Unpooled))
(import '(io.netty.handler.codec.http
  HttpHeaders$Values HttpHeaders$Names
  DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse HttpMethod))
(import '(jregex Matcher Pattern))
(import '(com.zotoh.frwk.net NetUtils))

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
      (-> (.headers req) (.contains HttpHeaders$Names/IF_NONE_MATCH))
      (var-set modd (not= eTag (HttpHeaders/getHeader req HttpHeaders$Names/IF_NONE_MATCH)))

      (-> (.headers req) (.contains HttpHeaders$Names/IF_UNMODIFIED_SINCE))
      (let [ s (HttpHeaders/getHeader req HttpHeaders$Names/IF_UNMODIFIED_SINCE) ]
        (when (SU/hgl? s)
          (CU/Try! (when (>= (.getTime (.parse (MVCUtils/getSDF) s)) lastTm)
                     (var-set modd false)))))
      :else nil)
    @modd))

(defn- addETag
  [^comzotohcljc.hhh.core.sys.Element src
   ^HTTPEvent evt ^HttpRequest req ^HttpResponse rsp ^File file ]

    (let [ maxAge (.getAttr src :cacheMaxAgeSecs)
           lastTm (.lastModified file)
           eTag  (str "\""  lastTm  "-"  (.hashCode file)  "\"") ]
      (if (isModified eTag lastTm req)
        (HttpHeaders/setHeader rsp HttpHeaders$Names/LAST_MODIFIED
                    (.format (MVCUtils/getSDF) (Date. lastTm)))
        (if (= (.getMethod req) HttpMethod/GET)
          (.setStatus rsp HttpResponseStatus/NOT_MODIFIED)))
      (HttpHeaders/setHeader rsp HttpHeaders$Names/CACHE_CONTROL
                  (if (= maxAge 0) "no-cache" (str "max-age=" maxAge)))
      (when (.getAttr src :useETag)
        (HttpHeaders/setHeader rsp HttpHeaders$Names/ETAG eTag)) ))

(defn- reply-error [^Emitter src code]
  (let [ ctr (.container src)
         appDir (.getAppDir ctr) ]
    (WP/getLocalFile appDir (str "pages/errors/" code ".html"))))

(defn- serve-error
  [^comzotohcljc.hhh.core.sys.Element src
   ^Channel ch
   code]
  (with-local-vars [ rsp (NE/makeHttpReply code) ]
    (try
      (let [ h (.getAttr src :errorHandler)
             ^HTTPErrorHandler
             cb (if (SU/hgl? h) (MU/make-obj h) nil)
             ^WebContent
             rc (if (nil? cb)
                  (reply-error src code)
                  (.getErrorResponse cb code)) ]
        (when-not (nil? rc)
          (HttpHeaders/setHeader ^HttpMessage @rsp "content-type" (.contentType rc))
          (let [ bits (.body rc) ]
            (HttpHeaders/setContentLength @rsp (alength bits))
            (NE/wflush ch @rsp)
            (var-set rsp nil)
            (NE/wflush ch (Unpooled/wrappedBuffer bits)))))
      (when-not (nil? @rsp)
        (NE/closeCF true (NE/wflush ch @rsp)))
      (catch Throwable e#
        (warn e# "")
        (NetUtils/closeChannel ch)))))

(defn- handleStatic [src ^Channel ch req ^HTTPEvent evt ^File file]
  (let [ rsp (NE/makeHttpReply 200) ]
    (try
      (if (or (nil? file)
              (not (.exists file)))
        (serve-error src ch 404)
        (do
          (debug "serving static file: " (CU/nice-fpath file))
          (addETag src evt req rsp file)
          ;; 304 not-modified
          (if (= (-> rsp (.getStatus)(.code)) 304)
            (NE/closeCF (not (.isKeepAlive evt)) (NE/wflush ch rsp))
            (WP/replyFileAsset src ch req rsp file))))
      (catch Throwable e#
        (error "failed to get static resource " (.getUri evt) e#)
        (CU/Try!  (serve-error src ch 500)))) ))

(defn- serveWelcomeFile [^HTTPEvent evt]
  (if (not (.matches (.getUri evt) "/?"))
    nil
    (let [ ^Emitter src (.emitter evt)
           ctr (.container src)
           appDir (.getAppDir ctr)
           fs (.getAttr ^comzotohcljc.hhh.core.sys.Element src :welcomeFiles) ]
      (some (fn [^String f]
              (let [ file (File. appDir (str DN_PUBLIC "/" f)) ]
                (if (and (.exists file)
                         (.canRead file)) file nil)))
            (seq fs)) )))

(defn- serveStatic
  [^Emitter src
   ^comzotohcljc.net.rts.RouteInfo ri
   ^Matcher mc ^Channel ch req ^HTTPEvent evt]
  (let [ ^File appDir (-> src (.container)(.getAppDir))
         mpt (SU/nsb (.getf ^comzotohcljc.util.core.MuObj ri :mountPoint))
         ps (CU/nice-fpath (File. appDir ^String DN_PUBLIC))
         uri (.getUri evt)
         gc (.groupCount mc) ]

    (with-local-vars [ mp (StringUtils/replace mpt
                                               "${app.dir}"
                                               (CU/nice-fpath appDir)) ]
      (if (> gc 1)
        (doseq [ i (range 1 gc) ]
          (var-set mp (StringUtils/replace ^String @mp "{}" (.group mc (int i)) 1))) )

      ;; ONLY serve static assets from *public folder*
      (var-set mp (CU/nice-fpath (File. ^String @mp)))
      (debug "request to serve static file: " @mp)
      (if (.startsWith ^String @mp ps)
        (handleStatic src ch req evt (File. ^String @mp))
        (do
          (warn "attempt to access non public file-system: " @mp)
          (serve-error src ch 403)
          )))))

(defn- serveRoute
  [^comzotohcljc.hhh.core.sys.Element src
   ^comzotohcljc.net.rts.RouteInfo ri
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
    ^comzotohcljc.net.comms.HTTPMsgInfo msgInfo
    ^XData xdata]
  (let [ ^HTTPEvent evt (ioes-reify-event co ch req xdata)
         ^comzotohcljc.hhh.core.sys.Element
         ctr (.container co)
         rcc (NE/make-routeCracker (.getAttr ctr :routes))
         [r1 ^comzotohcljc.net.rts.RouteInfo r2 r3 r4]
         (.crack rcc msgInfo) ]
    (cond
      (and r1 (SU/hgl? r4))
      (NE/sendRedirect ch false r4)

      (= r1 true)
      (do
        (debug "matched one route: " (.getPath r2) " , and static = " (.isStatic? r2))
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


(defmethod nettyServiceReq :czc.hhh.io/NettyMVC
  [^comzotohcljc.hhh.io.core.EmitterAPI co
   ch req msginfo xdata]
  (handleOneNettyREQ co ch req msginfo xdata))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private handler-eof nil)

