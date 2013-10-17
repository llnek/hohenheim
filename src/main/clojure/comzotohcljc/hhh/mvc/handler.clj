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

(import '(com.zotoh.hohenheim.io HTTPEvent Emitter))
(import '(com.zotoh.hohenheim.mvc
  HTTPErrorHandler MVCUtils WebAsset WebContent))

(import '(org.jboss.netty.buffer ChannelBuffers ChannelBuffer))
(import '(org.jboss.netty.channel Channel))

(import '(org.jboss.netty.handler.codec.http
  HttpHeaders$Values HttpHeaders$Names
  DefaultHttpRequest
  HttpContentCompressor HttpHeaders HttpVersion
  HttpMessage HttpRequest HttpResponse HttpResponseStatus
  DefaultHttpResponse HttpMethod))

(import '(com.zotoh.frwk.net NetUtils))
(import '(jregex Matcher Pattern))

(use '[clojure.tools.logging :only [info warn error debug] ])
(use '[comzotohcljc.util.core :only [MuObj Try! nice-fpath] ])
(use '[comzotohcljc.hhh.io.triggers])
(use '[comzotohcljc.hhh.io.netty])
(use '[comzotohcljc.hhh.io.core])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.mvc.tpls :only [getLocalFile replyFileAsset] ])
(use '[comzotohcljc.netty.comms :only [sendRedirect makeRouteCracker
                                       makeHttpReply closeCF] ])
(use '[comzotohcljc.util.str :only [hgl? nsb] ])
(use '[comzotohcljc.util.meta :only [make-obj] ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- isModified [^String eTag lastTm ^HttpRequest req]
  (with-local-vars [ modd true ]
    (cond
      (.containsHeader req "if-none-match")
      (var-set modd (not= eTag (HttpHeaders/getHeader req "if-none-match")))

      (.containsHeader req "if-unmodified-since")
      (let [ s (HttpHeaders/getHeader req "if-unmodified-since") ]
        (when (hgl? s)
          (Try! (when (>= (.getTime (.parse (MVCUtils/getSDF) s)) lastTm)
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
        (HttpHeaders/setHeader rsp "last-modified"
                    (.format (MVCUtils/getSDF) (Date. lastTm)))
        (if (= (.getMethod req) HttpMethod/GET)
          (.setStatus rsp HttpResponseStatus/NOT_MODIFIED)))
      (HttpHeaders/setHeader rsp "cache-control"
                  (if (= maxAge 0) "no-cache" (str "max-age=" maxAge)))
      (when (.getAttr src :useETag)
        (HttpHeaders/setHeader rsp "etag" eTag)) ))

(defn- reply-error [^Emitter src code]
  (let [ ctr (.container src)
         appDir (.getAppDir ctr) ]
    (getLocalFile appDir (str "pages/errors/" code ".html"))))

(defn- serve-error
  [^comzotohcljc.hhh.core.sys.Element src
   ^Channel ch
   code]
  (with-local-vars [ rsp (makeHttpReply code) bits nil wf nil]
    (try
      (let [ h (.getAttr src :errorHandler)
             ^HTTPErrorHandler
             cb (if (hgl? h) (make-obj h) nil)
             ^WebContent
             rc (if (nil? cb)
                  (reply-error src code)
                  (.getErrorResponse cb code)) ]
        (when-not (nil? rc)
          (HttpHeaders/setHeader ^HttpMessage @rsp "content-type" (.contentType rc))
          (var-set bits (.body rc)))
        (HttpHeaders/setContentLength @rsp
                                      (if (nil? @bits) 0 (alength ^bytes @bits)))
        (var-set wf (.write ch @rsp))
        (when-not (nil? @bits)
          (var-set wf (.write ch (ChannelBuffers/wrappedBuffer ^bytes @bits))))
        (closeCF true @wf))
      (catch Throwable e#
        (NetUtils/closeChannel ch)))))

(defn- handleStatic [src ^Channel ch req ^HTTPEvent evt ^File file]
  (let [ rsp (makeHttpReply ) ]
    (try
      (if (or (nil? file)
              (not (.exists file)))
        (serve-error src ch 404)
        (do
          (debug "serving static file: " (nice-fpath file))
          (addETag src evt req rsp file)
          ;; 304 not-modified
          (if (= (-> rsp (.getStatus)(.getCode)) 304)
            (do
              (HttpHeaders/setContentLength rsp 0)
              (closeCF (.isKeepAlive evt) (.write ch rsp)))
            (replyFileAsset src ch req rsp file))))
      (catch Throwable e#
        (error "failed to get static resource " (.getUri evt) e#)
        (Try!  (serve-error src ch 500)))) ))

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
         mpt (nsb (.getf ^comzotohcljc.util.core.MuObj ri :mountPoint))
         ps (nice-fpath (File. appDir ^String DN_PUBLIC))
         uri (.getUri evt)
         gc (.groupCount mc) ]

    (with-local-vars [ mp (StringUtils/replace mpt
                                               "${app.dir}"
                                               (nice-fpath appDir)) ]
      (if (> gc 1)
        (doseq [ i (range 1 gc) ]
          (var-set mp (StringUtils/replace ^String @mp "{}" (.group mc (int i)) 1))) )

      ;; ONLY serve static assets from *public folder*
      (var-set mp (nice-fpath (File. ^String @mp)))
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
         rcc (makeRouteCracker (.getAttr ctr :routes))
         [r1 ^comzotohcljc.net.rts.RouteInfo r2 r3 r4]
         (.crack rcc msgInfo) ]
    (cond
      (and r1 (hgl? r4))
      (sendRedirect ch false r4)

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

