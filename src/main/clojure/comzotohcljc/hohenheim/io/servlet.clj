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
  comzotohcljc.hohenheim.io.servlet

  (:gen-class
    :name comzotohcljc.hohenheim.io.WebServlet
    :extends javax.servlet.http.HttpServlet
    :init myInit
    :constructors {[] []}
    :exposes-methods { init super-init  getServletName myName}
    :state myState
  ))

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])



(def WEBSERVLET_DEVID "_#jetty.emitter#_")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- replyService [ this res rsp]
  (let [ sc (.statusCode res)
         hdrs (.headers res)
         data  (.data res) ]
    (with-local-vars [clen 0]
      (try
        (doseq [[n v] (seq hdrs)]
          (when-not (= "content-length" (.toLowerCase n))
            (.setHeader rsp n v)))
        (if (.hasError res)
          (.sendError rsp sc (.errorMsg res))
          (.setStatus rsp sc))
        (when-not (and (isa? XData data) (.hasContent data))
          (var-set clen (.size data))
          (IOUtils/copyLarge (.stream data) (.getOutputStream rsp) 0 clen))
        (.setContentLength rsp @clen)
        (catch Throwable e# (warn e#))))) )


(defn- dispREQ [this ct evt req rsp]
  (let [ dev @(.myState this)
         wm (.getAttr dev :waitMillis) ]
    (doto ct
      (.setTimeout wm)
      (.suspend rsp))
    (let [ w (AsyncWaitEvent. evt (AsyncServletTrigger. req rsp dev) )
           ev (.inner w) ]
      (.timeoutMillis w wm)
      (.hold dev w)
      (.dispatch dev ev))))

(defn- doASyncSvc [this evt req rsp]
  (let [ c (ContinuationSupport/getContinuation req) ]
    (when (.isInitial c) 
      (try
          (dispREQ this c evt req rsp)
        (catch Throwable e#
          (error e#))))))

(defn- doSyncSvc [this evt req rsp]
  (let [ w (SyncWaitEvent. evt)
         ev (.inner w)
         dev @(.myState this) ]
    (.hold dev w)
    (.dispatch dev ev)
    (try
        (.timeoutMillis w (.getAttr dev :waitMillis))
      (finally
        (.release dev w)))

    (let [ res (.result ev) ]
      (if (nil? res)
        (replyService this (HTTPResult. HTTPStatus/REQUEST_TIMEOUT) rsp)
        (replyService this res rsp) ))))


(defn -myInit []
  ([] (atom nil)))

(defn -service [this req rsp]
  (let [ state (.myState this)
         evt nil ] ;;(HTTPHplr/extract @state req) ]
    (debug
      "********************************************************************"
      (.getRequestURL req)
      "********************************************************************")
    (if true
      (doASyncSvc this evt req rsp)
      (doSyncSvc this evt req rsp))))


(defn -init [this cfg]
  (do
    (.super-init this cfg)
    (let [ ctx (.getServletContext cfg)
           state (.myState this)
           src (.getAttribute ctx WEBSERVLET_DEVID) ]
      (reset! state src)
      (CU/TryC
        (debug
          "********************************************************************"
          "Servlet Container: "
          (.getServerInfo ctx)
          "********************************************************************"
          "Servlet:iniz() - servlet:"
          (.myName this))) )))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private servlet-eof nil)

