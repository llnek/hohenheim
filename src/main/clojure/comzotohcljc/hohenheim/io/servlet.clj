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

(import '(org.eclipse.jetty.continuation ContinuationSupport))
(import '(org.eclipse.jetty.continuation Continuation))

(import '(org.apache.commons.io IOUtils))
(import '(java.io IOException))
(import '(com.zotoh.frwk.io XData))

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.hohenheim.io.triggers])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod ioes-reify-event :czc.hhh.io/JettyIO
  [co & args]
  (let [ e (make-servlet-event co
                    (nth args 0)) ]
    (reify HTTPEvent
      ())))


(defn- replyService [ this res rsp]
  (let [ s (.getStatus res)
         sc (.getCode s)
         hdrs (.getHeaders res)
         data  (.getData res) ]
    (with-local-vars [clen 0]
      (CU/TryC
        (doseq [[n v] (seq hdrs)]
          (when-not (= "content-length" (.toLowerCase n))
            (.setHeader rsp n v)))
        (if (.hasError res)
          (.sendError rsp sc (.getReasonPhrase s))
          (.setStatus rsp sc))
        (when-not (and (instance? XData data) (.hasContent data))
          (var-set clen (.size data))
          (IOUtils/copyLarge (.stream data) (.getOutputStream rsp) 0 clen))
        (.setContentLength rsp @clen) ))))


(defn- dispREQ [this ct evt req rsp]
  (let [ dev @(.myState this)
         wm (.getAttr dev :waitMillis) ]
    (doto ct
      (.setTimeout wm)
      (.suspend rsp))
    (let [ w  (make-async-wait-holder evt
                  (make-servlet-trigger req rsp dev)) ]
      (.timeoutMillis w wm)
      (.hold dev w)
      (.dispatch dev evt))))

(defn- doASyncSvc [this evt req rsp]
  (let [ c (ContinuationSupport/getContinuation req) ]
    (when (.isInitial c) 
      (CU/TryC
          (dispREQ this c evt req rsp) ))))

(defn- doSyncSvc [this evt req rsp]
  (throw (IOException. "No Sync Service!!!!!!")))

(defn -myInit []
  ([] (atom nil)))

(defn -service [this req rsp]
  (let [ state (.myState this)
         evt (ioes-reify-event @state req) ]
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
           src (.getAttribute ctx "czchhhiojetty") ]
      (reset! state src)
      (CU/TryC
        (debug
          "********************************************************************\n"
          (str "Servlet Container: " (.getServerInfo ctx) "\n")
          (str "Servlet IO: " src "\n")
          "********************************************************************\n"
          (str "Servlet:iniz() - servlet:" (.myName this) "\n" ) )) )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private servlet-eof nil)

