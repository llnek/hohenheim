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

  comzotohcljc.hhh.io.servlet

  (:gen-class
    :name comzotohcljc.hhh.io.WEBServlet
    :extends javax.servlet.http.HttpServlet
    :init myInit
    :constructors {[] []}
    :exposes-methods { init superInit  getServletName myName}
    :state myState
  ))

(import '(org.eclipse.jetty.continuation ContinuationSupport))
(import '(org.eclipse.jetty.continuation Continuation))
(import '(javax.servlet.http Cookie HttpServletRequest))
(import '(javax.servlet ServletConfig))
(import '(java.util ArrayList))
(import '(java.net HttpCookie))

(import '(org.apache.commons.io IOUtils))
(import '(java.io IOException))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.hohenheim.io IOSession HTTPResult HTTPEvent))
(import '(com.zotoh.frwk.core Identifiable))




(use '[comzotohcljc.hhh.io.events  :only (make-servlet-event) ])
(use '[comzotohcljc.hhh.io.http  :only (make-http-result) ])


(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.mime :as MM])
(use '[comzotohcljc.hhh.io.triggers])
(use '[comzotohcljc.hhh.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- dispREQ [ ^comzotohcljc.hhh.io.WEBServlet c0
                 ^Continuation ct evt req rsp]
  (let [ ^comzotohcljc.hhh.core.sys.Thingy dev @(.myState c0)
         wm (.getAttr dev :waitMillis) ]
    (doto ct
      (.setTimeout wm)
      (.suspend rsp))
    (let [ ^comzotohcljc.hhh.io.core.WaitEventHolder
           w  (make-async-wait-holder (make-servlet-trigger req rsp dev) evt)
          ^comzotohcljc.hhh.io.core.EmitterAPI  src @(.myState c0) ]
      (.timeoutMillis w wm)
      (.hold src w)
      (.dispatch src evt))))

(defn- doASyncSvc [this evt req rsp]
  (let [ c (ContinuationSupport/getContinuation req) ]
    (when (.isInitial c) 
      (CU/TryC
          (dispREQ this c evt req rsp) ))))

(defn- doSyncSvc [this evt req rsp]
  (throw (IOException. "No Sync Service!!!!!!")))

(defn -myInit []
  [ [] 
    (atom nil) ] )

(defn -service [ ^comzotohcljc.hhh.io.WEBServlet this
                 ^HttpServletRequest req rsp]
  (let [ state (.myState this)
         evt (ioes-reify-event @state req) ]
    (debug
      "********************************************************************"
      (.getRequestURL req)
      "********************************************************************")
    (if true
      (doASyncSvc this evt req rsp)
      (doSyncSvc this evt req rsp))))


(defn -init [ ^comzotohcljc.hhh.io.WEBServlet this ^ServletConfig cfg]
  (do
    (.superInit this cfg)
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

