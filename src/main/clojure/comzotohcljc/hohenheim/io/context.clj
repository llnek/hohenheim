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
  comzotohcljc.hohenheim.io.context

  (:gen-class
    :name comzotohcljc.hohenheim.io.WebContext
    :extends javax.servlet.ServletContextListener
    :init myInit
    :constructors {[] []}
    :state myState
  ))

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.coreutils :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- inizAsJ2EE [ctx ctxPath]
  (let [ webinf (File. (.getRealPath ctx "/WEB-INF/"))
         root (.getParentFile webinf) ]
    nil))

(defn -contextInitialized [_ evt]
  (let [ x (.getServletContext evt)
         m (.getMajorVersion x)
         n (.getMinorVersion x)
         ctx   (if (or (> m 2) (and (= m 2)(> n 4)))
                  (.getContextPath x)
                  nil) ]
    (debug "WEBContextListener: contextInitialized()")
    (try
        (inizAsJ2EE x (SU/nsb ctx))
      (catch Throwable e#
        (error e#))) ))

(defn -contextDestroyed [this e]
  (let [ state (.myState this)
         src @state ]
    (debug "WEBContextListener: contextDestroyed()")
    (when-not (nil? src)
      (reset! state nil)
      (-> src (.container) (.dispose )))))

(defn -myInit []
  ([] (atom nil)))


(def ^:private context-eof nil)


