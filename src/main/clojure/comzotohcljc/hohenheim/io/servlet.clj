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
(import '(javax.servlet.http Cookie HttpServletRequest))
(import '(javax.servlet ServletConfig))
(import '(java.util ArrayList))
(import '(java.net HttpCookie))

(import '(org.apache.commons.io IOUtils))
(import '(java.io IOException))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.hohenheim.io IOSession HTTPResult HTTPEvent))


(use '[comzotohcljc.hohenheim.io.events  :only (make-servlet-event) ])
(use '[comzotohcljc.hohenheim.io.http  :only (make-http-result) ])


(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.mime :as MM])
(use '[comzotohcljc.hohenheim.io.triggers])
(use '[comzotohcljc.hohenheim.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn- cookie-to-javaCookie [^Cookie c]
  (doto (HttpCookie. (.getName c) (.getValue c))
      (.setDomain (.getDomain c))
      (.setHttpOnly (.isHttpOnly c))
      (.setMaxAge (.getMaxAge c))
      (.setPath (.getPath c))
      (.setSecure (.getSecure c))
      (.setVersion (.getVersion c))) )

(defmethod ioes-reify-event :czc.hhh.io/JettyIO
  [co & args]
  (let [ ^HttpServletRequest req (first args)
         ^HTTPResult result (make-http-result)
         eid (SN/next-long) ]
    (reify HTTPEvent

      (getCookie [_ nm]
        (let [ lnm (.toLowerCase nm) cs (.getCookies req) ]
          (some (fn [^Cookie c]
                  (if (= lnm (.toLowerCase (.getName c)))
                    (cookie-to-javaCookie c)
                    nil) )
                  (if (nil? cs) [] (seq cs)))) )

      (getCookies [_]
        (let [ rc (ArrayList.) cs (.getCookies req) ]
          (if-not (nil? cs)
            (doseq [ c (seq cs) ]
              (.add rc (cookie-to-javaCookie c))))
          rc))

      (getSession [_] nil)
      (emitter [_] co)
      (getId [_] eid)
      (isKeepAlive [_]
        (= (-> (SU/nsb (.getHeader req "connection")) (.toLowerCase))
        "keep-alive"))
      (data [_] nil)
      (hasData [_] false)
      (contentLength [_] (.getContentLength req))
      (contentType [_] (.getContentType req))
      (encoding [_] (.getCharacterEncoding req))
      (contextPath [_] (.getContextPath req))

      (getHeaderValue [_ nm] (.getHeader req nm))

      (getHeaderValues [_ nm]
        (let [ rc (ArrayList.) ]
          (doseq [ s (seq (.getHeaders req nm)) ]
            (.add rc s))))

      (getHeaders [_]
        (let [ rc (ArrayList.) ]
          (doseq [ ^String s (seq (.getHeaderNames req)) ]
            (.add rc s))) )

      (getParameterValue [_ nm] (.getParameter req nm))

      (getParameterValues [_ nm]
        (let [ rc (ArrayList.) ]
          (doseq [ s (seq (.getParameterValues req nm)) ]
            (.add rc s))))

      (getParameters [_]
        (let [ rc (ArrayList.) ]
          (doseq [ ^String s (seq (.getParameterNames req)) ]
            (.add rc s))) )

      (localAddr [_] (.getLocalAddr req))
      (localHost [_] (.getLocalName req))
      (localPort [_] (.getLocalPort req))

      (method [_] (.getMethod req))
      (protocol [_] (.getProtocol req))
      (queryString [_] (.getQueryString req))

      (remoteAddr [_] (.getRemoteAddr req))
      (remoteHost [_] (.getRemoteHost req))
      (remotePort [_] (.getRemotePort req))

      (scheme [_] (.getScheme req))

      (serverName [_] (.getServerName req))
      (serverPort [_] (.getServerPort req))

      (host [_] (.getHeader req "host"))


      (isSSL [_] (= "https" (.getScheme req)))

      (getUri [_] (.getRequestURI req))

      (getRequestURL [_] (.getRequestURL req))

      (getResultObject [_] result)
      (replyResult [_] )


      )))

(defn- dispREQ [ ^comzotohcljc.hohenheim.io.WebServlet c0 
                 ^Continuation ct evt req rsp]
  (let [ ^comzotohcljc.hohenheim.core.sys.Component dev @(.myState c0)
         wm (.getAttr dev :waitMillis) ]
    (doto ct
      (.setTimeout wm)
      (.suspend rsp))
    (let [ ^comzotohcljc.hohenheim.io.core.WaitEventHolder 
           w  (make-async-wait-holder evt
                  (make-servlet-trigger req rsp dev))
          ^comzotohcljc.hohenheim.io.core.EmitterAPI src @(.myState c0) ]
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
  ([] (atom nil)))

(defn -service [ ^comzotohcljc.hohenheim.io.WebServlet this
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


(defn -init [ ^comzotohcljc.hohenheim.io.WebServlet this ^ServletConfig cfg]
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

