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

  comzotohcljc.hohenheim.io.http )

(import '(org.eclipse.jetty.server Server Connector))
(import '(java.net URL))
(import '(java.io File))

(import '(org.eclipse.jetty.server
  Connector
  HttpConfiguration
  HttpConnectionFactory
  SecureRequestCustomizer
  Server
  ServerConnector
  SslConnectionFactory))
(import '(org.eclipse.jetty.util.ssl SslContextFactory))
(import '(org.eclipse.jetty.util.thread QueuedThreadPool))

(import '(com.zotoh.hohenheim.io JettyUtils))

(use '[comzotohcljc.crypto.ssl])

(require '[comzotohcljc.crypto.codec :as CR])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.core.sys])
(use '[comzotohcljc.hohenheim.io.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn http-basic-config [co cfg]
  (let [ file (:server-key cfg)
         port (:port cfg)
         fv (:flavor cfg)
         socto (:soctoutmillis cfg)
         kbs (:threshold-kb cfg)
         w (:wait-millis cfg)
         bio (:sync cfg)
         tds (:workers cfg)
         ssl (SU/hgl? file) ]

    (.setAttr! co :port 
               (if (and (number? port)(pos? port)) port (if ssl 443 80)))
    (.setAttr! co :host (:host cfg))
    (.setAttr! co :sslType (if (SU/hgl? fv) fv "TLS"))

    (when (SU/hgl? file)
      (CU/test-cond "server-key file url" (.startsWith file "file:"))
      (.setAttr! co :serverKey (URL. file))
      (.setAttr! co :pwd (CR/pwdify (:passwd cfg))) )

    (.setAttr! co :sockTimeOut
               (if (and (number? socto)(pos? socto)) socto 0))
    (.setAttr! co :async (if (true? bio) false true))
    (.setAttr! co :workers
               (if (and (number? tds)(pos? tds)) tds 6))
    (.setAttr! co :limit
               (if (and (number? kbs)(pos? kbs)) kbs (* 1024 8))) ;; 8M
    (.setAttr! co :waitMillis
               (if (and (number? w)(pos? w)) w 300000)) ;; 5 mins
    co))



(defmethod comp-configure :czc.hhh.io/HTTP
  [co cfg]
  (http-basic-config co cfg))

(defmethod comp-configure :czc.hhh.io/JettyIO
  [co cfg]
  (let [ c (SU/nsb (:context cfg)) ]
    (.setAttr! co :contextPath (SU/strim c))
    (http-basic-config co cfg) ))

(defn- cfgHTTPS [server port keyfile pwd conf]
  ;; SSL Context Factory for HTTPS and SPDY
  (let[ sslxf (doto (SslContextFactory.)
                (.setKeyStorePath (-> keyfile (.toURI)(.toURL)(.toString)))
                (.setKeyStorePassword pwd)
                (.setKeyManagerPassword pwd))
        config (doto (HttpConfiguration. conf)
                 (.addCustomizer (SecureRequestCustomizer.)))
        https (ServerConnector. server
            (SslConnectionFactory. sslxf "HTTP/1.1")
            (HttpConnectionFactory. config)) ]
    (doto https
      (.setPort port)
      (.setIdleTimeout (int 500000)))))

(defmethod comp-initialize :czc.hhh.io/JettyIO
  [co]
  (let [ conf (doto (HttpConfiguration.)
                (.setRequestHeaderSize 8192)  ;; from jetty examples
                (.setOutputBufferSize (int 32768)))
         keyfile (.getAttr co :serverKey)
         host (.getAttr co :host)
         port (.getAttr co :port)
         pwdObj (.getAttr co :pwd)
         ws (.getAttr co :workers)
         q (QueuedThreadPool. (if (pos? ws) ws 8))
         svr (Server. q)
         cc  (if (nil? keyfile)
               (doto (ServerConnector. svr (HttpConnectionFactory. conf))
                 (.setPort port)
                 (.setIdleTimeout (int 30000)))
               (cfgHTTPS svr port keyfile (SU/nsb pwdObj)
                         (doto conf
                           (.setSecureScheme "https")
                           (.setSecurePort port)))) ]

    (when (SU/hgl? host) (.setHost cc host))
    (.setName cc (CU/uid))
    (doto svr
      (.setConnectors (into-array Connector [cc])))
    (.setAttr! co :jetty svr)

    co))


(defmethod ioes-start :czc.hhh.io/JettyIO
  [co]
  (let [ container (.getParent co)
         app (.getAppDir container)
         jetty (.getAttr co :jetty)
         webapp (JettyUtils/newWebAppContext "czchhhiojetty" co)
         logDir (-> (File. app WEB_LOG)(.toURI)(.toURL)(.toString))
         resBase (-> app (.toURI)(.toURL)(.toString)) ]
    ;; static resources are based from resBase, regardless of context
    (doto webapp
      (.setDescriptor (-> (File. app WEB_XML)(.toURI)(.toURL)(.toString)))
      (.setParentLoaderPriority true)
      (.setResourceBase resBase )
      (.setContextPath (.getAttr co :contextPath)))
    ;;webapp.getWebInf()
    (.setHandler jetty webapp)
    (.start jetty)
    (ioes-started co)))


(defmethod ioes-stop :czc.hhh.io/JettyIO
  [co]
  (let [ svr (.getAttr co :jetty) ]
    (when-not (nil? svr)
      (CU/TryC
          (.stop svr) ))
    (ioes-stopped co)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive :czc.hhh.io/JettyIO :czc.hhh.io/HTTP)



(def ^:private http-eof nil)

