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

  comzotohcljc.hhh.io.http )

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(org.eclipse.jetty.server Server Connector ConnectionFactory))
(import '(java.util.concurrent ConcurrentHashMap))
(import '(java.net URL))
(import '(java.util List Map HashMap ArrayList))
(import '(java.io File))
(import '(com.zotoh.frwk.util NCMap))
(import '(javax.servlet.http Cookie HttpServletRequest))
(import '(java.net HttpCookie))
(import '(org.eclipse.jetty.continuation Continuation ContinuationSupport))
(import '(com.zotoh.frwk.server Component))
(import '(com.zotoh.frwk.core
  Versioned Hierarchial
  Identifiable Disposable Startable))

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
(import '(org.eclipse.jetty.webapp WebAppContext))
(import '(com.zotoh.hohenheim.io ServletEmitter Emitter))


(import '(com.zotoh.hohenheim.io HTTPResult HTTPEvent JettyUtils))
(import '(com.zotoh.hohenheim.core Container))

(use '[comzotohcljc.crypto.ssl])

(require '[comzotohcljc.crypto.codec :as CR])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.util.core :only (MuObj) ])
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.io.core])
(use '[comzotohcljc.hhh.io.triggers])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn make-servlet-emitter "" [^Container parObj]
  (let [ eeid (SN/next-long)
         impl (CU/make-mmap) ]
    (.mm-s impl :backlog (ConcurrentHashMap.))
    (with-meta
      (reify

        Element

        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )

        Component

        (version [_] "1.0")
        (id [_] eeid)

        Hierarchial

        (parent [_] parObj)

        ServletEmitter

        (container [this] (.parent this))
        (doService [this req rsp]
          (let [ ^comzotohcljc.hhh.core.sys.Element dev this
                 ^long wm (.getAttr dev :waitMillis) ]
            (doto (ContinuationSupport/getContinuation req)
              (.setTimeout wm)
              (.suspend rsp))
            (let [ evt (ioes-reify-event this req)
                   ^comzotohcljc.hhh.io.core.WaitEventHolder
                     w  (make-async-wait-holder
                          (make-servlet-trigger req rsp dev) evt)
                     ^comzotohcljc.hhh.io.core.EmitterAPI  src this ]
                (.timeoutMillis w wm)
                (.hold src w)
                (.dispatch src evt))) )

        Disposable

        (dispose [this] (ioes-dispose this))

        Startable

        (start [this] (ioes-start this))
        (stop [this] (ioes-stop this))

        EmitterAPI

        (enabled? [_] (if (false? (.mm-g impl :enabled)) false true ))
        (active? [_] (if (false? (.mm-g impl :active)) false true))

        (suspend [this] (ioes-suspend this))
        (resume [this] (ioes-resume this))

        (release [_ wevt]
          (when-not (nil? wevt)
            (let [ ^Map b (.mm-g impl :backlog)
                   wid (.id ^Identifiable wevt) ]
              (debug "emitter releasing an event with id: " wid)
              (.remove b wid))))

        (hold [_ wevt]
          (when-not (nil? wevt)
            (let [ ^Map b (.mm-g impl :backlog)
                   wid (.id ^Identifiable wevt) ]
              (debug "emitter holding an event with id: " wid)
              (.put b wid wevt))))

        (dispatch [this ev]
          (CU/TryC
              (.notifyObservers parObj ev) )) )

      { :typeid :czc.hhh.io/JettyIO } )))

(defn http-basic-config [^comzotohcljc.hhh.core.sys.Element co cfg]
  (let [ ^String file (:server-key cfg)
         port (:port cfg)
         ^String fv (:flavor cfg)
         socto (:soctoutmillis cfg)
         kbs (:threshold-kb cfg)
         w (:wait-millis cfg)
         bio (:sync cfg)
         tds (:workers cfg)
         pkey (:hhh.pkey cfg)
         ssl (SU/hgl? file) ]

    (.setAttr! co :port
               (if (and (number? port)(pos? port)) port (if ssl 443 80)))
    (.setAttr! co :host (:host cfg))
    (.setAttr! co :sslType (if (SU/hgl? fv) fv "TLS"))

    (when (SU/hgl? file)
      (CU/test-cond "server-key file url" (.startsWith file "file:"))
      (.setAttr! co :serverKey (URL. file))
      (.setAttr! co :pwd (CR/pwdify ^String (:passwd cfg) pkey)) )

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
  [^comzotohcljc.hhh.core.sys.Element co cfg]
  (let [ c (SU/nsb (:context cfg)) ]
    (.setAttr! co K_APP_CZLR (get cfg K_APP_CZLR))
    (.setAttr! co :contextPath (SU/strim c))
    (http-basic-config co cfg) ))

(defn- cfgHTTPS ^ServerConnector [^Server server port ^URL keyfile ^String pwd conf]
  ;; SSL Context Factory for HTTPS and SPDY
  (let [ sslxf (doto (SslContextFactory.)
                (.setKeyStorePath (-> keyfile (.toURI)(.toURL)(.toString)))
                (.setKeyStorePassword pwd)
                (.setKeyManagerPassword pwd))
         config (doto (HttpConfiguration. conf)
                  (.addCustomizer (SecureRequestCustomizer.)))
         https (doto (ServerConnector. server)
                 (.addConnectionFactory (SslConnectionFactory. sslxf "HTTP/1.1"))
                 (.addConnectionFactory (HttpConnectionFactory. config))) ]
    (doto https
      (.setPort port)
      (.setIdleTimeout (int 500000)))))

(defmethod comp-initialize :czc.hhh.io/JettyIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ conf (doto (HttpConfiguration.)
                (.setRequestHeaderSize 8192)  ;; from jetty examples
                (.setOutputBufferSize (int 32768)))
         keyfile (.getAttr co :serverKey)
         ^String host (.getAttr co :host)
         port (.getAttr co :port)
         pwdObj (.getAttr co :pwd)
         ws (.getAttr co :workers)
         ;;q (QueuedThreadPool. (if (pos? ws) ws 8))
         svr (Server.)
         cc  (if (nil? keyfile)
               (doto (JettyUtils/makeConnector svr conf)
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
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ ^comzotohcljc.hhh.core.sys.Element ctr (.parent ^Hierarchial co)
         ^Server jetty (.getAttr co :jetty)
         ^File app (.getAttr ctr K_APPDIR)
         ^String cp (SU/strim (.getAttr co :contextPath))
         ^WebAppContext
         webapp (JettyUtils/newWebAppContext app cp "czchhhiojetty" co)
         logDir (-> (File. app "WEB-INF/logs")(.toURI)(.toURL)(.toString))
         resBase (-> app (.toURI)(.toURL)(.toString)) ]
    ;; static resources are based from resBase, regardless of context
    (.setClassLoader webapp (.getAttr co K_APP_CZLR))
    (doto webapp
      (.setDescriptor (-> (File. app "WEB-INF/web.xml")(.toURI)(.toURL)(.toString)))
      (.setParentLoaderPriority true)
      (.setResourceBase resBase )
      (.setContextPath cp))
    ;;webapp.getWebInf()
    (.setHandler jetty webapp)
    (.start jetty)
    (ioes-started co)))


(defmethod ioes-stop :czc.hhh.io/JettyIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ ^Server svr (.getAttr co :jetty) ]
    (when-not (nil? svr)
      (CU/TryC
          (.stop svr) ))
    (ioes-stopped co)))

(defn make-http-result []
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :version "HTTP/1.1" )
    (.mm-s impl :code -1)
    (.mm-s impl :hds (NCMap.))
    (.mm-s impl :cookies (ArrayList.))
    (reify

      MuObj

      (setf! [_ k v] (.mm-s impl k v) )
      (seq* [_] (seq (.mm-m* impl)))
      (getf [_ k] (.mm-g impl k) )
      (clrf! [_ k] (.mm-r impl k) )
      (clear! [_] (.mm-c impl))

      HTTPResult
      (setRedirect [_ url] (.mm-s impl :redirect url))

      (setProtocolVersion [_ ver]  (.mm-s impl :version ver))
      (setStatus [_ code] (.mm-s impl :code code))
      (addCookie [_ c]
        (let [ ^List a (.mm-g impl :cookies) ]
          (when-not (nil? c)
            (.add a c))))

      (containsHeader [_ nm]
        (let [ ^NCMap m (.mm-g impl :hds) ]
          (.containsKey m nm)))

      (removeHeader [_ nm]
        (let [ ^NCMap m (.mm-g impl :hds) ]
          (.remove m nm)))

      (clearHeaders [_]
        (let [ ^NCMap m (.mm-g impl :hds) ]
          (.clear m)))

      (addHeader [_ nm v]
        (let [ ^NCMap m (.mm-g impl :hds) ^List a (.get m nm) ]
          (if (nil? a)
            (.put m nm (doto (ArrayList.) (.add v)))
            (.add a v))))

      (setHeader [_ nm v]
        (let [ ^NCMap m (.mm-g impl :hds)
               a (ArrayList.) ]
          (.add a v)
          (.put m nm a)))

      (setChunked [_ b] (.mm-s impl :chunked b))

      (setContent [_ data]
        (if-not (nil? data)
          (.mm-s impl :data data)) )

      )) )


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
         impl (CU/make-mmap)
         eid (SN/next-long) ]
    (reify

      Identifiable
      (id [_] eid)

      HTTPEvent

      (getId [_] eid)
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

      (bindSession [_ s] (.mm-s impl :ios s))
      (getSession [_] (.mm-g impl :ios))
      (emitter [_] co)
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

      (getResultObj [_] result)
      (replyResult [this]
        (let [ ^comzotohcljc.hhh.io.core.WaitEventHolder
               wevt (.release ^comzotohcljc.hhh.io.core.EmitterAPI co this) ]
          (when-not (nil? wevt)
            (.resumeOnResult wevt result))))


      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private http-eof nil)

