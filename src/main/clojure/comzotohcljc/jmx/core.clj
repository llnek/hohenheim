(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.jmx.core )

(require '[comzotohcljc.util.core :as CU])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkJMXrror [^String msg ^Throwable e]
  (throw (doto (JMXError. msg)
           (.initCause e))))

(defn- startRMI [impl]
  (let [ port (.mm-g impl :regoPort) ]
    (try
      (.mm-s impl :rmi (LocateRegistry/createRegistry ^long port))
    (catch Throwable e#
      (mkJMXrror (str "Failed to create RMI registry: " port) e#)))) )

(defn- startJMX [impl]
  (let [ hn (-> (InetAddress/getLocalHost)(.getHostName))
         ^long regoPort (.mm-g impl :regoPort)
         ^long port (.mm-g impl :port)
         ^String host (.mm-g impl :host)
         endpt (-> "service:jmx:rmi://{{host}}:{{sport}}/jndi/rmi://:{{rport}}/jmxrmi"
                 (StringUtils/replace "{{host}}" (if (SU/hgl? host) host hn))
                 (StringUtils/replace "{{sport}}" (str "" port))
                 (StringUtils/replace "{{rport}}" (str "" regoPort)))
         url (try
               (JMXServiceURL. endpt)
               (catch Throwable e#
                  (mkJMXrror (str "Malformed url: " endpt) e)))
         ^JMXConnectorServer
         conn (try
                (JMXConnectorServerFactory/newJMXConnectorServer
                  url
                  nil
                  (ManagementFactory/getPlatformMBeanServer))
                (catch Throwable e# 
                  (mkJMXrror (str "Failed to connect JMX") e))) ]
    (try
      (.start conn)
      (catch Throwable e#
        (mkJMXrror (str "Failed to start JMX") e)))

    (.mm-s impl :beanSvr (.getMBeanServer conn))
    (.mm-s impl :conn conn)))

(defn- doReg[ ^MBeanServer svr ^ObjectName objName ^JMXBean mbean]
  (try
    (.registerMBean svr mbean objName)
    (catch Throwable e#
      (mkJMXrror (str "Failed to register bean: " objName) e)))
  (info "registered jmx-bean: " objName)
  objName)

(defn make-jmxServer [host]
  (let [ impl (CU/make-mmap)
         objNames (atom []) ]
    (reify

      JMXServer

      (reset [_]
        (let [ bs (.mm-g impl :beanSvr) ]
          (doseq [ nm (seq @objNames) ]
            (CU/Try!
               (.unregisterMBean bs nm)) )
          (reset! objNames [])))

      (deregObj [obj]
        (.unregisterMBean bsvr (inferObjectName obj)))

      (dereg [_ objName]
        (.unregisterMBean bsvr objName))

      (reg [obj domain nname paths]
        (try
          (conj _objNames (doReg (inferObjectNameEx obj domain nname paths) (JMXBean.obj)))
          (catch Throwable e#
            (mkJMXrror (str "Failed to register object: " obj) e#))))

      ;; jconsole port
      (setRegistryPort [_ port]
        (.mm-s impl :rego-port port))

      (setServerPort[_ port]
        (.mm-s impl :port port))

      Startable

      (start [_]
      if (_serverPort <= 0) { _serverPort = _registryPort + 1 }
        (startRMI)
        (startJMX))

      (stop [this]
        (reset this)
        (when-not (nil? c)
          (CU/TryC
            (.stop c)))
        (.mm-s impl :conn nil)
        (when-not (nil? r)
          (CU/TryC
            (UnicastRemoteObject/unexportObject r true)))
        (.mm-s impl :rmi nil))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private core-eof nil)

