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

  comzotohcljc.hhh.impl.ext )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.util Properties))
(import '(java.net URL))
(import '(java.io File))
(import '(com.zotoh.hohenheim.runtime AppMain))
(import '(com.zotoh.frwk.core
  Versioned Hierarchial Startable Disposable
  Identifiable ))
(import '(com.zotoh.hohenheim.core
  Container ConfigError ServiceError ))

(import '(com.zotoh.hohenheim.io IOEvent))
(import '(com.zotoh.frwk.util CoreUtils))
(import '(com.zotoh.wflow.core Job))
(import '(com.zotoh.wflow Pipeline))


(use '[clojure.tools.logging :only (info warn error debug)])

;;(use '[comzotohcljc.hhh.io.core :only (make-emitter)])
(use '[comzotohcljc.hhh.io.core :rename {enabled? io-enabled?} ])
(use '[comzotohcljc.hhh.io.loops])
(use '[comzotohcljc.hhh.io.mails])
(use '[comzotohcljc.hhh.io.files])
(use '[comzotohcljc.hhh.io.jms])
(use '[comzotohcljc.hhh.io.http])
(use '[comzotohcljc.hhh.io.netty])
(use '[comzotohcljc.hhh.io.events])
(use '[comzotohcljc.hhh.mvc.handler])

(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.impl.defaults
       :rename {enabled? blockmeta-enabled?
                start kernel-start
                stop kernel-stop } ])
(use '[comzotohcljc.hhh.etc.misc])
(use '[comzotohcljc.hhh.core.sys])

(use '[comzotohcljc.util.core :only (MuObj) ] )

(require '[ comzotohcljc.util.scheduler :as SC])
(require '[ comzotohcljc.util.process :as PU ] )
(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.seqnum :as SN ] )
(require '[ comzotohcljc.util.str :as SU ] )
(require '[ comzotohcljc.util.meta :as MU ] )

(require '[clojure.data.json :as JS])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(defprotocol CljAppMain
  ""
  (contextualize [_ ctr] )
  (configure [_ options] )
  (initialize [_] )
  (start [_] )
  (stop [_])
  (dispose [_] ))

(defn- make-job "" [_container evt]
  (let [ impl (CU/make-mmap)
         jid (SN/next-long) ]
    (with-meta
      (reify

        MuObj

          (setf! [_ k v] (.mm-s impl k v))
          (clear! [_] (.mm-c impl))
          (seq* [_] (seq (.mm-m* impl)))
          (getf [_ k] (.mm-g impl k))
          (clrf! [_ k] (.mm-r impl k))

        Job

          (container [_] _container)
          (event [_] evt)
          (id [_] jid))

      { :typeid (keyword "czc.hhh.impl/Job") } )))

(defprotocol ^:private JobCreator
  ""
  (update [_ event options] ))

(defn- make-jobcreator ""
  ^comzotohcljc.hhh.impl.ext.JobCreator [parObj]
  (let [ impl (CU/make-mmap) ]
    (info "about to synthesize a job-creator...")
    (with-meta
      (reify

        JobCreator

          (update [_  evt options]
            (let [ ^comzotohcljc.hhh.core.sys.Thingy
                   src (.emitter ^IOEvent evt)
                   c0 (.getAttr src :router)
                   c1 (:router options)
                   job (make-job parObj evt) ]
              (try
                (let [ p (Pipeline. job (if (SU/hgl? c0) c0 c1))
                       q (if (nil? p) (make-OrphanFlow job) p) ]
                  (.start ^Pipeline q))
                (catch Throwable e#
                  (-> (make-FatalErrorFlow job) (.start)))))))

      { :typeid (keyword "czc.hhh.impl/JobCreator") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ContainerAPI

(defprotocol ^:private ContainerAPI
  ""
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc cfg] )
  (reifyServices [_] )
  (enabled? [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-service-block [^Identifiable bk container cfg]
  (let [ eid (.id bk)
         ^comzotohcljc.hhh.core.sys.Thingy
         obj (if (= :czc.hhh.io/JettyIO eid)
               (make-servlet-emitter container)
               (make-emitter container eid))
         hid (:handler cfg)
         mm (meta obj) ]
    (info "about to synthesize an emitter: " eid)
    (info "emitter meta: " mm)
    (info "is emitter = " (isa?  (:typeid mm) :czc.hhh.io/Emitter))
    (synthesize-component obj { :ctx container :props cfg } )
    (.setAttr! obj :router hid)
    (info "emitter synthesized - OK. handler => " hid)
    obj))

(defn- make-app-container [pod]
  (let [ impl (CU/make-mmap) ]
    (info "about to create an app-container...")
    (with-meta
      (reify

        Thingy

          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (setCtx! [_ x] (.mm-s impl :ctx x) )
          (getCtx [_] (.mm-g impl :ctx) )

        Container
          (notifyObservers [this evt]
            (let [ ^comzotohcljc.hhh.impl.ext.JobCreator
                   jc (.getAttr this K_JCTOR) ]
              (.update jc evt {})))
          (getAppKey [_] (.appKey ^comzotohcljc.hhh.impl.defaults.PODMeta pod))
          (getAppDir [this] (.getAttr this K_APPDIR))
          (core [this]
            (.getAttr this K_SCHEDULER))

        Versioned
          (version [_] "1.0")

        Hierarchial
          (parent [_] nil)

        Identifiable
          (id [_] (.id ^Identifiable pod) )

        Startable
          (start [_]
            (let [ ^comzotohcljc.hhh.core.sys.Registry
                   srg (.mm-g impl K_SVCS)
                   main (.mm-g impl :main-app) ]
              (info "container starting all services...")
              (doseq [ [k v] (seq* srg) ]
                (.start ^Startable v))
              (info "container starting main app...")
              (cond
                (satisfies? CljAppMain main)
                (.start ^comzotohcljc.hhh.impl.ext.CljAppMain main)
                (instance? AppMain main)
                (.start ^AppMain main)
                :else nil)))

          (stop [_]
            (let [ ^comzotohcljc.hhh.core.sys.Registry
                   srg (.mm-g impl K_SVCS)
                   main (.mm-g impl :main-app) ]
              (info "container stopping all services...")
              (doseq [ [k v] (seq* srg) ]
                (.stop ^Startable v))
              (info "container stopping main app...")
              (cond
                (satisfies? CljAppMain main)
                (.stop ^comzotohcljc.hhh.impl.ext.CljAppMain main)
                (instance? AppMain main)
                (.stop ^AppMain main)
                :else nil)))

        Disposable
          (dispose [_]
            (let [ ^comzotohcljc.hhh.core.sys.Registry
                   srg (.mm-g impl K_SVCS)
                   main (.mm-g impl :main-app) ]
              (doseq [ [k v] (seq* srg) ]
                (.dispose ^Disposable v))
              (info "container dispose() - main app getting disposed.")
              (cond
                (satisfies? CljAppMain main)
                (.stop ^comzotohcljc.hhh.impl.ext.CljAppMain main)
                (instance? AppMain main)
                (.stop ^AppMain main)
                :else nil)))

        ContainerAPI

          (enabled? [_]
            (let [ env (.mm-g impl K_ENVCONF)
                   c (:container env) ]
              (if (false? (:enabled c))
                false
                true)))

          (reifyServices [this]
            (let [ env (.mm-g impl K_ENVCONF)
                   s (:services env) ]
              (if-not (empty? s)
                  (doseq [ [k v] (seq s) ]
                    (reifyOneService this k v)))))

          (reifyOneService [this nm cfg]
            (let [ svc (SU/nsb (:service cfg))
                   ^comzotohcljc.hhh.core.sys.Registry
                   srg (.mm-g impl K_SVCS)
                   b (:enabled cfg) ]
              (if-not (or (false? b) (SU/nichts? svc))
                (let [ s (reifyService this svc cfg) ]
                  (.reg srg s)))))

          (reifyService [this svc cfg]
            (let [^comzotohcljc.util.core.MuObj ctx (.getCtx this)
                   ^comzotohcljc.hhh.core.sys.Registry
                   root (.getf ctx K_COMPS)
                   ^comzotohcljc.hhh.core.sys.Registry
                   bks (.lookup root K_BLOCKS)
                   bk (.lookup bks (keyword svc)) ]
              (when (nil? bk)
                (throw (ServiceError. (str "No such Service: " svc "."))))
              (make-service-block bk this cfg))) )

    { :typeid (keyword "czc.hhh.ext/Container") } )) )

(defn make-container [^comzotohcljc.hhh.core.sys.Thingy pod]
  (let [ c (make-app-container pod)
         ^comzotohcljc.util.core.MuObj ctx (.getCtx pod)
         cl (.getf ctx K_APP_CZLR)
         ^comzotohcljc.hhh.core.sys.Registry
         root (.getf ctx K_COMPS)
         apps (.lookup root K_APPS)
         ^URL url (.srcUrl ^comzotohcljc.hhh.impl.defaults.PODMeta pod)
         ps { K_APPDIR (File. (.toURI  url)) K_APP_CZLR cl } ]
    (comp-compose c apps)
    (comp-contextualize c ctx)
    (comp-configure c ps)
    (if (.enabled? ^comzotohcljc.hhh.impl.ext.ContainerAPI c)
      (do (PU/coroutine (fn []
                          (do
                            (comp-initialize c)
                            (.start ^Startable c))) cl) c)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-configure :czc.hhh.ext/Container
  [^comzotohcljc.hhh.core.sys.Thingy co props]
  (let [ ^File appDir (K_APPDIR props)
         cfgDir (File. appDir ^String DN_CONF)
         srg (make-component-registry :EventSources K_SVCS "1.0" co)
         mf (CU/load-javaprops (File. appDir ^String MN_FILE))
         envConf (JS/read-str (FileUtils/readFileToString
                                (File. cfgDir "env.conf"))
                              :key-fn keyword)
         appConf (JS/read-str (FileUtils/readFileToString
                                (File. cfgDir "app.conf"))
                              :key-fn keyword) ]
    ;;WebPage.setup(new File(appDir))
    ;;maybeLoadRoutes(cfgDir)
    ;;_ftlCfg = new FTLCfg()
    ;;_ftlCfg.setDirectoryForTemplateLoading( new File(_appDir, DN_PAGES+"/"+DN_TEMPLATES))
    ;;_ftlCfg.setObjectWrapper(new DefaultObjectWrapper())
    (synthesize-component srg {} )
    (doto co
      (.setAttr! K_APPDIR appDir)
      (.setAttr! K_SVCS srg)
      (.setAttr! K_ENVCONF_FP (File. cfgDir "env.conf"))
      (.setAttr! K_APPCONF_FP (File. cfgDir "app.conf"))
      (.setAttr! K_ENVCONF envConf)
      (.setAttr! K_APPCONF appConf)
      (.setAttr! K_MFPROPS mf))
    (info "container: configured app: " (.id ^Identifiable co))))


(defn- doCljApp [ctr opts ^comzotohcljc.hhh.impl.ext.CljAppMain obj]
  (.contextualize obj ctr)
  (.configure obj opts)
  (.initialize obj))

(defn- doJavaApp [^comzotohcljc.hhh.core.sys.Thingy ctr ^AppMain obj]
  (let [ ^File cfg (.getAttr ctr K_APPCONF_FP)
         json (CoreUtils/readJson cfg) ]
  (.contextualize obj ctr)
  (.configure obj json)
  (.initialize obj)) )

(defmethod comp-initialize :czc.hhh.ext/Container
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ env (.getAttr co K_ENVCONF)
         app (.getAttr co K_APPCONF)
         ^Properties mf (.getAttr co K_MFPROPS)
         mCZ (SU/strim (.get mf "Main-Class"))
         reg (.getAttr co K_SVCS)
         jc (make-jobcreator co)
         ^comzotohcljc.util.scheduler.SchedulerAPI
         sc (SC/make-scheduler co)
         cfg (:container env) ]

    (.setAttr! co K_SCHEDULER sc)
    (.setAttr! co K_JCTOR jc)

    (when (SU/nichts? mCZ) (warn "no main-class defined."))
    ;;(CU/test-nestr "Main-Class" mCZ)

    (when (SU/hgl? mCZ)
      (let [ obj (MU/make-obj mCZ) ]
        (cond
          (satisfies? CljAppMain obj)
          (doCljApp co app obj)
          (instance? AppMain obj)
          (doJavaApp co obj)
          :else (throw (ConfigError. (str "Invalid Main Class " mCZ))))
        (.setAttr! co :main-app obj)
        (info "application main-class " mCZ " created and invoked")))

    (let [ svcs (:services env) ]
      (if (empty? svcs)
          (warn "No system service defined in env.conf.")
          (.reifyServices ^comzotohcljc.hhh.impl.ext.ContainerAPI  co)))

    ;; start the scheduler
    (.activate sc cfg)

    (info "Initialized app: " (.id ^Identifiable co))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(def ^:private ext-eof nil)

