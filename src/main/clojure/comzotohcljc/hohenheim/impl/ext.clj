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

  comzotohcljc.hohenheim.impl.ext )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File))
(import '(com.zotoh.hohenheim.core ServiceError Job))


(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.impl.defaults])
(use '[comzotohcljc.hohenheim.etc.misc])
(use '[comzotohcljc.hohenheim.core.sys])

(use '[comzotohcljc.util.core :only (MutableObjectAPI) ] )
(use '[comzotohcljc.wflow.core])
(use '[comzotohcljc.wflow.user])

(require '[ comzotohcljc.util.scheduler :as SC])
(require '[ comzotohcljc.util.process :as PU ] )
(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.seqnum :as SN ] )
(require '[ comzotohcljc.util.str :as SU ] )
(require '[ comzotohcljc.util.meta :as MU ] )

(require '[clojure.data.json :as JS])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Job-Creator

(defprotocol AppMainAPI
  (contextualize [_ container] )
  (configure [_ options] )
  (initialize [_] )
  (dispose [_] ))


(defn- make-job "" [container evt]
  (let [ impl (CU/make-mmap)
         jid (SN/next-long) ]
    (with-meta
      (reify

        MutableObjectAPI

          (setf! [_ k v] (.mm-s impl k v))
          (clear! [_] (.mm-c impl))
          (seq* [_] (seq (.mm-m* impl)))
          (getf [_ k] (.mm-g impl k))
          (clrf! [_ k] (.mm-r impl k))

        Job

          (parent [_] container)
          (event [_] evt)
          (id [_] jid))

      { :typeid (keyword "czc.hhh.impl/Job") } )))

(defprotocol ^:private JobCreator
  (update [_ event options] ))

(defn- make-jobcreator "" [parObj]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        JobCreator

          (update [_ evt options]
            (let [ cz (if (.hasRouter evt)
                        (.routerClass evt)
                        (:router-class options))
                   job (make-job parObj evt) ]
              (try
                (let [ p (make-pipeline job cz)
                       q (if (nil? p) (make-OrphanFlow job) p) ]
                  (.start q))
                (catch Throwable e#
                  (-> (make-FatalErrorFlow job) (.start)))))))

      { :typeid (keyword "czc.hhh.impl/JobCreator") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ContainerAPI

(defprotocol ContainerAPI ""
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc cfg] )
  (reifyServices [_] )
  (enabled? [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-service-block [bk container cfg]
  (let [ obj (MU/make-obj (.id bk)) ]
    (synthesize-component obj { :ctx container :props cfg } )
    obj))

(defn- make-app-container [pod]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        Container
          (notifyObservers [_ evt] )
          (core [this]
            (.getAttr this K_SCHEDULER))

        Component

          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (setCtx! [_ x] (.mm-s impl :ctx x) )
          (getCtx [_] (.mm-g impl :ctx) )
          (version [_] "1.0")
          (parent [_] nil)
          (id [_] (.id pod) )

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
                   srg (.mm-g impl K_SVCS)
                   b (:enabled cfg) ]
              (if-not (or (false? b) (SU/nichts? svc))
                (let [ s (reifyService this svc cfg) ]
                  (.reg srg s)))))

          (reifyService [this svc cfg]
            (let [ root (.getf (.getCtx this) K_COMPS)
                   bks (.lookup root K_BLOCKS)
                   bk (.lookup bks (keyword svc)) ]
              (when (nil? bk)
                (throw (ServiceError. (str "No such Service: " svc "."))))
              (make-service-block bk this cfg))) )

    { :typeid (keyword "czc.hhh.ext/Container") } )) )

(defn make-container [pod]
  (let [ c (make-app-container pod)
         ctx (.getCtx pod)
         cl (.getf ctx K_APP_CZLR)
         root (.getf ctx K_COMPS)
         apps (.lookup root K_APPS)
         ps { K_APPDIR (File. (-> (.srcUrl pod) (.toURI))) } ]
    (comp-compose c apps)
    (comp-contextualize c ctx)
    (comp-configure c ps)
    (if (.enabled? c)
      (do (PU/coroutine (fn []
                          (do
                            (comp-initialize c)
                            (.start c))) cl) c)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-configure :czc.hhh.ext/Container
  [co props]
  (let [ appDir (K_APPDIR props)
         cfgDir (File. appDir DN_CONF)
         srg (make-component-registry :EventSources K_SVCS "1.0" co)
         mf (CU/load-javaprops (File. appDir MN_FILE))
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
      (.setAttr! K_SVCS srg)
      (.setAttr! K_ENVCONF envConf)
      (.setAttr! K_APPCONF appConf)
      (.setAttr! K_MFPROPS mf))
    (info "container: configured app: " (.id co))))


(defmethod comp-initialize :czc.hhh.ext/Container
  [co]
  (let [ env (.getAttr co K_ENVCONF)
         app (.getAttr co K_APPCONF)
         mf (.getAttr co K_MFPROPS)
         mCZ (SU/strim (.get mf "Main-Class"))
         reg (.getAttr co K_SVCS)
         jc (make-jobcreator co)
         sc (SC/make-scheduler co)
         cfg (:container env) ]

    (.setAttr! co K_SCHEDULER sc)
    (.setAttr! co K_JCTOR jc)

    (when (SU/nichts? mCZ) (warn "no main-class defined."))
    ;;(CU/test-nestr "Main-Class" mCZ)

    (when (SU/hgl? mCZ)
      (let [ obj (MU/make-obj mCZ) ]
        (.contextualize obj co)
        (.configure obj app)
        (.initialize obj)
        (info "application main-class " mCZ " created and invoked")))

    (let [ svcs (:services env) ]
      (if (empty? svcs)
          (warn "No system service defined in env.conf.")
          (.reifyServices co)))

    ;; start the scheduler
    (.start sc cfg)

    (info "Initialized app: " (.id co))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(def ^:private ext-eof nil)

