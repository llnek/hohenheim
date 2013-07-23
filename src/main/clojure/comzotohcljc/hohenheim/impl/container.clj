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
  comzotohcljc.hohenheim.impl.container )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File))
(import '(com.zotoh.hohenheim.core ServiceError))

(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.impl.defaults])
(use '[comzotohcljc.hohenheim.impl.jobcreator])
(use '[comzotohcljc.hohenheim.impl.scheduler])

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.metautils :as MU ] )
(require '[ comzotohcljc.util.procutils :as PU ] )

(require '[clojure.data.json :as JS])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ContainerAPI ""
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc cfg] )
  (reifyServices [_] )
  (core [_] )
  (enabled? [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-service-block [bk container cfg]
  (let [ cz (.id bk)
         obj (MU/make-obj cz) ]
    (synthesize-component obj { :ctx container :props cfg } )
    obj))


(defn- make-app-container [pod]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

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
              (if (empty? s)
                  (warn "No system service \"depend\" found in env.conf.")
                  (doseq [ [k v] (seq s) ]
                    (reifyOneService this k v)))))

          (reifyOneService [this nm cfg]
            (let [ svc (SU/nsb (:service cfg))
                   srg (.mm-g impl K_SVCS)
                   b (:enabled cfg) ]
              (if (or (false? b) (SU/nichts? svc))
                (info "service \"" svc "\" is disabled.")
                (let [ s (reifyService this svc cfg) ]
                  (.reg srg s)
                  (info "service \"" svc "\" synthesis - OK.")))))

          (reifyService [this svc cfg]
            (let [ root (.getf (.getCtx this) K_COMPS)
                   bks (.lookup root K_BLOCKS)
                   bk (.lookup bks (keyword svc)) ]
              (when (nil? bk)
                (throw (ServiceError. (str "No such Service: " svc "."))))
              (make-service-block bk this cfg)))
        )

    { :typeid :Container } )) )

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

(defmethod comp-configure :Container [co props]
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
    (-> co
      (.setAttr! K_SVCS srg)
      (.setAttr! K_ENVCONF envConf)
      (.setAttr! K_APPCONF appConf)
      (.setAttr! K_MFPROPS mf))
    (info "container: configured app: " (.id co))))


(defmethod comp-initialize :Container [co]
  (let [ env (.getAttr co K_ENVCONF)
         app (.getAttr co K_APPCONF)
         mf (.getAttr co K_MFPROPS)
         mCZ (SU/strim (.get mf "Main-Class"))
         reg (.getAttr co K_SVCS)
         jc (make-jobcreator co)
         sc (make-scheduler co)
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










(def ^:private container-eof nil)

