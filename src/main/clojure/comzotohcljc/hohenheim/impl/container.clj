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

(defprotocol ContainerAPI
  (reifyOneService [_ sid cfg] )
  (reifyService [_ svc cfg] )
  (reifyServices [_] )
  (core [_] )
  (enabled? [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-app-container ^{ :doc "" }
  [pod]
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
                   c (get env :container) ]
              (if (nil? c)
                false
                (let [ v (get c :enabled) ]
                  (if (nil? v) true v)))))

          (reifyServices [this]
            (let [ env (.mm-g impl K_ENVCONF)
                   s (get env :services) ]
              (if (empty? s)
                  (warn "No system service \"depend\" found in env.conf.")
                  (doseq [ [k v] (seq s) ]
                    (reifyOneService this k v)))))

          (reifyOneService [this k cfg]
            (let [ svc (SU/nsb (get cfg :service))
                   b (get cfg :enabled) ]
              (if (or (false? b) (SU/nichts? svc))
                (info "System service \"" svc "\" is disabled.")
                (reifyService this svc cfg))))

                ;;_svcReg.add(key, rc._2 )
        ;;;; TODO
          (reifyService [this svc cfg] nil) )

    { :typeid :Container } )) )

(defn make-container [pod]
  (let [ c (make-app-container pod)
         ctx (.getCtx pod)
         cl (get ctx K_APP_CZLR)
         root (get ctx K_COMPS)
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
  (let [ appDir (get props K_APPDIR)
         cfgDir (File. appDir DN_CONF)
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
    (-> co
      (.setAttr! K_ENVCONF envConf)
      (.setAttr! K_APPCONF appConf)
      (.setAttr! K_MFPROPS mf))
    (info "container: configured app: " (.id co))))


(defmethod comp-initialize :Container [co]
  (let [ env (.getAttr co K_ENVCONF)
         app (.getAttr co K_APPCONF)
         mf (.getAttr co K_MFPROPS)
         mCZ (SU/nsb (.get mf "Main-Class"))
         reg (make-component-registry (CU/uid) "1.0" co)
         jc (make-jobcreator co)
         sc (make-scheduler co)
         cfg (get env :container) ]

    (synthesize-component reg)

    (.setAttr! co K_SCHEDULER sc)
    (.setAttr! co K_SVCS reg)
    (.setAttr! co K_JCTOR jc)

    (when (SU/nichts? mCZ) (warn "no main-class defined."))
    ;;(CU/test-nestr "Main-Class" mCZ)

    (when (SU/hgl? mCZ)
      (let [ obj (MU/make-obj mCZ) ]
        (.contextualize obj co)
        (.configure obj app)
        (.initialize obj)
        (info "application main-class " mCZ " created and invoked")))

    (let [ svcs (get env :services) ]
      (if (empty? svcs)
          (warn "No system service \"depend\" found in env.conf.")
          (.reifyServices co)))

    (.start sc cfg)

    (info "Initialized app: " (.id co))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(def ^:private container-eof nil)

