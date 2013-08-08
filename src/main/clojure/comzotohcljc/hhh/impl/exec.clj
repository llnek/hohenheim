;
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

  comzotohcljc.hhh.impl.exec )

(import '(org.apache.commons.io.filefilter DirectoryFileFilter))
(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File FileFilter))
(import '(java.net URL))
(import '(java.util Date))
(import '(com.zotoh.frwk.io IOUtils))
(import '(com.zotoh.hohenheim.core
  Startable Versioned Hierarchial Identifiable))
(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.impl.defaults])

(use '[comzotohcljc.hhh.impl.sys :only (make-kernel make-podmeta make-deployer) ])

(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.meta :as MU ] )
(require '[ comzotohcljc.util.str :as SU ] )
(require '[ comzotohcljc.util.ini :as WI ] )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)



(def ^:private START-TIME (.getTime (Date.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol ExecvisorAPI ""
  (homeDir [_] )
  (confDir [_] )
  (podsDir [_] )
  (playDir [_] )
  (logDir [_] )
  (tmpDir [_] )
  (dbDir [_] )
  (blocksDir [_] )
  (getStartTime [_] )
  (kill9 [_] )
  (getUpTimeInMillis [_] ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- chkManifest

  [^comzotohcljc.hhh.core.sys.Thingy execv
   app
   ^File des
   mf]

  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx execv)
         ^comzotohcljc.hhh.core.sys.Registry
         root (.getf ctx K_COMPS)
         ^comzotohcljc.hhh.core.sys.Registry
         apps (.lookup root K_APPS)
         ps (CU/load-javaprops mf)
         ver (.getProperty ps "Implementation-Version" "")
         cz (.getProperty ps "Main-Class" "") ]

    (CU/test-nestr "POD-MainClass" cz)
    (CU/test-nestr "POD-Version" ver)

    (info "checking manifest for app: " app ", version: " ver ", main-class: " cz)

    ;;ps.gets("Manifest-Version")
    ;;.gets("Implementation-Title")
    ;;.gets("Implementation-Vendor-URL")
    ;;.gets("Implementation-Vendor")
    ;;.gets("Implementation-Vendor-Id")

    (let [ ^comzotohcljc.hhh.core.sys.Thingy
           m (-> (make-podmeta app ver nil cz (-> des (.toURI) (.toURL)))
                 (synthesize-component { :ctx ctx })) ]
      (.setf! ^comzotohcljc.util.core.MuObj (.getCtx m) K_EXECV execv)
      (.reg apps m)
      m)))

(defn- inspect-pod [execv ^File des]
  (let [ app (FilenameUtils/getBaseName (CU/nice-fpath des))
         mf (File. des ^String MN_FILE) ]
    (info "About to inspect app: " app)
    (info "app-dir: " des)
    (CU/TryC
        (precondDir (File. des ^String POD_INF))
        (precondDir (File. des ^String POD_CLASSES))
        (precondDir (File. des ^String POD_LIB))
        (precondDir (File. des ^String META_INF))
        (precondFile (File. des ^String CFG_APP_CF))
        (precondFile (File. des ^String CFG_ENV_CF))
        (precondDir (File. des ^String DN_CONF))
        (precondFile mf)
        (chkManifest execv app des mf) )))

;; check all apps to ensure they are kosher.
(defn- inspect-pods [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         ^FileFilter ff DirectoryFileFilter/DIRECTORY
         ^File pd (.getf ctx K_PLAYDIR) ]
    (doseq [ f (seq (.listFiles pd ff)) ]
      (inspect-pod co f)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-execvisor "" [parObj]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        Thingy

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )

        Versioned
          (version [_] "1.0")

        Hierarchial
          (parent [_] parObj)

        Identifiable
          (id [_] K_EXECV )

        ExecvisorAPI

          (getStartTime [_] START-TIME)
          (getUpTimeInMillis [_]
            (- (System/currentTimeMillis) START-TIME))
          (homeDir [this] (maybeDir (getCtx this) K_BASEDIR))
          (confDir [this] (maybeDir (getCtx this) K_CFGDIR))
          (podsDir [this] (maybeDir (getCtx this) K_PODSDIR))
          (playDir [this] (maybeDir (getCtx this) K_PLAYDIR))
          (logDir [this] (maybeDir (getCtx this) K_LOGDIR))
          (tmpDir [this] (maybeDir (getCtx this) K_TMPDIR))
          (dbDir [this] (maybeDir (getCtx this) K_DBSDIR))
          (blocksDir [this] (maybeDir (getCtx this) K_BKSDIR))
          (kill9 [this] (.stop ^Startable parObj))

        Startable
          (start [this]
            (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                   ^comzotohcljc.hhh.core.sys.Registry
                   root (.getf ctx K_COMPS)
                   ^Startable k (.lookup root K_KERNEL) ]
              (inspect-pods this)
              (.start k)))
          (stop [this]
            (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                   ^comzotohcljc.hhh.core.sys.Registry
                   root (.getf ctx K_COMPS)
                   ^Startable k (.lookup root K_KERNEL) ]
              (.stop k)))  )

       { :typeid (keyword "czc.hhh.impl/Execvisor") } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- start-jmx []
  (info "no JMS yet")
)

(defmethod comp-initialize :czc.hhh.impl/Execvisor
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         ^comzotohcljc.util.ini.IWin32Conf
         cf (.getf ctx K_PROPS)
         comps (.getSection cf K_COMPS)
         regs (.getSection cf K_REGS)
         jmx  (.getSection cf K_JMXMGM) ]

    (info "initializing component: Execvisor: " co)
    (CU/test-nonil "conf file: components" comps)
    (CU/test-nonil "conf file: registries" regs)
    (CU/test-nonil "conf file: jmx mgmt" jmx)

    (System/setProperty "file.encoding" "utf-8")

    (let [ ^File home (.homeDir ^comzotohcljc.hhh.impl.exec.ExecvisorAPI co)
           sb (doto (File. home ^String DN_BOXX)
                  (.mkdir))
           bks (doto (File. home ^String DN_BLOCKS)
                  (.mkdir))
           tmp (doto (File. home ^String DN_TMP)
                  (.mkdir))
           db (doto (File. home ^String DN_DBS)
                  (.mkdir))
           log (doto (File. home ^String DN_LOGS)
                  (.mkdir))
           pods (doto (File. home ^String DN_PODS)
                  (.mkdir)) ]
      (precondDir pods)
      (precondDir sb)
      (precondDir log)
      (precondDir tmp)
      (precondDir db)
      (precondDir bks)
      (doto ^comzotohcljc.util.core.MuObj (.getCtx co)
          (.setf! K_PODSDIR pods)
          (.setf! K_PLAYDIR sb)
          (.setf! K_LOGDIR log)
          (.setf! K_DBSDIR db)
          (.setf! K_TMPDIR tmp)
          (.setf! K_BKSDIR bks)) )
    (start-jmx)
    (let [ ^comzotohcljc.hhh.core.sys.Registry
           root (make-component-registry :SystemRegistry K_COMPS "1.0" co)
           bks (make-component-registry :BlocksRegistry K_BLOCKS "1.0" nil)
           apps (make-component-registry :AppsRegistry K_APPS "1.0" nil)
           deployer (make-deployer)
           knl (make-kernel) ]

      (.setf! ^comzotohcljc.util.core.MuObj (.getCtx co) K_COMPS root)
      (.reg root deployer)
      (.reg root knl)
      (.reg root apps)
      (.reg root bks)
      (.setf! ^comzotohcljc.util.core.MuObj (.getCtx co) K_EXECV co)
      (let [ options { :ctx (.getCtx co) } ]
        (synthesize-component root options)
        (synthesize-component bks options)
        (synthesize-component apps options)
        (synthesize-component deployer options)
        (synthesize-component knl options)) )

    ))

(defn- make-blockmeta "" [^URL url]
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :id (keyword (CU/uid)))
    (.mm-s impl K_META url)
    ;; url points to block-meta file
    (with-meta
      (reify

        Thingy

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )

        Versioned
          (version [_] "1.0")

        Hierarchial
          (parent [_] nil)

        Identifiable
          (id [_] (.mm-g impl :id))

        BlockMeta

          (enabled? [_] (true? (.mm-g impl :active)))
          (metaUrl [_] (.mm-g impl K_META)) )

      { :typeid (keyword "czc.hhh.impl/BlockMeta") } )))


(defmethod comp-initialize :czc.hhh.impl/BlockMeta
  [^comzotohcljc.hhh.impl.defaults.BlockMeta bk]
  (let [ ^URL url (.metaUrl bk)
         ^comzotohcljc.util.ini.IWin32Conf cfg (WI/parse-inifile url)
         inf (.getSection cfg "info") ]
    (CU/test-nonil "Invalid block-meta file, no info section." inf)
    (info "initializing BlockMeta: " url)
    (let [ cz (SU/strim (.optString cfg "info" "block-type" ""))
           ^comzotohcljc.hhh.core.sys.Thingy co bk  ]
      (when (SU/hgl? cz)
        (.setAttr! co :id (keyword cz))
        (CU/TryC
          (MU/load-class cz)
          (.setAttr! co :active true) ))
      (.setAttr! co :version (SU/strim (.optString cfg "info" "version" "")))
      (.setAttr! co :name (SU/strim (.optString cfg "info" "name" "")))
      co)))

(defmethod comp-initialize :czc.hhh.impl/BlocksRegistry
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         bDir (.getf ctx K_BKSDIR)
         fs (IOUtils/listFiles ^File bDir "meta" false) ]
    (doseq [ ^File f (seq fs) ]
      (let [ ^comzotohcljc.hhh.core.sys.Thingy
             b (-> (make-blockmeta (-> f (.toURI)(.toURL)))
                   (synthesize-component {}) ) ]
        (.reg ^comzotohcljc.hhh.core.sys.Registry co b)
        (info "added one block: " (.id ^Identifiable b)) ))))




















(def ^:private exec-eof nil)

