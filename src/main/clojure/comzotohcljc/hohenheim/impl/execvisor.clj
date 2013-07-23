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
  comzotohcljc.hohenheim.impl.execvisor )

(import '(org.apache.commons.io.filefilter DirectoryFileFilter))
(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.io File FileFilter))
(import '(java.util Date))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.impl.defaults])

(use '[comzotohcljc.hohenheim.impl.kernel :only (make-kernel make-podmeta) ])
(use '[comzotohcljc.hohenheim.impl.deployer :only (make-deployer) ])

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.metautils :as MU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.win32ini :as WI ] )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private START-TIME (.getTime (Date.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol ExecvisorAPI ""
  (start [_] )
  (stop [_] )
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


(defprotocol BlockMetaAPI ""
  (enabled? [_] )
  (metaUrl [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- chkManifest [execv app des mf]
  (let [ ctx (.getCtx execv)
         root (.getf ctx K_COMPS)
         apps (.lookup root K_APPS)
         ps (CU/load-javaprops mf)
         ver (.getProperty ps "Implementation-Version" "")
         cz (.getProperty ps "Main-Class" "") ]

    (CU/test-nestr "POD-MainClass" cz)
    (CU/test-nestr "POD-Version" ver)

    ;;ps.gets("Manifest-Version")
    ;;.gets("Implementation-Title")
    ;;.gets("Implementation-Vendor-URL")
    ;;.gets("Implementation-Vendor")
    ;;.gets("Implementation-Vendor-Id")

    (let [ m (-> (make-podmeta app ver nil cz (-> des (.toURI) (.toURL)))
                 (synthesize-component { :ctx ctx })) ]
      (.setf! (.getCtx m) K_EXECV execv)
      (.reg apps m)
      m)))

(defn- inspect-pod [execv des]
  (let [ app (FilenameUtils/getBaseName (CU/nice-fpath des))
         mf (File. des MN_FILE) ]
    (try
        (precondDir (File. des POD_INF))
        (precondDir (File. des POD_CLASSES))
        (precondDir (File. des POD_LIB))
        (precondDir (File. des META_INF))
        (precondFile (File. des CFG_APP_CF))
        (precondFile (File. des CFG_ENV_CF))
        (precondDir (File. des DN_CONF))
        (precondFile mf)
        (chkManifest execv app des mf)
      (catch Throwable e#
        (error e#)))) )

(defn- inspect-pods [co]
  (let [ fs (-> (.getf (.getCtx co) K_PLAYDIR)
                (.listFiles (cast FileFilter DirectoryFileFilter/DIRECTORY))) ]
    (doseq [ f (seq fs) ]
      (inspect-pod co f)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-execvisor "" [parObj]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        Component

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-s impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )

          (version [_] "1.0")
          (parent [_] parObj)
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
          (kill9 [this] (.stop parObj))
          (start [this]
            (let [ root (.getf (getCtx this) K_COMPS)
                   k (.lookup root K_KERNEL) ]
              (inspect-pods this)
              (.start k)))
          (stop [this]
            (let [ root (.getf (getCtx this) K_COMPS)
                   k (.lookup root K_KERNEL) ]
              (.stop k)))  )

       { :typeid :Execvisor } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :Execvisor [co]
  (let [ cf (.getf (.getCtx co) K_PROPS)
         comps (.getSection cf K_COMPS)
         regs (.getSection cf K_REGS)
         jmx  (.getSection cf K_JMXMGM) ]

    (CU/test-nonil "conf file: components" comps)
    (CU/test-nonil "conf file: registries" regs)
    (CU/test-nonil "conf file: jmx mgmt" jmx)

    (System/setProperty "file.encoding" "utf-8")

    (let [ home (.homeDir co)
           sb (doto (File. home DN_BOXX)
                  (.mkdir))
           bks (doto (File. home DN_BLOCKS)
                  (.mkdir))
           tmp (doto (File. home DN_TMP)
                  (.mkdir))
           db (doto (File. home DN_DBS)
                  (.mkdir))
           log (doto (File. home DN_LOGS)
                  (.mkdir))
           pods (doto (File. home DN_PODS)
                  (.mkdir)) ]
      (precondDir pods)
      (precondDir sb)
      (precondDir log)
      (precondDir tmp)
      (precondDir db)
      (precondDir bks)
      (-> (.getCtx co)
          (.setf! K_PODSDIR pods)
          (.setf! K_PLAYDIR sb)
          (.setf! K_LOGDIR log)
          (.setf! K_DBSDIR db)
          (.setf! K_TMPDIR tmp)
          (.setf! K_BKSDIR bks)) )
    ;;(start-jmx)
    (let [ root (make-component-registry :SystemRegistry K_COMPS "1.0" co)
           bks (make-component-registry :BlocksRegistry K_BLOCKS "1.0" nil)
           apps (make-component-registry :AppsRegistry K_APPS "1.0" nil)
           deployer (make-deployer)
           knl (make-kernel) ]
      (.setf! (.getCtx co) K_COMPS root)
      (.reg root deployer)
      (.reg root knl)
      (.reg root apps)
      (.reg root bks)
      (.setf! (.getCtx co) K_EXECV co)
      (->> { :ctx (.getCtx co) }
        (synthesize-component root)
        (synthesize-component bks)
        (synthesize-component apps)
        (synthesize-component deployer)
        (synthesize-component knl)) )

    ))


(defn- make-blockmeta [url]
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :id (keyword (CU/uid)))
    (.mm-s impl K_META url)
    (with-meta
      (reify

        Component

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-s impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (version [_] "1.0")
          (parent [_] nil)
          (id [_] (.mm-g impl :id))

        BlockMetaAPI

          (enabled? [_] (true? (.mm-g impl :active)))
          (metaUrl [_] (.mm-g impl K_META)) )

      { :typeid :BlockMeta } )))


(defmethod comp-initialize :BlockMeta [co]
  (let [ cfg (WI/parse-inifile (.metaUrl co))
         inf (.getSection cfg "info") ]
    (CU/test-nonil "Invalid block-meta file, no info section." inf)

    (let [ cz (SU/strim (.optString "info" "block-type" "")) ]
      (when (SU/hgl? cz)
        (.setAttr! co :id (keyword cz))
        (try
          (MU/load-class cz)
          (.setAttr! co :active true)
          (catch Throwable e#
            (warn e#))))
      (.setAttr! co :version (SU/strim (.optString "info" "version" "")))
      (.setAttr! co :name (SU/strim (.optString "info" "name" "")))

      co)))

(defmethod comp-initialize :BlocksRegistry [co]
  (let [ ctx (.getCtx co)
         bDir (.getf ctx K_BKSDIR)
         fs (FileUtils/listFiles bDir (into-array String ["meta"]) false) ]
    (doseq [ f (seq fs) ]
      (let [ b (-> (make-blockmeta (-> f (.toURI)(.toURL)))
                   (synthesize-component {}) ) ]
        (.reg co b)
        (info "added one block: " (.id b)) ))))




















(def ^:private execvisor-eof nil)

