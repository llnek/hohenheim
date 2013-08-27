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

  comzotohcljc.hhh.impl.sys )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(com.zotoh.hohenheim.loaders AppClassLoader))
(import '(com.zotoh.frwk.core
  Identifiable Hierarchial Versioned Startable))
(import '(java.net URL))
(import '(java.io File))
(import '(java.security SecureRandom))
(import '(java.util.zip ZipFile))
(import '(com.zotoh.frwk.io IOUtils))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.impl.ext])
(use '[comzotohcljc.hhh.impl.defaults
       :rename {enabled? blockmeta-enabled?
                start kernel-start
                stop kernel-stop}])

(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.str :as SU ] )
(require '[ comzotohcljc.util.process :as PU ] )
(require '[ comzotohcljc.util.files :as FU ] )
(require '[ comzotohcljc.util.mime :as MI ] )
(require '[ comzotohcljc.util.seqnum :as SN ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deployer

(defn make-deployer "" []
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        Thingy

          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (setCtx! [_ x] (.mm-s impl :ctx x) )
          (getCtx [_] (.mm-g impl :ctx) )

        Versioned
          (version [_] "1.0" )

        Hierarchial
          (parent [_] nil)

        Identifiable
          (id [_] K_DEPLOYER )

        Deployer

          (undeploy [this app]
            (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                   dir (File. ^File (.getf ctx K_PLAYDIR) ^String app) ]
              (when (.exists dir)
                  (FileUtils/deleteDirectory dir))))

          (deploy [this src]
            (let [ app (FilenameUtils/getBaseName (CU/nice-fpath src))
                   ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                   des (File. ^File (.getf ctx K_PLAYDIR) ^String app) ]
              (when-not (.exists des)
                (FU/unzip src des)))) )

      { :typeid (keyword "czc.hhh.impl/Deployer") } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-contextualize :czc.hhh.impl/Deployer
  [co ctx]
  (do
    (precondDir (maybeDir ctx K_BASEDIR))
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (comp-clone-context co ctx)))

(defmethod comp-initialize :czc.hhh.impl/Deployer
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         py (.getf ctx K_PLAYDIR)
         pd (.getf ctx K_PODSDIR)
         fs (IOUtils/listFiles ^File pd "pod" false) ]
    (doseq [ ^File f (seq fs)]
      (.deploy ^comzotohcljc.hhh.impl.defaults.Deployer co f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kernel

(defn- maybe-start-pod

  [^comzotohcljc.hhh.core.sys.Thingy knl
   ^comzotohcljc.hhh.core.sys.Thingy pod]

  (CU/TryC
    (let [ cache (.getAttr knl K_CONTAINERS)
           cid (.id ^Identifiable pod)
           ctr (make-container pod) ]
      (if (CU/notnil? ctr)
        (do
          (.setAttr! knl K_CONTAINERS (assoc cache cid ctr))
        ;;_jmx.register(ctr,"", c.name)
          )
        (info "kernel: container " cid " disabled.")) ) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-kernel "" []
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl K_CONTAINERS {} )
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
          (id [_] K_KERNEL )

        Kernel
        Startable
          (start [this]
            (let [ ^comzotohcljc.util.core.MuObj ctx (getCtx this)
                   ^comzotohcljc.hhh.core.sys.Registry
                   root (.getf ctx K_COMPS)
                   ^comzotohcljc.util.core.MuObj
                   apps (.lookup root K_APPS) ]
              ;; need this to prevent deadlocks amongst pods
              ;; when there are dependencies
              ;; TODO: need to handle this better
              (doseq [ [k v] (seq* apps) ]
                (let [ r (-> (CU/new-random) (.nextInt 6)) ]
                  (maybe-start-pod this v)
                  (PU/safe-wait (* 1000 (Math/max (int 1) r)))))))

          (stop [this]
            (let [ cs (.mm-g impl K_CONTAINERS) ]
              (doseq [ [k v] (seq cs) ]
                (.stop ^Startable v))
              (.mm-s impl K_CONTAINERS {}))) )

      { :typeid (keyword "czc.hhh.impl/Kernel") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-podmeta "" [app ver parObj podType appid pathToPOD]
  (let [ pid (str podType "#" (SN/next-long))
         impl (CU/make-mmap) ]
    (with-meta
      (reify

        Thingy

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )

        Versioned
          (version [_] ver)

        Hierarchial
          (parent [_] parObj)

        Identifiable
          (id [_] pid )

        PODMeta

          (srcUrl [_] pathToPOD)
          (appKey [_] appid)
          (typeof [_] podType))

      { :typeid (keyword "czc.hhh.impl/PODMeta") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :czc.hhh.impl/PODMeta
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^comzotohcljc.util.core.MuObj ctx (.getCtx co)
         rcl (.getf ctx K_ROOT_CZLR)
         ^URL url (.srcUrl ^comzotohcljc.hhh.impl.defaults.PODMeta co)
         cl  (AppClassLoader. rcl) ]
    (.configure cl (CU/nice-fpath (File. (.toURI  url))) )
    (.setf! ctx K_APP_CZLR cl)))

(defmethod comp-compose :czc.hhh.impl/Kernel
  [co rego]
  ;; get the jmx server from root
  co)

(defmethod comp-contextualize :czc.hhh.impl/Kernel
  [co ctx]
  (let [ base (maybeDir ctx K_BASEDIR) ]
    (precondDir base)
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (MI/setup-cache (-> (File. base (str DN_CFG "/app/mime.properties"))
                      (.toURI)(.toURL )))
    (comp-clone-context co ctx)))
























(def ^:private sys-eof nil)

