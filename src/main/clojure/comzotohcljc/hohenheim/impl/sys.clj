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

  comzotohcljc.hohenheim.impl.sys )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(com.zotoh.hohenheim.loaders AppClassLoader))
(import '(com.zotoh.hohenheim.core Startable))
(import '(java.io File))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.core.sys])
(use '[comzotohcljc.hohenheim.impl.defaults])
(use '[comzotohcljc.hohenheim.impl.ext])

(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.str :as SU ] )
(require '[ comzotohcljc.util.process :as PU ] )
(require '[ comzotohcljc.util.files :as FU ] )
(require '[ comzotohcljc.util.mime :as MI ] )
(require '[ comzotohcljc.util.seqnum :as SN ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)



(defprotocol DeployerAPI ""
  (undeploy [_ app] )
  (deploy [_ src] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PODMetaAPI ""
  (typeof [_ ])
  (srcUrl [_ ]))

(defprotocol KernelAPI ""
  (start [_] )
  (stop [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deployer

(defn make-deployer "" []
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        Component

          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (setCtx! [_ x] (.mm-s impl :ctx x) )
          (getCtx [_] (.mm-g impl :ctx) )
          (version [_] "1.0" )
          (parent [_] nil)
          (id [_] K_DEPLOYER )

        DeployerAPI

          (undeploy [this app]
            (let [ ^comzotohcljc.util.core.MutableObjectAPI ctx (getCtx this)
                   dir (File. (.getf ctx K_PLAYDIR) app) ]
              (when (.exists dir)
                  (FileUtils/deleteDirectory dir))))

          (deploy [this src]
            (let [ app (FilenameUtils/getBaseName (CU/nice-fpath src))
                   ^comzotohcljc.util.core.MutableObjectAPI ctx (getCtx this)
                   des (File. (.getf ctx K_PLAYDIR) app)
                   pod (File. (.toURI src)) ]
              (when-not (.exists des)
                (FU/unzip pod des)))) )

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
  [^comzotohcljc.hohenheim.core.sys.Component co]
  (let [ ^comzotohcljc.util.core.MutableObjectAPI ctx (.getCtx co)
         py (.getf ctx K_PLAYDIR)
         ^File pd (.getf ctx K_PODSDIR)
         fs (FileUtils/listFiles pd (into-array String ["pod"]) false) ]
    (doseq [ ^File f (seq fs)]
      (.deploy ^comzotohcljc.hohenheim.impl.sys.DeployerAPI co (-> f (.toURI)(.toURL))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kernel

(defn- maybe-start-pod 
  
  [^comzotohcljc.hohenheim.core.sys.Component knl 
   ^comzotohcljc.hohenheim.core.sys.Component pod]

  (CU/TryC
    (let [ cache (.getAttr knl K_CONTAINERS)
           cid (.id pod)
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

        Component

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (version [_] "1.0")
          (parent [_] nil)
          (id [_] K_KERNEL )
 
        KernelAPI

          (start [this]
            (let [ ^comzotohcljc.util.core.MutableObjectAPI ctx (getCtx this)
                   ^comzotohcljc.hohenheim.core.sys.Registry root (.getf ctx K_COMPS)
                   ^comzotohcljc.util.core.MutableObjectAPI apps (.lookup root K_APPS) ]
              ;; need this to prevent deadlocks amongst pods
              ;; when there are dependencies
              ;; TODO: need to handle this better
              (doseq [ [k v] (seq* apps) ]
                (let [ r (-> (CU/new-random) (.nextInt 6)) ]
                  (maybe-start-pod this v)
                  (PU/safe-wait (* 1000 (Math/max 1 r)))))))

          (stop [this]
            (let [ cs (.mm-g impl K_CONTAINERS) ]
              (doseq [ [k v] (seq cs) ]
                (.stop ^Startable v))
              (.mm-s impl K_CONTAINERS {}))) )

      { :typeid (keyword "czc.hhh.impl/Kernel") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-podmeta "" [app ver parObj podType pathToPOD]
  (let [ pid (str podType "#" (SN/next-long))
         impl (CU/make-mmap) ]
    (with-meta
      (reify

        Component

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (version [_] ver)
          (parent [_] parObj)
          (id [_] pid )

        PODMetaAPI

          (srcUrl [_] pathToPOD)
          (typeof [_] podType))

      { :typeid (keyword "czc.hhh.impl/PODMeta") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :czc.hhh.impl/PODMeta
  [^comzotohcljc.hohenheim.core.sys.Component co]
  (let [ ^comzotohcljc.util.core.MutableObjectAPI ctx (.getCtx co)
         rcl (.getf ctx K_ROOT_CZLR)
         cl  (AppClassLoader. rcl) ]
    (.configure cl (CU/nice-fpath (File. (.toURI (.srcUrl co)))) )
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

