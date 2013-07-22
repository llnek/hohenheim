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
  comzotohcljc.hohenheim.impl.kernel )

(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(com.zotoh.hohenheim.core AppClassLoader))
(import '(java.io File))


(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hohenheim.core.constants])
(use '[comzotohcljc.hohenheim.impl.defaults])
(use '[comzotohcljc.hohenheim.impl.container])

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.procutils :as PU ] )
(require '[ comzotohcljc.util.mimeutils :as MI ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol KernelAPI
  (start [_] )
  (stop [_] ))

(def PODMetaAPI
  (srcUrl [_ ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- maybe-start-pod [knl pod]
  (try
    (let [ ctx (.getCtx knl)
           root (get ctx K_COMPS)
           apps (.lookup root K_APPS)
           cid (.id pod)
           ctr (make-container pod) ]
      (if (CU/notnil? ctr)
        (do
          (.reg apps ctr)
        ;;_jmx.register(ctr,"", c.name)
          )
        (info "kernel: container " cid " disabled.")) )
    (catch Throwable e# (error e#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-kernel ^{ :doc "" }
  [parObj]
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
          (id [_] K_KERNEL )
 
        KernelAPI

          (start [this]
            (let [ ctx (getCtx this)
                   root (get ctx K_COMPS)
                   apps (.lookup root K_APPS)
                   cs (.getf apps K_PODS) ]
              ;; need this to prevent deadlocks amongst pods
              ;; when there are dependencies
              ;; TODO: need to handle this better
              (doseq [ [k v] (seq cs) ]
                (let [ r (-> (CU/new-random) (.nextLong 6)) ]
                  (maybe-start-pod this v)
                  (PU/safe-wait (* 1000 (Math/max 1 r)))))))

          (stop [this]
            (let [ cs (.mm-g impl K_APPS) ]
              (doseq [ [k v] (seq cs) ]
                (.stop v))
              (.mm-s impl K_APPS {}))) )

      { :typeid :Kernel } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-podmeta ^{ :doc "" }
  [parObj podType pathToPOD]
  (let [ pid (str podType "#" (SN/next-long))
         impl (CU/make-mmap) ]
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
          (id [_] pid )

        PODMetaAPI

          (typeof [_] podType)
          (srcUrl [_] pathToPOD))

      { :typeid :PODMeta } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :PODMeta [co]
  (let [ ctx (.getCtx co)
         rcl (get ctx K_ROOT_CZLR)
         cl  (AppClassLoader. rcl) ]
    (.configure cl (CU/nice-fpath (File. (.toURI (.srcUrl co)))) )
    (.setCtx! co (assoc ctx K_APP_CZLR cl))) )

(defmethod comp-compose :Kernel [co rego]
  ;; get the jmx server from root
  co)

(defmethod comp-contextualize :Kernel [co ctx]
  (let [ base (maybeDir ctx K_BASEDIR) ]
    (precondDir base)
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (MI/setup-cache (-> (File. base (str DN_CFG "/app/mime.properties"))
                      (.toURI)(.toURL )))
    (.setCtx! co ctx)))
























(def ^:private kernel-eof nil)

