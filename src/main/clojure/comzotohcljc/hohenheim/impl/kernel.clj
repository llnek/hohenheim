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

(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.str :as SU ] )
(require '[ comzotohcljc.util.process :as PU ] )
(require '[ comzotohcljc.util.mime :as MI ] )
(require '[ comzotohcljc.util.seqnum :as SN ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PODMetaAPI ""
  (typeof [_ ])
  (srcUrl [_ ]))

(defprotocol KernelAPI ""
  (start [_] )
  (stop [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- maybe-start-pod [knl pod]
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

(defn make-kernel "" [parObj]
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl K_CONTAINERS {} )
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
                   root (.getf ctx K_COMPS)
                   apps (.lookup root K_APPS) ]
              ;; need this to prevent deadlocks amongst pods
              ;; when there are dependencies
              ;; TODO: need to handle this better
              (doseq [ [k v] (seq* apps) ]
                (let [ r (-> (CU/new-random) (.nextLong 6)) ]
                  (maybe-start-pod this v)
                  (PU/safe-wait (* 1000 (Math/max 1 r)))))))

          (stop [this]
            (let [ cs (.mm-g impl K_CONTAINERS) ]
              (doseq [ [k v] (seq cs) ]
                (.stop v))
              (.mm-s impl K_CONTAINERS {}))) )

      { :typeid (keyword (str *ns* "/Kernel")) } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-podmeta "" [parObj podType pathToPOD]
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

      { :typeid (keyword (str *ns* "/PODMeta")) } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-initialize :comzotohcljc.hohenheim.impl.kernel/PODMeta
  [co]
  (let [ ctx (.getCtx co)
         rcl (.getf ctx K_ROOT_CZLR)
         cl  (AppClassLoader. rcl) ]
    (.configure cl (CU/nice-fpath (File. (.toURI (.srcUrl co)))) )
    (.setf! ctx K_APP_CZLR cl)))

(defmethod comp-compose :comzotohcljc.hohenheim.impl.kernel/Kernel
  [co rego]
  ;; get the jmx server from root
  co)

(defmethod comp-contextualize :comzotohcljc.hohenheim.impl.kernel/Kernel
  [co ctx]
  (let [ base (maybeDir ctx K_BASEDIR) ]
    (precondDir base)
    (precondDir (maybeDir ctx K_PODSDIR))
    (precondDir (maybeDir ctx K_PLAYDIR))
    (MI/setup-cache (-> (File. base (str DN_CFG "/app/mime.properties"))
                      (.toURI)(.toURL )))
    (comp-clone-context co ctx)))
























(def ^:private kernel-eof nil)

