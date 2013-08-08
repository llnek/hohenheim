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

  comzotohcljc.hhh.impl.defaults )

(import '(com.zotoh.frwk.util CoreUtils))
(import '(com.zotoh.hohenheim.loaders AppClassLoader))
(import '(com.zotoh.hohenheim.core
  Versioned Identifiable Hierarchial
  RegistryError ServiceError ConfigError))
(import '(java.io File))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[ comzotohcljc.util.core :only (MuObj) ] )
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.core.sys])

(require '[ comzotohcljc.util.files :as FU ] )
(require '[ comzotohcljc.util.core :as CU ] )
(require '[ comzotohcljc.util.str :as SU ] )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(defn precondDir "" [d]
  (CU/test-cond (str "Directory " d " must be read-writable.") 
                (FU/dir-readwrite? d)))

(defn precondFile "" [f]
  (CU/test-cond (str "File " f " must be readable.") 
                (FU/file-read? f)))

(defn maybeDir "" ^File [^comzotohcljc.util.core.MuObj m kn]
  (let [ v (.getf m kn) ]
    (cond
      (instance? String v)
      (File. ^String v)

      (instance? File v)
      v

      :else
      (throw (ConfigError. (str "No such folder for key: " kn))))))

(defn print-mutableObj
  ([ctx] (print-mutableObj ctx false))
  ([^comzotohcljc.util.core.MuObj ctx dbg]
    (let [ b (StringBuilder.) ]
      (doseq [ [k v] (.seq* ctx) ]
        (.append b (str k " = " v "\n")))
      (if dbg
        (debug b)
        (info b)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Deployer
  ""
  (undeploy [_ app] )
  (deploy [_ src] ))

(defprotocol BlockMeta ""
  (enabled? [_] )
  (metaUrl [_] ))

(defprotocol PODMeta 
  ""
  (typeof [_ ])
  (srcUrl [_ ]))

(defprotocol Kernel 
  ""              )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-context "" ^comzotohcljc.util.core.MuObj []
  (let [ impl (CU/make-mmap) ]
    (reify MuObj
      (setf! [_ k v] (.mm-s impl k v) )
      (seq* [_] (seq (.mm-m* impl)))
      (getf [_ k] (.mm-g impl k) )
      (clrf! [_ k] (.mm-r impl k) )
      (clear! [_] (.mm-c impl)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-component-registry "" [regoType regoId ver parObj]

  (let [ impl (CU/make-mmap) ]
    (CU/test-cond "registry type" (keyword? regoType))
    (CU/test-cond "registry id" (keyword? regoId))
    (CU/test-nestr "registry version" ver)
    (.mm-s impl :cache {} )
    (with-meta
      (reify

        Thingy

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )

        Hierarchial
          (parent [_] parObj)

        Versioned
          (version [_] ver)

        Identifiable
          (id [_] regoId)

        Registry

          (has? [_ cid]
            (let [ cache (.mm-g impl :cache)
                   c (get cache cid) ]
              (if (nil? c) false true)) )

          (lookup [_ cid]
            (let [ cache (.mm-g impl :cache)
                   c (get cache cid) ]
              (if (and (nil? c) (satisfies? Registry parObj))
                (.lookup ^comzotohcljc.hhh.core.sys.Registry parObj cid)
                c)) )

          (seq* [_]
            (let [ cache (.mm-g impl :cache) ]
              (seq cache)))

          (dereg [this c]
            (let [ cid (if (nil? c) nil (.id  c))
                   cache (.mm-g impl :cache) ]
              (when (has? this cid)
                (.mm-s impl :cache (dissoc cache cid)))))

          (reg [this c]
            (let [ cid (if (nil? c) nil (.id  c))
                   cache (.mm-g impl :cache) ]
              (when (has? this cid)
                (throw (RegistryError.
                         (str "Component \"" cid "\" already exists" ))))
              (.mm-s impl :cache (assoc cache cid c)))) )

      { :typeid (keyword (str "czc.hhh.impl/" (name regoType))) } )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn comp-clone-context 
  [^comzotohcljc.hhh.core.sys.Thingy co
   ^comzotohcljc.util.core.MuObj ctx]
  (do
    (when-not (nil? ctx)
      (let [ x (make-context) ]
        (doseq [ [k v] (.seq* ctx) ]
          (.setf! x k v))
        (.setCtx! co x)))
    co))

(defmethod comp-contextualize :default [co ctx]
  (comp-clone-context co ctx))

(defmethod comp-configure :default [co props] co)
(defmethod comp-initialize :default [co] co)
(defmethod comp-compose :default [co rego] co)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private defaults-eof nil)

