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

  comzotohcljc.hhh.impl.defaults )

(import '(com.zotoh.frwk.util CoreUtils))
(import '(com.zotoh.hohenheim.loaders AppClassLoader))
(import '(com.zotoh.frwk.core
  Versioned Identifiable Hierarchial))
(import '(com.zotoh.hohenheim.core
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Deployer
  ""
  (undeploy [_ app] )
  (deploy [_ src] ))

(defprotocol BlockMeta
  ""
  (enabled? [_] )
  (metaUrl [_] ))

(defprotocol PODMeta
  ""
  (typeof [_ ] )
  (appKey [_ ] )
  (srcUrl [_ ]))

(defprotocol Kernel "")

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
            (let [ cid (if (nil? c) nil (.id  ^Identifiable c))
                   cache (.mm-g impl :cache) ]
              (when (has? this cid)
                (.mm-s impl :cache (dissoc cache cid)))))

          (reg [this c]
            (let [ cid (if (nil? c) nil (.id  ^Identifiable c))
                   cache (.mm-g impl :cache) ]
              (when (has? this cid)
                (throw (RegistryError.
                         (str "Component \"" cid "\" already exists" ))))
              (.mm-s impl :cache (assoc cache cid c)))) )

      { :typeid (keyword (str "czc.hhh.impl/" (name regoType))) } )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private defaults-eof nil)

