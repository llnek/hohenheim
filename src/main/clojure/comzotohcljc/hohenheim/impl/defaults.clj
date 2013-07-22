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
  comzotohcljc.hohenheim.impl.defaults )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.frwk.util CoreUtils))
(import '(com.zotoh.hohenheim.core
  RegistryError ServiceError ConfigError AppClassLoader))
(import '(java.io File))

(require '[ comzotohcljc.util.coreutils :as CU ] )
(require '[ comzotohcljc.util.strutils :as SU ] )
(require '[ comzotohcljc.util.fileutils :as FU ] )

(use '[comzotohcljc.hohenheim.core.constants])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn precondDir ^{ :doc "" }
  [d]
  (CU/test-cond (str "Directory " d " must be read-writable.") (FU/dir-readwrite? d)))

(defn precondFile ^{ :doc "" }
  [f]
  (CU/test-cond (str "File " f " must be readable.") (FU/file-read? f)))

(defn maybeDir ^{ :doc "" }
  [m kn]
  (let [ v (get m kn) ]
    (cond
      (instance? String v) (File. v)
      (instance? File v) v
      :else (throw (ConfigError. (str "No such folder for key: " kn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti comp-set-context ^{ :doc "" } 
  (fn [a b] (class a)))

(defmulti comp-get-context ^{ :doc "" }
  class)

(defmulti comp-set-cache ^{ :doc "" }
  (fn [a b] (class a)))

(defmulti comp-get-cache ^{ :doc "" }
  class)

(defmulti comp-compose ^{ :doc "" }
  (fn [a rego] (class a)))

(defmulti comp-contextualize ^{ :doc "" }
  (fn [a b] (class a)))

(defmulti comp-configure ^{ :doc "" }
  (fn [a b] (class a)))

(defmulti comp-initialize ^{ :doc "" }
  class)

(defn synthesize-component ^{ :doc "" }
  [c options]
  (let [ rego (:rego options)
         ctx (:ctx options)
         props (:props options) ]
   (when-not (nil? rego) (comp-compose c rego))
   (when-not (nil? ctx) (comp-contextualize c ctx))
   (when-not (nil? props) (comp-configure c props))
   (comp-initialize c)
   c) )

(defprotocol Component
  ""
  (setCtx! [_ ctx] )
  (getCtx [_] )
  (setAttr! [_ a v] )
  (clrAttr! [_ a] )
  (getAttr [_ a] )
  (version [_] )
  (parent [_] )
  (id [_] ))

(defprotocol Registry
  ""
  (lookup [_ cid] )
  (has? [_ cid] )
  (reg [_ c] )
  (dereg [_ c] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-component-registry ^{ :doc "" } 
  [regoId ver parObj]
  (let [ impl (CU/make-mmap) ]
    (CU/test-cond "registry id" (keyword? regoId))
    (CU/test-nestr "registry version" ver)
    (reify
      Component
        (setCtx! [_ x] (.mm-s impl :ctx x))
        (getCtx [_] (.mm-g impl :ctx))
        (parent [_] parObj)
        (setAttr! [_ a v] (.mm-s impl a v) )
        (clrAttr! [_ a] (.mm-r impl a) )
        (getAttr [_ a] (.mm-g impl a) )
        (version [_] ver)
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
              (.lookup parObj cid)
              c)) )

        (dereg [this c]
          (let [ cid (if (nil? c) nil (.id c))
                 cache (.mm-g impl :cache) ]
            (when (has? this cid)
              (.mm-s impl :cache (dissoc cache cid)))))

        (reg [this c]
          (let [ cid (if (nil? c) nil (.id c))
                 cache (.mm-g impl :cache) ]
            (when (has? this cid)
              (throw (RegistryError. (str "Component \"" cid "\" already exists" ))))
            (.mm-s impl :cache (assoc cache cid c))))
  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-get-context :default [co] @(.ctxHolder co))

(defmethod comp-set-context :default [co x]
  (dosync (ref-set (.ctxHolder co) x)))

(defmethod comp-get-cache :default [co] @(.cacheHolder co))

(defmethod comp-set-cache :default [co x]
  (dosync (ref-set (.cacheHolder co) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comp-compose :default [co rego] co)

(defmethod comp-contextualize :default [co ctx]
  (do (comp-set-context co ctx) co))

(defmethod comp-configure :default [co props] co)
(defmethod comp-initialize :default [co] co)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private defaults-eof nil)

