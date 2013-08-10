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

  comzotohcljc.hhh.core.sys )

(import '(com.zotoh.frwk.core
  Hierarchial Identifiable Versioned))

(use '[comzotohcljc.util.core :only (MuObj)])
(require '[comzotohcljc.util.core :as CU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol Thingy
  ""
  (setCtx! [_ ctx] )
  (getCtx [_] )
  (setAttr! [_ a v] )
  (clrAttr! [_ a] )
  (getAttr [_ a] ) )

(defprotocol Registry
  ""
  (lookup [_ cid] )
  (has? [_ cid] )
  (seq* [_] )
  (reg [_ c] )
  (dereg [_ c] ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti ^comzotohcljc.hhh.core.sys.Thingy comp-contextualize
  ""
  (fn [a ctx] (:typeid (meta a))))

(defmulti ^comzotohcljc.hhh.core.sys.Thingy comp-compose
  ""
  (fn [a rego] (:typeid (meta a))))

(defmulti ^comzotohcljc.hhh.core.sys.Thingy comp-configure
  ""
  (fn [a options] (:typeid (meta a))))

(defmulti ^comzotohcljc.hhh.core.sys.Thingy comp-initialize
  ""
  (fn [a] (:typeid (meta a))))

(defn synthesize-component ""

  ^comzotohcljc.hhh.core.sys.Thingy
  [c options]

  (let [ rego (:rego options)
         ctx (:ctx options)
         props (:props options) ]
   (when-not (nil? rego) (comp-compose c rego))
   (when-not (nil? ctx) (comp-contextualize c ctx))
   (when-not (nil? props) (comp-configure c props))
   (comp-initialize c)
   c) )


(defn make-context "" ^comzotohcljc.util.core.MuObj []
  (let [ impl (CU/make-mmap) ]
    (reify MuObj
      (setf! [_ k v] (.mm-s impl k v) )
      (seq* [_] (seq (.mm-m* impl)))
      (getf [_ k] (.mm-g impl k) )
      (clrf! [_ k] (.mm-r impl k) )
      (clear! [_] (.mm-c impl)))) )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private sys-eof nil)

