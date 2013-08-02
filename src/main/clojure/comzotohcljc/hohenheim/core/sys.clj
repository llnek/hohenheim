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

  comzotohcljc.hohenheim.core.sys )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Component ""
  (setCtx! [_ ctx] )
  (getCtx [_] )
  (setAttr! [_ a v] )
  (clrAttr! [_ a] )
  (getAttr [_ a] )
  (version [_] )
  (parent [_] )
  (id [_] ))

(defprotocol Registry ""
  (lookup [_ cid] )
  (has? [_ cid] )
  (seq* [_] )
  (reg [_ c] )
  (dereg [_ c] ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti comp-contextualize "" (fn [a ctx] (:typeid (meta a))))

(defmulti comp-compose "" (fn [a rego] (:typeid (meta a))))

(defmulti comp-configure "" (fn [a options] (:typeid (meta a))))

(defmulti comp-initialize "" (fn [a] (:typeid (meta a))))

(defn synthesize-component "" [c options]
  (let [ rego (:rego options)
         ctx (:ctx options)
         props (:props options) ]
   (when-not (nil? rego) (comp-compose c rego))
   (when-not (nil? ctx) (comp-contextualize c ctx))
   (when-not (nil? props) (comp-configure c props))
   (comp-initialize c)
   c) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private sys-eof nil)

