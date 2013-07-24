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
  comzotohcljc.wflow.switch )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.hohenheim.core Job))

(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.util.coreutils :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol SwitchPoint)
(defprotocol Switch)

(defprotocol SwitchChoiceExpr
  (getChoice [_ job] ))

(defn make-switch [choiceExpr]
  (let [ a (make-activity Switch) ]
    (.setf a :test choiceExpr)
    (.setf a :default nil)
    (.setf a :choices {} )
    a))

(defmethod ac-reify :Switch [ac cur]
  (ac-spawnpoint ac cur SwitchPoint))

(defmethod ac-realize! :Switch [ac cur]
  (let [ cs (.getf ac :choices)
         df (.getf cur :default)
         np (.getf cur :next)
         t  (reduce (fn [sum en]
                      (assoc sum (first en) (ac-reify (last en) np)) )
                    {} (seq cs)) ]
    (.setf cur :choices t)
    (when-not (nil? df)
      (.setf cur :default (ac-reify df np)))
    (.setf cur :test (.getf ac :test))
    cur))

(defmethod fw-evaluate! :SwitchPoint [fw job]
  (let [ cs (.getf fw :choices)
         df (.getf fw :default)
         c (fw-popattmt! fw)
         e (.getf fw :test) ]
    (let [ h (get cs (.getChoice e job))
           x (if (nil? h) df h) ]
      (when-not (nil? x) (fw-setattmt! x c))
      (fw-realize! fw)
      x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def ^:private switch-eof nil)


