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

  comzotohcljc.wflow.conditionals )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.hohenheim.core BoolExpr Job))

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ConditionalPoint)
(defprotocol Conditional)

(defprotocol IfPoint)
(defprotocol If)

(defprotocol WhilePoint)
(defprotocol While)

(defprotocol ForLoopCountExpr
  (getCount [_ job] ))
(defprotocol ForPoint)
(defprotocol For)

(defprotocol SwitchPoint)
(defprotocol Switch)

(defprotocol SwitchChoiceExpr
  (getChoice [_ job] ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If

(defn make-if [expr then else]
  (let [ b (make-activity Conditional If) ]
    (.setf b :test expr)
    (.setf b :then then)
    (.setf b :else else)
    b))

(defmethod ac-reify :comzotohcljc.wflow.conditionals/If
  [ac cur]
  (ac-spawnpoint ac cur ConditionalPoint IfPoint))

(defmethod ac-realize! :comzotohcljc.wflow.conditionals/If
  [ac fw]
  (let [ np (fw-next* fw)
         t (.getf ac :then)
         c (.getf ac :test)
         e (.getf ac :else) ]
    (.setf fw :else (if (nil? e) np (ac-reify e np)))
    (.setf fw :then (ac-reify t np))
    (.setf fw :test c)
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.conditionals/IfPoint
  [fw job]
  (let [ c (fw-popattmt! fw)
         t (.getf fw :then)
         e (.getf fw :else)
         b (.evalulate (.getf fw :test) job) ]
    (debug "if-(test) = " (if b "OK" "FALSE"))
    (let [ rc (if b t e) ]
      (fw-setattmt! rc c)
      (fw-realize! fw)
      rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; While

(defn make-while [expr body]
  (let [ b (make-activity Conditional While) ]
    (.setf b :test expr)
    (.setf b :then body)
    b))

(defmethod ac-reify :comzotohcljc.wflow.conditionals/While
  [ac cur]
  (ac-spawnpoint ac cur ConditionalPoint WhilePoint))

(defmethod ac-realize! :comzotohcljc.wflow.conditionals/While
  [ac fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b fw)))
    (.setf fw :test t)
    fw))

(defn- evalWhilePoint [fw job]
  (let [ c (fw-popattmt! fw)
         np (fw-next* fw)
         body (.getf fw :body)
         tst (.getf fw :test) ]
    (with-local-vars [ rc fw ]
      (if (not (.evalulate tst job))
        (do
          (debug "test-condition == false")
          (var-set rc np)
          (fw-setattmt! @rc c)
          (fw-realize! fw))
        (do
          (debug "looping - eval body")
          (fw-setattmt! body c)
          (let [ f (fw-evaluate! body job)
                 id (fw-id* f) ]
            (cond
              (or (= id :comzotohcljc.wflow.delays/AsyncWaitPoint)
                  (= id :comzotohcljc.wflow.delays/DelayPoint))
              (do (.setf f :next @rc) (var-set rc f))

              :else
              (when-not (identical? f fw)
                (.setf fw :body f))))) )
      @rc) ))

(defmethod fw-evaluate! :comzotohcljc.wflow.conditionals/WhilePoint
  [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For

;; expr == ForLoopCountExpr
(defn make-for [expr body]
  (let [ b (make-activity Conditional While For) ]
    (.setf b :test expr)
    (.setf b :body body)
    b))

(defmethod ac-reify :comzotohcljc.wflow.conditionals/For
  [ac cur]
  (ac-spawnpoint ac cur ConditionalPoint WhilePoint ForPoint))

(deftype ForLoopExpr [ ^:unsynchronized-mutable started
                       ^:unsynchronized-mutable loopCnt
                       loopCountExpr ] BoolExpr
  (evaluate [_ job]
    (try
      (when-not (started)
        (set! loopCnt (.getCount loopCountExpr job))
        (set! started true))
      (debug "current loop " loopCnt)
      (let [ rc (> loopCnt 0) ]
        rc)
      (finally
        (set! loopCnt (dec loopCnt))))))

(defmethod ac-realize! :comzotohcljc.wflow.conditionals/For
  [ac fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b fw)))
    (.setf fw :test (ForLoopExpr. false 0 t))
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.conditionals/ForPoint
  [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch
;;

(defn make-switch [choiceExpr]
  (let [ a (make-activity Switch) ]
    (.setf a :test choiceExpr)
    (.setf a :default nil)
    (.setf a :choices {} )
    a))

(defmethod ac-reify :comzotohcljc.wflow.conditionals/Switch
  [ac cur]
  (ac-spawnpoint ac cur SwitchPoint))

(defmethod ac-realize! :comzotohcljc.wflow.conditionals/Switch
  [ac cur]
  (let [ cs (.getf ac :choices)
         df (.getf cur :default)
         np (fw-next* cur)
         t  (persistent! (reduce (fn [sum en]
                                    (assoc! sum (first en)
                                            (ac-reify (last en) np)) )
                                 (transient {})
                                 (seq cs))) ]
    (.setf cur :choices t)
    (when-not (nil? df)
      (.setf cur :default (ac-reify df np)))
    (.setf cur :test (.getf ac :test))
    cur))

(defmethod fw-evaluate! :comzotohcljc.wflow.conditionals/SwitchPoint
  [fw job]
  (let [ cs (.getf fw :choices)
         df (.getf fw :default)
         c (fw-popattmt! fw)
         e (.getf fw :test) ]
    (let [ h (get cs (.getChoice e job))
           x (if (nil? h) df h) ]
      (fw-setattmt! x c)
      (fw-realize! fw)
      x)))




(def ^:private conditionals-eof nil)

