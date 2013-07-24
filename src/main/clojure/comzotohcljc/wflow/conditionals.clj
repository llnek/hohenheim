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

(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.util.coreutils :as CU])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If

(defn make-if [expr then else]
  (let [ b (make-activity Conditional If) ]
    (.setf b :test expr)
    (.setf b :then then)
    (.setf b :else else)
    b))

(defmethod ac-reify :If [ac cur]
  (ac-spawnpoint ac cur ConditionalPoint IfPoint))

(defmethod ac-realize! :If [ac fw]
  (let [ np (.getf fw :next)
         t (.getf ac :then)
         c (.getf ac :test)
         e (.getf ac :else) ]
    (.setf fw :else (if (nil? e) np (ac-reify e np)))
    (.setf fw :then (ac-reify t np))
    (.setf fw :test c)
    fw))

(defmethod fw-evaluate! :IfPoint [fw job]
  (let [ c (fw-popattmt! fw)
         t (.getf fw :then)
         e (.getf fw :else)
         b (.eval (.getf fw :test) job) ]
    (debug "if-(test) = " (if b "OK" "FALSE"))
    (let [ rc (if b t e) ]
      (when-not (nil? rc) (fw-setattmt! rc c))
      (fw-realize! fw)
      rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; While

(defn make-while [expr body]
  (let [ b (make-activity Conditional While) ]
    (.setf b :test expr)
    (.setf b :then body)
    b))

(defmethod ac-reify :While [ac cur]
  (ac-spawnpoint ac cur ConditionalPoint WhilePoint))

(defmethod ac-realize! :While [ac fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b fw)))
    (.setf fw :test t)
    fw))

(defn- evalWhilePoint [fw job]
  (let [ c (fw-popattmt! fw)
         np (.getf fw :next)
         body (.getf fw :body)
         tst (.getf fw :test) ]
    (with-local-vars [ rc fw ]
      (if (not (.eval tst job))
        (do
          (debug "test-condition == false")
          (var-set rc np)
          (when-not (nil? @rc) (fw-setattmt! @rc c))
          (fw-realize! fw))
        (do
          (debug "looping - eval body")
          (fw-setattmt! body c)
          (let [ f (fw-evaluate! body job)
                 id (:typeid (meta f)) ]
            (cond
              (or (= id :AsyncWaitPoint)
                  (= id :DelayPoint))
              (do (.setf f :next @rc) (var-set rc f))

              :else
              (when-not (identical? f fw)
                (.setf fw :body f))))) )
      @rc) ))

(defmethod fw-evaluate! :WhilePoint [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For

;; expr == ForLoopCountExpr
(defn make-for [expr body]
  (let [ b (make-activity Conditional While For) ]
    (.setf b :test expr)
    (.setf b :body body)
    b))

(defmethod ac-reify :For [ac cur]
  (ac-spawnpoint ac cur ConditionalPoint WhilePoint ForPoint))

(deftype ForLoopExpr [ ^:unsynchronized-mutable started
                       ^:unsynchronized-mutable loopCnt
                       loopCountExpr ] BoolExpr
  (evaluate [_ job]
    (try
      (when-not (started)
        (set! loopCnt (.getCount loopCountExpr job))
        (set! started true))
      (debug "currnet loop " loopCnt)
      (let [ rc (> loopCnt 0) ]
        rc)
      (finally
        (set! loopCnt (dec loopCnt))))))

(defmethod ac-realize! :For [ac fw]
  (let [ b (.getf ac :body)
         t (.getf ac :test) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b fw)))
    (.setf fw :test (ForLoopExpr. false 0 t))
    fw))

(defmethod fw-evaluate! :ForPoint [fw job]
  (evalWhilePoint fw job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private conditionals-eof nil)

