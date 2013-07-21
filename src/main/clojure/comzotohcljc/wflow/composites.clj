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
  comzotohcljc.wflow.composites )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.hohenheim.core Job))
(import '(java.util.concurrent.atomic AtomicLong))
(require '[comzotohcljc.util.coreutils :as CU])
(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MutableListAPI
  (is-empty? [_] )
  (length [_])
  (shift [_]))
(deftype MutableList [ ^:unsynchronized-mutable data] MutableListAPI
  (is-empty? [_] (= 0 (count data) ))
  (length [_] (count data))
  (shift [this]
    (if (is-empty? this)
      nil
      (let [ f (first data) ]
        (set! data (pop data))
        f))))

(defprotocol InnerPointsAPI
  (isEmpty [_])
  (size [_])
  (nextPoint [_]))

;; children == (vec ... activities )
(defn make-innerPoints [outerPoint children]
  (let [ impl (MutableList. (into () (reverse children))) ]
    (reify
      InnerPointsAPI
      (isEmpty [_] (.is-empty? impl))
      (size [_] (.size impl))
      (nextPoint [_]
        (let [ ac (.shift impl) ]
          (if (nil? ac)
            nil
            (ac-reify ac outerPoint)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite

(defprotocol CompositePoint)
(defprotocol Composite)

(defn composite-add! [b a]
  (when-not (nil? a)
    (let [ c (.getf b :children) ]
      (.setf b :children (conj c a))))
  b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block

(defprotocol BlockPoint)
(defprotocol Block)

(defn make-block [ & args ]
  (let [ b (make-activity :Block Composite Block)
         v (if (empty? args)
             []
             (vec (flatten (conj [] args)))) ]
    (.setf b :children v)
    b))

(defmethod ac-reify :Block [ac cur]
  (ac-spawnpoint ac cur :BlockPoint CompositePoint BlockPoint))

(defmethod ac-realize! :Block [ac fw]
  (let [ w (make-innerPoints fw (.getf ac :children)) ]
    (.setf fw :inner-points w)
    fw))

(defmethod fw-evaluate! :BlockPoint [fw  job]
  (let [ w (.getf fw :inner-points)
         np (.getf fw :next)
         c (fw-popattmt! fw) ]
    (with-local-vars [ rc nil ]
      (if (or (nil? w) (.isEmpty w))
        (do
          (debug "no more inner points.")
          (var-set rc np)
          (when-not (nil? @rc) (fw-setattmt! @rc c))
          (fw-realize! fw)
          @rc)
        (do
          (debug (.size w) " inner points remaining.")
          (fw-evaluate! (->  (.nextPoint w)(fw-setattmt! c)) job))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Join

(defprotocol JoinPoint)
(defprotocol Join)

(defprotocol NullJoinPoint)
(defprotocol NullJoin)

(defprotocol AndJoinPoint)
(defprotocol AndJoin)

(defprotocol OrJoinPoint)
(defprotocol OrJoin)

(defn make-nulljoin []
  (let [ a (make-activity :NullJoin Join NullJoin) ]
    (.setf a :branches 0)
    (.setf a :body nil)
    a))

(defmethod ac-reify :NullJoin [ac cur]
  (ac-spawnpoint ac cur :NullJoinPoint JoinPoint NullJoinPoint))

(defmethod fw-evaluate! :NullJoinPoint [fw  job] nil)
(defmethod ac-realize! :NullJoin [ac fw] fw)


(defn make-andjoin [body]
  (let [ a (make-activity :AndJoin Join AndJoin) ]
    (.setf a :body body)
    (.setf a :branches 0)
    a))

(defmethod ac-reify :AndJoin [ac cur]
  (ac-spawnpoint ac cur :AndJoinPoint JoinPoint AndJoinPoint))

(defmethod ac-realize! :AndJoin [ac fw]
  (let [ b (.getf ac :body)
         np (.getf fw :next)
         n (.getf ac :branches) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b np)) )
    (.setf fw :branches n)
    (.setf fw :counter (AtomicLong. 0))
    fw))

(defmethod fw-evaluate! :AndJoinPoint [fw job]
  (let [ c (fw-popattmt! fw)
         b (.getf fw :branches)
         body (.getf fw :body)
         np (.getf fw :next)
         n (.getf fw :counter)
         nn (.incrementAndGet n) ]
    (debug "branches " b ", counter " nn ", join(pid) = " fw)
    (with-local-vars [ rc nil ]
      ;; all branches have returned, proceed...
      (when (= nn b)
        (var-set rc (if (nil? body) np body))
        (when-not (nil? @rc) (fw-setattmt! @rc c))
        (fw-realize! fw))
      @rc)))


(defn make-orjoin [body]
  (let [ a (make-activity :OrJoin Join OrJoin) ]
    (.setf a :body body)
    (.setf a :branches 0)
    a))

(defmethod ac-reify :OrJoin [ac cur]
  (ac-spawnpoint ac cur :OrJoinPoint JoinPoint OrJoinPoint))

(defmethod ac-realize! :OrJoin [ac fw]
  (let [ b (.getf ac :body)
         np (.getf fw :next)
         n (.getf ac :branches) ]
    (when-not (nil? b)
      (.setf fw :body (ac-reify b np)) )
    (.setf fw :branches n)
    (.setf fw :counter (AtomicLong. 0))
    fw))

(defmethod fw-evaluate! :OrJoinPoint [fw job]
  (let [ c (fw-popattmt! fw)
         b (.getf fw :branches)
         np (.getf fw :next)
         body (.getf fw :body)
         n (.getf fw :counter)
         nn (.incrementAndGet n) ]
    (debug "branches " b ", counter " nn ", join(pid) = " fw)
    (with-local-vars [ rc nil ]
      (cond
        (= b 0) (do (var-set rc np) (fw-realize! fw))
        (= 1 nn) (var-set rc (if (nil? body) np body))
        (= b nn) (do (var-set rc nil) (fw-realize! fw)))
      (when-not (nil? @rc) (fw-setattmt! @rc c))
      @rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split

(defprotocol SplitPoint)
(defprotocol Split)

(defn make-split [joiner]
  (let [ s (make-activity :Split Composite Split) ]
    (.setf s :children [])
    (.setf s :join joiner)
    s))

(defmethod ac-reify :Split [ac cur]
  (ac-spawnpoint ac cur :SplitPoint CompositePoint SplitPoint))

(defmethod ac-realize! :Split [ac fw]
  (let [ cs (.getf ac :children)
         j (.getf ac :join)
         np(.getf fw :next)
         n (count cs)
         s (if (nil? j)
             (ac-reify (make-nulljoin) np)
             (do
               (.setf j :branches n)
               (ac-reify j np))) ]
    (when (nil? j)
      (.setf fw :fall-thru true) )
    (.setf fw :inner-points (make-innerPoints s cs))
    fw))

(defmethod fw-evaluate! :SplitPoint [fw job]
  (let [ w (:getf fw :inner-points)
         pipe (.getf fw :pipeline)
         c (fw-popattmt! fw) ]
    (while (and (CU/notnil? w) (not (.isEmpty w)))
      (let [ n (.nextPoint w) ]
        (fw-setattmt! n c)
        (.run pipe n)))
    (fw-realize! fw)
    ;; should we also pass the closure to the next step ? not for now
    (if (.getf fw :fall-thru)
      (.getf fw :next)
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composites-eof nil)


