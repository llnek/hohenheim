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

(import '(java.util.concurrent.atomic AtomicLong))
(import '(com.zotoh.hohenheim.core Job))

(require '[comzotohcljc.util.core :as CU])
(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ^:private MutableListAPI
  ""
  (is-empty? [_] )
  (length [_])
  (shift [_]))

(deftype ^:private MutableList
  [ ^:unsynchronized-mutable data] MutableListAPI

  (is-empty? [_] (= 0 (count data) ))
  (length [_] (count data))
  (shift [this]
    (if (is-empty? this)
      nil
      (let [ f (first data) ]
        (set! data (pop data))
        f))))

(defprotocol ^:private InnerPointsAPI
  (isEmpty [_])
  (size [_])
  (nextPoint [_]))

;; children == (vec ... activities )
(defn- make-innerPoints [outerPoint children]
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

(defn- composite-add! [b a]
  (when-not (nil? a)
    (let [ c (.getf b :children) ]
      (.setf! b :children (conj c a))))
  b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block

(defprotocol BlockPoint)
(defprotocol Block)

(defn make-block "Create a new Block Activity."
  [ & args ]
  (let [ b (make-activity Composite Block)
         v (if (empty? args)
             []
             (vec (flatten (conj [] args)))) ]
    (.setf! b :children v)
    b))

(defmethod ac-reify :comzotohcljc.wflow.composites/Block
  [ac cur]
  (ac-spawnpoint ac cur CompositePoint BlockPoint))

(defmethod ac-realize! :comzotohcljc.wflow.composites/Block
  [ac fw]
  (let [ w (make-innerPoints fw (.getf ac :children)) ]
    (.setf! fw :inner-points w)
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.composites/BlockPoint
  [fw  job]
  (let [ w (.getf fw :inner-points)
         c (fw-popattmt! fw)
         np (fw-next* fw) ]

    (with-local-vars [ rc nil ]
      (if (or (nil? w) (.isEmpty w))
        (do
          (debug "no more inner points.")
          (var-set rc np)
          (fw-setattmt! @rc c)
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

(defn- make-nulljoin "Create a NULL Join Activity."
  []
  (let [ a (make-activity Join NullJoin) ]
    (.setf! a :branches 0)
    (.setf! a :body nil)
    a))

(defmethod ac-reify :comzotohcljc.wflow.composites/NullJoin
  [ac cur]
  (ac-spawnpoint ac cur  JoinPoint NullJoinPoint))

(defmethod fw-evaluate! :comzotohcljc.wflow.composites/NullJoinPoint
  [fw  job] nil)

(defmethod ac-realize! :comzotohcljc.wflow.composites/NullJoin
  [ac fw] fw)


(defn make-andjoin "Create an And Join Activity."
  [body]
  (let [ a (make-activity Join AndJoin) ]
    (.setf! a :body body)
    (.setf! a :branches 0)
    a))

(defmethod ac-reify :comzotohcljc.wflow.composites/AndJoin
  [ac cur]
  (ac-spawnpoint ac cur JoinPoint AndJoinPoint))

(defmethod ac-realize! :comzotohcljc.wflow.composites/AndJoin
  [ac fw]
  (let [ n (.getf ac :branches)
         b (.getf ac :body)
         np (fw-next* fw) ]
    (when-not (nil? b)
      (.setf! fw :body (ac-reify b np)) )
    (.setf! fw :branches n)
    (.setf! fw :counter (AtomicLong. 0))
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.composites/AndJoinPoint
  [fw job]
  (let [ b (.getf fw :branches)
         body (.getf fw :body)
         c (fw-popattmt! fw)
         np (fw-next* fw)
         n (.getf fw :counter)
         nn (.incrementAndGet n) ]

    (debug "branches " b ", counter " nn ", join(pid) = " fw)
    (with-local-vars [ rc nil ]
      ;; all branches have returned, proceed...
      (when (= nn b)
        (var-set rc (if (nil? body) np body))
        (fw-setattmt! @rc c)
        (fw-realize! fw))
      @rc)))

(defn make-orjoin "Create a Or Join Activity."
  [body]
  (let [ a (make-activity Join OrJoin) ]
    (.setf! a :body body)
    (.setf! a :branches 0)
    a))

(defmethod ac-reify :comzotohcljc.wflow.composites/OrJoin
  [ac cur]
  (ac-spawnpoint ac cur JoinPoint OrJoinPoint))

(defmethod ac-realize! :comzotohcljc.wflow.composites/OrJoin
  [ac fw]
  (let [ n (.getf ac :branches)
         b (.getf ac :body)
         np (fw-next* fw) ]

    (when-not (nil? b)
      (.setf! fw :body (ac-reify b np)) )
    (.setf! fw :branches n)
    (.setf! fw :counter (AtomicLong. 0))
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.composites/OrJoinPoint
  [fw job]
  (let [ b (.getf fw :branches)
         c (fw-popattmt! fw)
         np (fw-next* fw)
         body (.getf fw :body)
         n (.getf fw :counter)
         nn (.incrementAndGet n) ]

    (debug "branches " b ", counter " nn ", join(pid) = " fw)
    (with-local-vars [ rc nil ]
      (cond
        (= b 0) (do (var-set rc np) (fw-realize! fw))
        (= 1 nn) (var-set rc (if (nil? body) np body))
        (= nn b) (do (var-set rc nil) (fw-realize! fw)))
      (fw-setattmt! @rc c)
      @rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split

(defprotocol SplitPoint)
(defprotocol Split)

(defn make-split [joiner]
  (let [ s (make-activity Composite Split) ]
    (.setf! s :children [])
    (.setf! s :join joiner)
    s))

(defmethod ac-reify :comzotohcljc.wflow.composites/Split
  [ac cur]
  (ac-spawnpoint ac cur CompositePoint SplitPoint))

(defmethod ac-realize! :comzotohcljc.wflow.composites/Split
  [ac fw]
  (let [ cs (.getf ac :children)
         j (.getf ac :join)
         np (fw-next* fw)
         n (count cs)
         s (if (nil? j)
             (ac-reify (make-nulljoin) np)
             (do
               (.setf! j :branches n)
               (ac-reify j np))) ]
    (when (nil? j)
      (.setf! fw :fall-thru true) )
    (.setf! fw :inner-points (make-innerPoints s cs))
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.composites/SplitPoint
  [fw job]
  (let [ w (:getf fw :inner-points)
         pipe (fw-pipe* fw)
         c (fw-popattmt! fw) ]

    (while (and (CU/notnil? w) (not (.isEmpty w)))
      (let [ n (.nextPoint w) ]
        (fw-setattmt! n c)
        (.run pipe n)))
    (fw-realize! fw)
    ;; should we also pass the closure to the next step ? not for now
    (if (.getf fw :fall-thru)
      (fw-next* fw)
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composites-eof nil)


