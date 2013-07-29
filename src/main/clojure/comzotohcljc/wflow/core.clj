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

  comzotohcljc.wflow.core )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.hohenheim.core Job))

(use '[comzotohcljc.util.core :only (MutableObjectAPI) ])

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU ])
(require '[comzotohcljc.util.meta :as MU])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol FlowPoint)
(defprotocol Activity)

(defprotocol NihilPoint)
(defprotocol Nihil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for flow points
(defmulti fw-configure! "" (fn [a b c] (:typeid (meta a))))
(defmulti fw-realize! "" (fn [a] (:typeid (meta a))))
(defmulti fw-evaluate! "" (fn [a b] (:typeid (meta a))))


;; for activities
(defmulti ac-realize! "" (fn [a b] (:typeid (meta a))))
(defmulti ac-reify "" (fn [a b] (:typeid (meta a))))

(defmacro fw-pipe* [fw]
  `(.getf ~fw :pipeline))

(defmacro fw-next* [fw]
  `(.getf ~fw :next))

(defmacro fw-id* [fw]
  `(:typeid (meta ~fw)))

(comment
(defmacro fw-gf* [fw p]
  `(.getf ~fw ~p))

(defmacro fw-sf* [fw p v]
  `(.setf ~fw ~p ~v))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fw-runafter [fw]
  (let [ pipe (fw-pipe* fw)
         np (fw-next* fw)
         id (fw-id* fw)
         c (->
             pipe
             (.container)
             (.core)) ]

    (cond
      (= id :comzotohcljc.wflow.delays/DelayPoint)
      (.delay c np (.getf fw :delayMillis))

      (= id :comzotohcljc.wflow.delays/AsyncWaitPoint)
      (.hold c np)

      (= id :comzotohcljc.wflow.core/NihilPoint)
      (.stop pipe)

      :else
      (.run c fw))
    fw))


(defn fw-rerun "" [fw]
  (let [ pipe (fw-pipe* fw) ]
    (-> pipe
      (.container)
      (.core)
      (.reschedule fw))
    fw))


(defn fw-run "" [fw]
  (let [ pipe (fw-pipe* fw)
         job (.job pipe)
         np (fw-next* fw)
         c (->
             pipe
             (.container)
             (.core)) ]

    (with-local-vars [ rc nil err nil ]
      (.dequeue c fw)
      (try
          (var-set rc (fw-evaluate! fw job))
        (catch Throwable e#
          (var-set err (.onError pipe e# fw))))
      (when-not (nil? @err) (var-set rc @err))
      (if (nil? @rc)
        (do
          ;; indicate skip, happens with joins
          (debug "rc==null => skip."))
        (fw-runafter @rc)))))

;; attmt is data passed back from previous async call, if any
(defn fw-popattmt! "" [fw]
  (let [ c (.getf fw :attmt) ]
    (.setf! fw :attmt nil)
    c))

(defn fw-setattmt! "" [fw c]
  (do
    (when-not (nil? fw) (.setf! fw :attmt c))
    fw))

(defmethod fw-configure! :default [fw ac cur]
  (do
    (.setf! fw :pipeline (fw-pipe* cur))
    (.setf! fw :pid (SN/next-long))
    (.setf! fw :template ac)
    (.setf! fw :next cur)
    fw))

(defmethod fw-realize! :default [fw]
  (let [ a (.getf fw :template) ]
    (ac-realize! a fw)
    fw))

(defmethod fw-evaluate! :default [fw job] fw)

(defmethod ac-realize! :default [ac fw] fw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro make-flowpoint "" [ & args ]
  `(let [ impl# (CU/make-mmap) ]
    (with-meta
      (reify

        MutableObjectAPI

          (seq* [_] (CU/test-cond "not implemented" false))
          (setf! [_ k1# v1#] (.mm-s impl# k1# v1#))
          (getf [_ k2#] (.mm-g impl# k2#))
          (clear! [_] (.mm-c impl#))
          (clrf! [_ k3#] (.mm-r impl# k3#))

        Runnable

          (run [me#] (CU/Guard (fw-run me#)))

        FlowPoint
          ~@(filterv CU/notnil? args))

      { :typeid  ~(keyword (str *ns* "/" (last args))) } )))

(defmacro make-activity "" [ & args ]
  `(let [ impl# (CU/make-mmap) ]
     (with-meta
       (reify

          MutableObjectAPI

          (seq* [_] (CU/test-cond "not implemented" false))
          (setf! [_ k1# v1#] (.mm-s impl# k1# v1#))
          (getf [_ k2#] (.mm-g impl# k2#))
          (clear! [_] (.mm-c impl#))
          (clrf! [_ k3#] (.mm-r impl# k3#))

          Activity
            ~@(filterv CU/notnil? args))

       { :typeid  ~(keyword (str *ns* "/" (last args)))  } )) )


(defmacro ac-spawnpoint "" [ ac cur & args ]
  `(let [ f# (make-flowpoint ~@args) ]
    (fw-configure! f# ~ac ~cur)
    (fw-realize! f#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-nihil "" [] (make-activity Nihil) )

(defn ac-reify-nihil "" [pipe]
  (let [ f (make-flowpoint NihilPoint) ]
    (fw-configure! f (make-nihil) f)
    (fw-realize! f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PipelineDelegateAPI
  "External User implementation interface."
  (getStart [_] ) ;; (fn [pipe] return Activity )
  (getStop [_] ) ;; (fn [pipe return nil)
  (getError [_] )) ;; (fn [pipe error current] return next-flow? )

(defprotocol PipelineAPI
  (container [_] )
  (isActive [_] )
  (onError [_ error curPoint] )
  (start [_] )
  (stop [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-pipeline [job delegateClassName]
  (let [ delegate (MU/make-obj delegateClassName)
         pid (SN/next-long)
         impl (CU/make-mmap) ]
    (CU/test-cond "Delegate does not implement PipelineDelegateAPI"
                  (satisfies? PipelineDelegateAPI delegate))
    (CU/test-nonil "the-job" job)
    (debug "Pipeline (" pid ") being created...")
    (with-meta
      (reify

        Object

          (finalize [this]
            (debug (str "=====================>" (.toString this) " finz'ed")) )
          (toString [this]
            (str (-> this (.getClass)(.getSimpleName)) "(" pid ")" ))

        PipelineAPI

          (isActive [_] (true? (.mm-g impl :active)))
          (container [_] (.parent job))

          (onError [this err cur]
            (let [ h (.getError delegate) ]
              (error err)
              (let [ a (if (fn? h)
                         (CU/TryC (h this err cur))) ]
                (if (nil? a)
                  (ac-reify-nihil this)
                  a))))

          (start [this]
            (with-local-vars [ f9 nil ]
              (let [ f1 (ac-reify-nihil this)
                     h (.getStart delegate) ]
                (debug "Pipeline " pid " starting...")
                (when-not (fn? h)
                  (warn "no start function defined by delegate!"))
                (try
                  (let [ a (if (fn? h) (h this))
                         f2 (cond
                                (or (nil? a) (satisfies? Nihil a))
                                (ac-reify-nihil this)
                                :else
                                (ac-reify a f1)) ]
                    (var-set f9 f2)
                    (-> this (.container) (.core) (.run @f9))
                    (.mm-s impl :active true))
                  (catch Throwable e#
                      (onError this e# @f9))))) )

          (stop [this]
            (CU/TryC
              (debug "Pipeline " pid " stopping...")
              (let [ h (.getStop delegate) ]
                (when (fn? h)
                  (h this)))) ) )

      { :typeid (keyword (str *ns* "/Pipeline")) } )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- no-flow [pipe job]
  (let [ f (fn [_] (ac-reify-nihil pipe))
         g (reify PipelineDelegateAPI
             (getStart [_] f)
             (getStop [_] nil)
             (getError [_] nil)) ]
    (make-pipeline job g)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private core-eof nil)

