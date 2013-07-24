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


(use '[comzotohcljc.util.coreutils :only (MutableObjectAPI) ])
(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.hohenheim.core Job))

(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.metautils :as MU])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol FlowPoint)
(defprotocol Activity)

(defprotocol NihilPoint)
(defprotocol Nihil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti fw-configure! "" (fn [a b c] (:typeid (meta a))))
(defmulti fw-realize! "" (fn [a] (:typeid (meta a))))
(defmulti fw-evaluate! "" (fn [a b] (:typeid (meta a))))

(defmulti ac-realize! "" (fn [a b] (:typeid (meta a))))
(defmulti ac-reify "" (fn [a b] (:typeid (meta a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn fw-rerun "" [fw]
  (let [ pipe (.getf fw :pipeline) ]
    (-> pipe (.container) (.core) (.reschedule fw))
    fw))

(defn fw-runafter [fw]
  (let [ pipe (.getf fw :pipeline)
         c (-> pipe (.container)(.core))
         id (:typeid (meta fw))
         np (.getf fw :next) ]
    (cond
      (= id :DelayPoint)
      (.delay c np (.getf fw :delayMillis))

      (= id :AsyncWaitPoint)
      (.hold c np)

      (= id :NihilPoint)
      (.stop pipe)

      :else
      (.run c fw))
    fw))


(defn fw-run "" [fw]
  (let [ pipe (.getf fw :pipeline)
         c (-> pipe (.container)(.core))
         job (.job pipe)
         np (.getf fw :next) ]
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
    (.setf fw :attmt nil)
    c))

(defn fw-setattmt! "" [fw c]
  (do
    (.setf fw :attmt c)
    fw))

(defmethod fw-configure! :default [fw ac cur]
  (do
    (.setf fw :pipeline (.getf cur :pipeline))
    (.setf fw :pid (SN/next-long))
    (.setf fw :template ac)
    (.setf fw :next cur)
    fw))

(defmethod fw-realize! :default [fw]
  (let [ a (.getf fw :template) ]
    (ac-realize! a fw)
    fw))

(defmethod fw-evaluate! :default [fw job] fw)

(defmethod ac-realize! :default [ac fw] fw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

          (run [me#] (CU/TryC (fw-run me#)))

        FlowPoint
          ~@(filterv CU/notnil? args))

      { :typeid ~(keyword (last args))  :isa :FlowPoint } )))

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

       { :typeid ~(keyword (last args)) :isa :Activity } )) )


(defmacro ac-spawnpoint "" [ ac cur & args ]
  `(let [ pipe# (.getf ~cur :pipeline)
          f# (make-flowpoint ~@args) ]
    (fw-configure! f# ~ac ~cur)
    (fw-realize! f#)))


(defn make-nihil "" [] (make-activity Nihil) )

(defn ac-reify-nihil "" [pipe]
  (let [ f (make-flowpoint NihilPoint) ]
    (fw-configure! f (make-nihil) f)
    (fw-realize! f)))


(defprotocol PipelineDelegateAPI
  (getStart [_] )
  (getStop [_] )
  (getError [_] ))

(defprotocol PipelineAPI
  (container [_] )
  (isActive [_] )
  (onError [_ error curPoint] )
  (start [_] )
  (stop [_] ))



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
          (container [_] (.container job))

          (onError [this err cur]
            (let [ h (.getError delegate) ]
              (error err)
              (let [ a (if (nil? h)
                         nil
                         (try
                            (apply h err cur)
                            (catch Throwable e#
                              (do (error e#) nil)))) ]
                (if (nil? a)
                  (ac-reify-nihil this)
                  a))))

          (start [this]
            (with-local-vars [ f9 nil ]
              (let [ f1 (ac-reify-nihil this)
                     h (.getStart delegate) ]
                (debug "Pipeline " pid " starting...")
                (when (nil? h) (warn "no start function defined by delegate!"))
                (try
                  (let [ a (if (nil? h) nil (apply h this))
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

          (stop [_]
            (try
              (debug "Pipeline " pid " stopping...")
              (let [ h (.getStop delegate) ]
                (when-not (nil? h)
                  (apply h)))
              (catch Throwable e# (error e#)))) )

      { :typeid :Pipeline } )) )


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

