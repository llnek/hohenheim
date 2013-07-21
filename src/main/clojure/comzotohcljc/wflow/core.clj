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

(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.util.coreutils :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MutableObjectAPI
  (setf [_ k v] )
  (getf [_ k] )
  (clrf [_ k] ))

(defprotocol FlowPoint)
(defprotocol Activity)

(defprotocol NihilPoint)
(defprotocol Nihil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^{ :doc "" } fw-configure! (fn [a b c] (:typeid (meta a))))
(defmulti ^{ :doc "" } fw-realize! (fn [a] (:typeid (meta a))))
(defmulti ^{ :doc "" } fw-evaluate! (fn [a b] (:typeid (meta a))))

(defmulti ^{ :doc "" } ac-realize! (fn [a b] (:typeid (meta a))))
(defmulti ^{ :doc "" } ac-reify (fn [a b] (:typeid (meta a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn fw-rerun [fw]
  (let [ pipe (.getf fw :pipeline) ]
    (-> pipe (.container) (.reschedule fw))
    fw))

(defn fw-runafter [fw]
  (let [ pipe (.getf fw :pipeline)
         c (.container pipe)
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


(defn ^{ :doc "" }
  fw-run
  [fw]
  (let [ pipe (.getf fw :pipeline)
         ct (.container pipe)
         job (.job pipe)
         np (.getf fw :next) ]
    (with-local-vars [ rc nil err nil ]
      (.dequeue ct fw)
      (try
        (var-set rc (fw-evaluate! fw job))
        (catch Throwable e#
          (var-set err (.onError pipe e# fw))))
      (when-not (nil? @err)
        (var-set rc @err))
      (if (nil? @rc)
        (do
          ;; indicate skip, happens with joins
          (debug "rc==null => skip."))
        (fw-runafter pipe @rc)))))

;; attmt is data passed back from previous async call, if any
(defn ^{ :doc "" }
  fw-popattmt!
  [fw]
  (let [ c (.getf fw :attmt) ]
    (.setf fw :attmt nil)
    c))

(defn ^{ :doc "" }
  fw-setattmt!
  [fw c]
  (do
    (.setf fw :attmt c)
    fw))

(defmethod fw-configure! :default
  [fw ac cur]
  (do
    (.setf fw :pipeline (.getf cur :pipeline))
    (.setf fw :pid (SN/next-long))
    (.setf fw :template ac)
    (.setf fw :next cur)
    fw))

(defmethod fw-realize! :default
  [fw]
  (let [ a (.getf fw :template) ]
    (ac-realize! a fw)
    fw))

(defmethod fw-evaluate! :default
  [fw job]
  fw)

(defmethod ac-realize! :default
  [ac fw]
  fw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro ^{ :doc "" } make-flowpoint [ id & args ]
  `(let [ impl# (CU/make-mmap) ]
    (with-meta (reify
                  MutableObjectAPI
                  (setf [_ k1# v1#] (.mm-s impl# k1# v1#))
                  (getf [_ k2#] (.mm-g impl# k2#))
                  (clrf [_ k3#] (.mm-r impl# k3#))
                  Runnable
                  (run [me#] (CU/TryC (fw-run me#)))
                  FlowPoint
                  ~@(filterv CU/notnil? args)) { :typeid ~id } )))

(defmacro make-activity [ id & args ]
  `(let [ impl# (CU/make-mmap) ]
     (with-meta (reify
                  MutableObjectAPI
                  (setf [_ k1# v1#] (.mm-s impl# k1# v1#))
                  (getf [_ k2#] (.mm-g impl# k2#))
                  (clrf [_ k3#] (.mm-r impl# k3#))
                  Activity
                  ~@(filterv CU/notnil? args)) { :typeid ~id } )) )

(defmacro ac-spawnpoint [ ac cur id & args ]
  `(let [ pipe# (.getf ~cur :pipeline)
          f# (make-flowpoint ~id ~@args) ]
    (fw-configure! f# ~ac ~cur)
    (fw-realize! f#)))

(defn ^{ :doc "" }
  make-nihil
  []
  (make-activity :Nihil Nihil) )

(defn ^{ :doc "" }
  ac-reify-nihil
  [pipe]
  (let [ f (make-flowpoint :NihilPoint NihilPoint) ]
    (fw-configure! f (make-nihil) f)
    (fw-realize! f)))


(defprotocol PipelineAPI
  (container [_] )
  (isActive [_] )
  (onError [_ error curPoint] )
  (start [_] )
  (stop [_] ))

(deftype Pipeline [job pid ^:unsynchronized-mutable active delegate]

  Object

  (finalize [this]
    (debug (str "=====================>" (.toString this) " finz'ed")) )

  (toString [this]
    (str (-> this (.getClass)(.getSimpleName)) "(" pid ")" ))

  PipelineAPI

  (container [_] (.container job))

  (isActive [_] active)

  (onError [this err cur]
    (let [ h (:on-error delegate) ]
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
             h (:on-start delegate) ]
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
            (-> this (.container) (.run @f9))
            (set! active true))
          (catch Throwable e#
              (onError this e# @f9))))) )

  (stop [_]
    (try
      (debug "Pipeline " pid " stopping...")
      (let [ h (:on-stop delegate) ]
        (when-not (nil? h)
          (apply h)))
      (catch Throwable e# (error e#))))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-pipeline [job delegate]
  (let [ pid (SN/next-long)
         pipe (Pipeline. job pid false delegate) ]
    (debug "Pipeline " pid " created.")
    (CU/test-nonil "the-job" job)))


(defn- no-flow [pipe job]
  (let [ f (fn [_] (ac-reify-nihil pipe)) ]
    (make-pipeline job { :on-start f })) )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private core-eof nil)

