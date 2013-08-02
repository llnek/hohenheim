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

  comzotohcljc.util.scheduler )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.frwk.util
  RunnableWithId Schedulable TCore ))
(import '(java.util
  HashMap Properties
  Timer TimerTask))

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- xrefPID [w]
  (if (instance? RunnableWithId w)
    (let [p (.getId w) ]
      (debug "flow-point has pid " p)
      p)
    nil))

(defprotocol SchedulerAPI ""
  (preRun [_ w] )
  (activate [_ options] )
  (deactivate [_] )
  (addTimer [_ task dely] ) )

(defn make-scheduler "" [parObj]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        SchedulerAPI

          (preRun [this w]
            (let [pid (xrefPID w) ]
              (when-not (nil? pid)
                (let [ holdQ (.mm-g impl :holdQ)
                       runQ (.mm-g impl :runQ) ]
                  (.remove holdQ pid)
                  (.put runQ pid w)))) )

          (activate [_ options]
            (let [ t (:threads options) ]
              (.mm-s impl :holdQ (HashMap.))
              (.mm-s impl :runQ (HashMap.))
              (.mm-s impl :core
                     (TCore. (CU/uid) (if (nil? t) 4 t)))
              (.mm-s impl :timer (Timer. (CU/uid) true))
              (.start (.mm-g impl :core)) ) )

          (deactivate [_]
            (do
              (.cancel (.mm-g impl :timer))
              (.clear (.mm-g impl :holdQ))
              (.clear (.mm-g impl :runQ))
              (.stop (.mm-g impl :core))) )

          (addTimer [_ task dely]
            (.schedule (.mm-g impl :timer) task dely))

        Schedulable

          ;; called by a *running* task to remove itself from the running queue
          (dequeue [_ w]
            (let [ runQ (.mm-g impl :runQ)
                   pid (xrefPID w) ]
              (when-not (nil? pid)
                (.remove runQ pid))) )

          (run [this w]
            (do
              (preRun this w)
              (.schedule (.mm-g impl :core) w)) )

          (postpone [this w delayMillis]
            (cond
              (= delayMillis 0)
              (.run this w)

              (< delayMillis 0)
              (.hold this w)

              :else
              (do
                (addTimer this
                  (proxy [TimerTask] []
                    (run [_] (.wakeup this w))) delayMillis)) ))

          (hold [this w]
            (let [ pid (xrefPID w) ]
              (.hold this pid w)))

          (hold [this pid w]
            (when-not (nil? pid)
              (let [ holdQ (.mm-g impl :holdQ)
                     runQ (.mm-g impl :runQ) ]
                (.remove runQ pid)
                (.put holdQ pid w) )))

          (wakeup [this w]
            (let [ pid (xrefPID w) ]
              (.wakeAndRun this pid w)))

          (wakeAndRun [this pid w]
            (when-not (nil? pid)
              (let [ holdQ (.mm-g impl :holdQ)
                     runQ (.mm-g impl :runQ) ]
                (.remove holdQ pid)
                (.put runQ pid w)
                (.run this w) )))

          (reschedule [this w]
            (when-not (nil? w)
              (.run this w)))

          (dispose [_]
            (let [c (.mm-g impl :core) ]
              (when-not (nil? c) (.dispose c)))) )

      { :typeid (keyword "czc.frwk.util/Scheduler") } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;











(def ^:private scheduler-eof nil)


