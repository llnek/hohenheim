;;
;; Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
;;
;; This library is distributed in the hope that it will be useful
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hhh.io.loops )

(import '(java.util Date Timer TimerTask))
(import '(com.zotoh.hohenheim.io TimerEvent))
(import '(com.zotoh.frwk.core Identifiable Startable))

(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.util.core :only (MuObj) ])

(require '[comzotohcljc.util.process :as PU])
(require '[comzotohcljc.util.dates :as DU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defmulti loopable-oneloop "" (fn [a] (:typeid (meta a)) ))
(defmulti loopable-wakeup "" (fn [a & args] (:typeid (meta a)) ))
(defmulti loopable-schedule "" (fn [a] (:typeid (meta a)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- config-repeat-timer [^Timer tm delays intv func]
  (let [ tt (proxy [TimerTask][]
              (run []
                (CU/TryC
                    (when (fn? func) (func)))))
         [^Date dw ^long ds] delays ]
    (when (instance? Date dw)
      (.schedule tm tt dw ^long intv) )
    (when (number? ds)
      (.schedule tm tt ds ^long intv))) )

(defn- config-timer [^Timer tm delays func]
  (let [ tt (proxy [TimerTask][]
              (run []
                (when (fn? func) (func))))
         [^Date dw ^long ds] delays]
    (when (instance? Date dw)
      (.schedule tm tt dw) )
    (when (number? ds)
      (.schedule tm tt ds))) )


(defn- config-timertask [^comzotohcljc.hhh.core.sys.Element co]
  (let [ intv (.getAttr co :intervalMillis)
         t (.getAttr co :timer)
         ds (.getAttr co :delayMillis)
         dw (.getAttr co :delayWhen)
         func (fn [] (loopable-wakeup co)) ]
    (if (number? intv)
      (config-repeat-timer t [dw ds] intv func)
      (config-timer t [dw ds] func))
    co))


(defn cfg-loopable [^comzotohcljc.hhh.core.sys.Element co cfg]
  (let [ intv (:interval-secs cfg)
         ds (:delay-secs cfg)
         dw (SU/nsb (:delay-when cfg)) ]
    (if (SU/hgl? dw)
      (.setAttr! co :delayWhen (DU/parse-date (SU/strim dw) "yyyy-MM-ddTHH:mm:ss"))
      (do
        (.setAttr! co :delayMillis
                   (* 1000 (Math/min (int 3)
                                     (int (if (number? ds) ds 3)))))))
    (when (number? intv)
      (.setAttr! co :intervalMillis (* 1000 intv)))
    (info "loopable config: " cfg)
    co))

(defn- start-timer [^comzotohcljc.hhh.core.sys.Element co]
  (do
    (.setAttr! co :timer (Timer. true))
    (loopable-schedule co)))

(defn- kill-timer [^comzotohcljc.hhh.core.sys.Element co]
  (let [ ^Timer t (.getAttr co :timer) ]
    (CU/TryC
        (when-not (nil? t) (.cancel t)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeating Timer

(defn make-repeating-timer [container]
  (make-emitter container :czc.hhh.io/RepeatingTimer))

(defmethod ioes-reify-event :czc.hhh.io/RepeatingTimer
  [co & args]
  (let [ eeid (SN/next-long) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        TimerEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (emitter [_] co)
        (isRepeating [_] true))
      { :typeid :czc.hhh.io/TimerEvent } )))

(defmethod comp-configure :czc.hhh.io/RepeatingTimer
  [co cfg]
  (cfg-loopable co cfg))

(defmethod ioes-start :czc.hhh.io/RepeatingTimer
  [co]
  (start-timer co)
  (ioes-started co))

(defmethod ioes-stop :czc.hhh.io/RepeatingTimer
  [co]
  (kill-timer co)
  (ioes-stopped co))

(defmethod loopable-wakeup :czc.hhh.io/RepeatingTimer
  [^comzotohcljc.hhh.io.core.EmitterAPI co & args]
  (.dispatch co (ioes-reify-event co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod loopable-schedule :default [co]
  (config-timertask co))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Once Timer

(defn make-once-timer [container]
  (make-emitter container :czc.hhh.io/OnceTimer))

(defmethod ioes-reify-event :czc.hhh.io/OnceTimer
  [co & args]
  (let [ eeid (SN/next-long) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        TimerEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (emitter [_] co)
        (isRepeating [_] false))
      { :typeid :czc.hhh.io/TimerEvent } )))

(defmethod comp-configure :czc.hhh.io/OnceTimer
  [co cfg]
  ;; get rid of interval millis field, if any
  (cfg-loopable co (dissoc cfg :interval-secs)))

(defmethod ioes-start :czc.hhh.io/OnceTimer
  [co]
  (start-timer co)
  (ioes-started co))

(defmethod ioes-stop :czc.hhh.io/OnceTimer
  [co]
  (kill-timer co)
  (ioes-stopped co))

(defmethod loopable-wakeup :czc.hhh.io/OnceTimer
  [^comzotohcljc.hhh.io.core.EmitterAPI co & args]
  (do
    (.dispatch co (ioes-reify-event co))
    (.stop ^Startable co)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threaded Timer

;;(defmethod loopable-oneloop :default [co] nil)

(defmethod loopable-schedule :czc.hhh.io/ThreadedTimer
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ intv (.getAttr co :intervalMillis)
         cl (MU/get-cldr)
         loopy (atom true)
         func (fn []
                (PU/coroutine (fn []
                                (while @loopy (loopable-wakeup co intv)))
                              cl)) ]
    (.setAttr! co :loopy loopy)
    (info "threaded one timer - interval = " intv)
    (func)))

(defmethod loopable-wakeup :czc.hhh.io/ThreadedTimer
  [co & args]
  (do
    (CU/TryC
        (loopable-oneloop co) )
    (PU/safe-wait (first args) )) )


(defmethod ioes-start :czc.hhh.io/ThreadedTimer
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ ds (.getAttr co :delayMillis)
         dw (.getAttr co :delayWhen)
         intv (.getAttr co :intervalMillis)
         loopy (atom true)
         cl (MU/get-cldr)
         func (fn [] (loopable-schedule co)) ]
    (.setAttr! co :loopy loopy)
    (if (or (number? ds) (instance? Date dw))
      (config-timer (Timer.) [dw ds] func)
      (func))
    (ioes-started co)))


(defmethod ioes-stop :czc.hhh.io/ThreadedTimer
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ loopy (.getAttr co :loopy) ]
    (reset! loopy false)
    (ioes-stopped co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private loops-eof nil)


