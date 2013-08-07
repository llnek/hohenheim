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

  comzotohcljc.hhh.io.loops )

(import '(java.util Date Timer TimerTask))
(import '(com.zotoh.hohenheim.io TimerEvent))
(import '(com.zotoh.hohenheim.core Startable))

(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.util.core :only (MuObj) ])

(require '[comzotohcljc.util.process :as PU])
(require '[comzotohcljc.util.dates :as DU])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hhh.io.events  :only (make-timer-event) ])

(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defmulti loopable-oneloop "" (fn [a] (:typeid (meta a)) ))
(defmulti loopable-wakeup "" (fn [a & args] (:typeid (meta a)) ))
(defmulti loopable-schedule "" (fn [a] (:typeid (meta a)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- config-repeat-timer [^Timer tm delays ^long intv func]
  (let [ tt (proxy [TimerTask][]
              (run [_]
                (CU/TryC
                    (when (fn? func) (func)))))
         [^Date dw ^long ds] delays ]
    (when (instance? Date dw)
      (.schedule tm tt dw intv) )
    (when (number? ds)
      (.schedule tm tt ds intv))) )

(defn- config-timer [^Timer tm delays func]
  (let [ tt (proxy [TimerTask][]
              (run [_]
                (when (fn? func) (func))))
         [^Date dw ^long ds] delays]
    (when (instance? Date dw)
      (.schedule tm tt dw) )
    (when (number? ds)
      (.schedule tm tt ds))) )


(defn- config-timertask [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ intv (.getAttr co :intervalMillis)
         t (.getAttr co :timer)
         ds (.getAttr co :delayMillis)
         dw (.getAttr co :delayWhen)
         func (fn [] (loopable-wakeup co)) ]
    (if (number? intv)
      (config-repeat-timer t [dw ds] intv func)
      (config-timer t [dw ds] func))
    co))


(defn cfg-loopable [^comzotohcljc.hhh.core.sys.Thingy co cfg]
  (let [ intv (:interval-secs cfg)
         ds (:delay-secs cfg)
         dw (SU/nsb (:delay-when cfg)) ]
    (when (SU/hgl? dw)
      (.setAttr! co :delayWhen (DU/parse-iso8601 (SU/strim dw))))
    (when-not (number? ds)
      (.setAttr! co :delayMillis (* 1000 ds)))
    (when-not (number? intv)
      (.setAttr! co :intervalMillis (* 1000 intv)))
    (info "loopable config: " cfg)
    co))

(defn- start-timer [^comzotohcljc.hhh.core.sys.Thingy co]
  (do
    (.setAttr! co :timer (Timer. true))
    (loopable-schedule co)))

(defn- kill-timer [^comzotohcljc.hhh.core.sys.Thingy co]
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
      (reify TimerEvent
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
      (reify TimerEvent
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

(defmethod loopable-wakeup :czc.hhh.io/ThreadedTimer
  [co & args]
  (do
    (CU/TryC
        (loopable-oneloop co) )
    (PU/safe-wait (first args) )) )


(defmethod ioes-start :czc.hhh.io/ThreadedTimer
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ds (.getAttr co :delayMillis)
         dw (.getAttr co :delayWhen)
         intv (.getAttr co :intervalMillis)
         loopy (atom true)
         func (fn []
                (PU/coroutine (fn []
                                (while @loopy (loopable-wakeup co intv))))) ]
    (.setAttr! co :loopy loopy)
    (if (or (number? ds) (instance? Date dw))
      (config-timer (Timer.) dw ds func)
      (func))
    (ioes-started co)))


(defmethod ioes-stop :czc.hhh.io/ThreadedTimer
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ loopy (.getAttr co :loopy) ]
    (reset! loopy false)
    (ioes-stopped co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(derive :czc.hhh.io/RepeatingTimer :czc.hhh.io/Emitter)
(derive :czc.hhh.io/OnceTimer :czc.hhh.io/Emitter)
(derive :czc.hhh.io/ThreadedTimer :czc.hhh.io/RepeatingTimer)


(def ^:private loops-eof nil)


