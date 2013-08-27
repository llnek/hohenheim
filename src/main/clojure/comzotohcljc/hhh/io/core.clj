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

  comzotohcljc.hhh.io.core )

(import '(com.zotoh.frwk.server Service))

(import '(com.zotoh.frwk.core
  Versioned Hierarchial
  Identifiable Disposable Startable))
(import '(com.zotoh.hohenheim.core Container))
(import '(com.zotoh.hohenheim.io ServletEmitter Emitter))

(import '(java.util HashMap))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hhh.core.sys])

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defprotocol EmitterAPI
  ""
  (enabled? [_] )
  (active? [_] )

  (suspend [_] )
  (resume [_] )

  (release [_ wevt] )
  (hold [_ wevt] )
  (dispatch [_ ev] ))

(defprotocol WaitEventHolder ""
  (resumeOnResult [_ res] )
  (timeoutMillis [_ millis] )
  (onExpiry [_])
  (timeoutSecs [_ secs] ) )

(defprotocol AsyncWaitTrigger ""
  (resumeWithResult [_ res] )
  (resumeWithError [_] )
  (emitter [_] ))



(defmulti ioes-dispatch "" (fn [a & args] (:typeid (meta a))))
(defmulti ioes-dispose "" (fn [a] (:typeid (meta a))))
(defmulti ioes-reify-event "" (fn [a & args] (:typeid (meta a))))

(defmulti ioes-start "" (fn [a] (:typeid (meta a))))
(defmulti ioes-stop "" (fn [a] (:typeid (meta a))))
(defmulti ioes-suspend "" (fn [a] (:typeid (meta a))))
(defmulti ioes-resume "" (fn [a] (:typeid (meta a))))

(defmulti ioes-stopped "" (fn [a] (:typeid (meta a))))
(defmulti ioes-started "" (fn [a] (:typeid (meta a))))


(defmethod ioes-started :default [co]
  (info "Emitter " (:typeid (meta co)) " started - OK"))

(defmethod ioes-stopped :default [co]
  (info "Emitter "  (:typeid (meta co))  " stopped - OK"))

(defmethod ioes-dispose :default [co]
  (info "Emitter "  (:typeid (meta co))  " disposed - OK"))

(defmethod ioes-suspend :default [co]
  (throw (Exception. "Not Implemented")))

(defmethod ioes-resume :default [co]
  (throw (Exception. "Not Implemented")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-emitter "" [^Container parObj emId]
  (let [ eeid (SN/next-long)
         impl (CU/make-mmap) ]
    (.mm-s impl :backlog (HashMap.))
    (with-meta
      (reify

        Thingy

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )

        Versioned
          (version [_] "1.0")

        Hierarchial
          (parent [_] parObj)

        Identifiable
          (id [_] eeid)

        Emitter
          (container [this] (.parent this))

        Disposable

          (dispose [this] (ioes-dispose this))

        Startable

          (start [this] (ioes-start this))
          (stop [this] (ioes-stop this))

        Service
          (getv [_ k] (.mm-g impl (keyword k)))
          (setv [_ k v]
                (.mm-s impl (keyword k) v))

        EmitterAPI

          (enabled? [_] (if (false? (.mm-g impl :enabled)) false true ))
          (active? [_] (if (false? (.mm-g impl :active)) false true))

          (suspend [this] (ioes-suspend this))
          (resume [this] (ioes-resume this))

          (release [_ wevt]
            (when-not (nil? wevt)
              (let [ ^HashMap b (.mm-g impl :backlog)
                     wid (.id ^Identifiable wevt) ]
                (debug "emitter releasing an event with id: " wid)
                (.remove b wid))))

          (hold [_ wevt]
            (when-not (nil? wevt)
              (let [ ^HashMap b (.mm-g impl :backlog)
                     wid (.id ^Identifiable wevt) ]
                (debug "emitter holding an event with id: " wid)
                (.put b wid wevt))))

          (dispatch [this ev]
            (CU/TryC
                (.notifyObservers parObj ev) )) )

      { :typeid emId } )))

(defmethod comp-contextualize :czc.hhh.io/Emitter [co ctx]
  (let [ ^comzotohcljc.hhh.core.sys.Thingy c ctx ]
    (comp-clone-context co (.getCtx c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(derive :czc.hhh.io/HTTP :czc.hhh.io/Emitter)

(derive :czc.hhh.io/JettyIO :czc.hhh.io/HTTP)
(derive :czc.hhh.io/NettyIO :czc.hhh.io/HTTP)
(derive :czc.hhh.io/NettyMVC :czc.hhh.io/NettyIO)

(derive :czc.hhh.io/RepeatingTimer :czc.hhh.io/Emitter)
(derive :czc.hhh.io/OnceTimer :czc.hhh.io/Emitter)
(derive :czc.hhh.io/ThreadedTimer :czc.hhh.io/RepeatingTimer)

(derive :czc.hhh.io/FilePicker :czc.hhh.io/ThreadedTimer)
(derive :czc.hhh.io/IMAP :czc.hhh.io/ThreadedTimer)
(derive :czc.hhh.io/POP3 :czc.hhh.io/ThreadedTimer)

(derive :czc.hhh.io/JMS :czc.hhh.io/Emitter)
(derive :czc.hhh.io/SocketIO :czc.hhh.io/Emitter)



(def ^:private core-eof nil)

