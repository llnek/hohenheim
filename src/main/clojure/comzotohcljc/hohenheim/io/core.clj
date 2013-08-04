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

  comzotohcljc.hohenheim.io.core )

(import '(com.zotoh.hohenheim.core Identifiable Disposable Startable))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hohenheim.core.sys])

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
  (info "Emitter " co " started - OK"))

(defmethod ioes-stopped :default [co]
  (info "Emitter " co " stopped - OK"))

(defmethod ioes-dispose :default [co]
  (info "Emitter " co " disposed - OK"))

(defmethod ioes-suspend :default [co]
  (throw (Exception. "Not Implemented")))

(defmethod ioes-resume :default [co]
  (throw (Exception. "Not Implemented")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-emitter "" [container emId]
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :backlog (atom {}) )
    (with-meta
      (reify

        Component

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (version [_] "1.0")
          (parent [_] container)
          (id [_] emId)

        Disposable

          (dispose [this] (ioes-dispose this))

        Startable

          (start [this] (ioes-start this))
          (stop [this] (ioes-stop this))

        EmitterAPI

          (enabled? [_] (if (false? (.mm-g impl :enabled)) false true ))
          (active? [_] (if (false? (.mm-g impl :active)) false true))

          (suspend [this] (ioes-suspend this))
          (resume [this] (ioes-resume this))

          (release [_ wevt]
            (when-not (nil? wevt)
              (let [ b (.mm-g impl :backlog)
                     wid (.id ^Identifiable wevt) ]
                (swap! b dissoc wid))))

          (hold [_ wevt]
            (when-not (nil? wevt)
              (let [ b (.mm-g impl :backlog)
                     wid (.id ^Identifiable wevt) ]
                (swap! b assoc wid wevt))))

          (dispatch [this ev]
            (CU/TryC
                (.notifyObservers container ev) )) )

      { :typeid id } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(derive :czc.hhh.io/HTTP :czc.hhh.io/Emitter)

(def ^:private core-eof nil)

