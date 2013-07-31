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


(require '[comzotohcljc.util.core :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti ioes-dispatch "" (fn [a & args] (:typeid (meta a))))
(defmulti ioes-start "" (fn [a] (:typeid (meta a))))
(defmulti ioes-stop "" (fn [a] (:typeid (meta a))))

(defmacro make-event-emitter [container id]
  `(with-meta (make-emitter ~container)
     { :typeid ~id } ))

(defn ioes-started [co]
  (info "Emitter " co " started - OK"))

(defn ioes-stopped [co]
  (info "Emitter " co " stopped - OK"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-emitter "" [container]
  (let [ impl (CU/make-mmap) ]
    (.mm-s impl :backlog (atom {}) )
    (with-meta
      (reify

        Component

          (setCtx! [_ x] (.mm-s impl :ctx x))
          (getCtx [_] (.mm-g impl :ctx))
          (parent [_] nil)
          (setAttr! [_ a v] (.mm-s impl a v) )
          (clrAttr! [_ a] (.mm-r impl a) )
          (getAttr [_ a] (.mm-g impl a) )
          (version [_] ver)
          (parent [_] container)
          (id [_] "")

        Disposable

          (dispose [_] )

        Startable

          (start [_] )
          (stop [_] )

        EmitterAPI

          (enabled? [_] (if (false? (.mm-g impl :enabled)) false true ))
          (active? [_] (if (false? (.mm-g impl :active)) false true))

          (suspend [_] )
          (resume [_] )

          (release [_ wevt]
            (when-not (nil? wevt)
              (let [ b (.mm-g impl :backlog)
                     wid (.id wevt) ]
                (swap! b dissoc wid))))

          (hold [_ wevt]
            (when-not (nil? wevt)
              (let [ b (.mm-g impl :backlog)
                     wid (.id wevt) ]
                (swap! b assoc wid wevt))))

          (dispatch [this ev]
            (CU/TryC
                (.notifyObservers container ev) )) )

      { } )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private core-eof nil)

