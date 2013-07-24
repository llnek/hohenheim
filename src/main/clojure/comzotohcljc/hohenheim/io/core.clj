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


(require '[comzotohcljc.util.coreutils :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti ioes-start "" (fn [a] (:typeid (meta a))))
(defmulti ioes-stop "" (fn [a] (:typeid (meta a))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-emitter "" [container]
  (let [ ;;chg (AtomicBoolean.)
         impl (CU/make-mmap) ]

    (.mm-s impl :backlog {} )
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
          (id [_] "")

        Disposable

          (dispose [_] )

        Startable

          (start [_] )
          (stop [_] )

        EmitterAPI

          (enabled? [_] (true? (.mm-g impl :enabled)) )
          (active? [_] (true? (.mm-g impl :active)) )
          ;;(setChanged! [_ b] (.set chg b) )

          (suspend [_] )
          (resume [_] )

          (release [_ wevt]
            (when-not (nil? wevt)
              (let [ b (.mm-g impl :backlog) ]
                (.mm-s impl :backlog (dissoc b (.id wevt))))))

          (hold [_ wevt]
            (when-not (nil? wevt)
              (let [ b (.mm-g impl :backlog) ]
                (.mm-s impl :backlog (assoc b (.id wevt) wevt)))))

          (dispatch [this ev]
            (try
                ;;(setChanged! this true)
                (.notifyObservers container ev)
              (catch Throwable e#
                (error e#))))


        )

      { :typeid xxx } )))


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private core-eof nil)

