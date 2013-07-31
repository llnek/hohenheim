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

  comzotohcljc.wflow.user )


(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.hohenheim.core Job))

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PTaskPoint)
(defprotocol PTask)
(defprotocol Work
  (perform [_ fw job] ))

(defn- make-ptask-work [cb]
  (let [ impl (CU/make-mmap) ]
    (reify Work
      (perform [_ fw job]
        (let [ c (fw-popattmt! fw) ]
          (.mm-s impl :res nil)
          (.mm-s impl :cur fw)
          (cb fw job c))))))

(defn make-ptask "Create a PTask Activity."
  [cb]
  (let [ b (make-activity PTask) ]
    (.setf! b :task (make-ptask-work cb))
    b))

(defmethod ac-reify :comzotohcljc.wflow.user/PTask
  [ac cur]
  (ac-spawnpoint ac cur PTaskPoint))

(defmethod ac-realize! :comzotohcljc.wflow.user/PTask
  [ac fw]
  (let [ w (.getf ac :task) ]
    (.setf! fw :task w)
    fw))

(defmethod fw-evaluate! :comzotohcljc.wflow.user/PTaskPoint
  [fw job]
  (do
    (debug "[" (.getf fw :pid) "] about to perform work.")
    (let [ pipe (fw-pipe* fw)
           w (.getf fw :task)
           np (fw-next* fw)
           na (.perform w fw job) ]
      (with-local-vars [rc np]
        (when-not (nil? na)
          (if (= :czc.wflow/Nihil (.moniker na))
            (var-set rc (ac-reify-nihil pipe))
            (var-set rc (ac-reify na @rc))))
        @rc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private user-eof nil)


