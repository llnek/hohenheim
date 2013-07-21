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
  comzotohcljc.wflow.delays)



(use '[clojure.tools.logging :only (info warn error debug)])
(import '(com.zotoh.hohenheim.core Job))

(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.util.coreutils :as CU])

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AsyncWaitPoint)
(defprotocol AsyncWait)

(defprotocol DelayPoint)
(defprotocol Delay)

(defprotocol FAsyncResumeToken
  (resume [_ resArg] ))

(defn async-resume-token [fw]
  (reify
    FAsyncResumeToken
      (resume [_ resArg]
        (let [ np (.getf fw :next) ]
          (when-not (nil? np)
            (fw-setattmt! np resArg)
            (fw-rerun np))))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delay

(defn make-delay [delayMillis]
  (let [ b (make-activity :Delay Delay) ]
    (.setf b :delayMillis delayMillis)
    b))

(defmethod ac-reify :Delay [ac cur]
  (ac-spawnpoint ac cur :DelayPoint DelayPoint))

(defmethod ac-realize! :Delay [ac fw]
  (let [ d (.getf ac :delayMillis) ]
    (.setf fw :delayMillis d)
    fw))

(defmethod fw-evaluate! :DelayPoint [fw job] fw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AsyncWait

(defn make-asyncwait []
  (let [ b (make-activity :AsyncWait AsyncWait) ]
    b))

(defmethod ac-reify :AsyncWait [ac cur]
  (ac-spawnpoint ac cur :AsyncWaitPoint AsyncWaitPoint))

(defmethod fw-evaluate! :AsyncWaitPoint [fw job] fw)
(defmethod ac-realize! :AsyncWait [ac fw] fw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private delays-eof nil)


