
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
            (fw-setattmt np resarg)
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
  (let [ b (make-activity AsyncWait) ]
    b))

(defmethod ac-reify :AsyncWait [ac cur]
  (let [ pipe (get (meta cur) :pipeline)
         f (make-FlowPoint pipe AsyncWaitPoint) ]
    (fw-configure! f ac cur)
    (fw-realize! f)))

(defmethod ac-realize! :AsyncWait [ac fw] fw)
(defmethod fw-evaluate! :AsyncWait [fw job] fw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private delays-eof nil)


