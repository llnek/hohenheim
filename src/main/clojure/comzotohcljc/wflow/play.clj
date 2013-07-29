(ns comzotohcljc.wflow.play)

(import '(com.zotoh.hohenheim.core Job))

(require '[comzotohcljc.util.core :as CU])
(use '[comzotohcljc.util.core :only (MutableObjectAPI) ])

(use '[comzotohcljc.util.scheduler])
(use '[comzotohcljc.wflow.core])
(use '[comzotohcljc.wflow.user])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype PPP [] PipelineDelegateAPI

  (getStart [_]
    (fn [pipe]
      (make-ptask (fn [fw job arg] (println "called!!!")))
      ))

  (getStop [_]
    (fn [pipe] (println "DUDE, please stop!")))

  (getError [_] ))


(defprotocol CCC
  (core [_] ))

(defn makeccc []
  (reify
    CCC
    (core [_] )))

(def SCH (make-scheduler nil))
(def CTR (makeccc))

(defn makejob []
  (let [ impl (CU/make-mmap) ]
    (reify
      MutableObjectAPI
            (seq* [_] (CU/test-cond "not implemented" false))
            (setf! [_ k v] (.mm-s impl k v))
            (getf [_ k] (.mm-g impl k))
            (clear! [_] (.mm-c impl))
            (clrf! [_ k] (.mm-r impl k))

      Job
      (container [_] CTR)
      (id [_] 333)
      (event[_] nil))) )


(.activate SCH {} )

(def PIPE (make-pipeline (makejob) "comzotohcljc.wflow.play.PPP"))

(.start PIPE)




;;(.stop SCH {} )







(def ^:private play-eof nil)

