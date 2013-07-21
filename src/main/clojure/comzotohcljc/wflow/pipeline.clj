
(ns ^{ doc: ""
       :author "kenl" }
  comzotohcljc.wflow.sysobjs )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JobAPI
  (container [_] ))

(deftype Job

(defprotocol PipelineAPI
  (container [_] )
  (isActive [_] )
  (onError [_ error curPoint] )
  (start [_] )
  (stop [_] ))

(deftype Pipeline [job pid ^:unsynchronized-mutable active delegate]

  Object

  (finalize [this]
    (debug (str "=====================>" (toString this) " finz'ed")) )

  (toString [_]
    (str (-> (.getClass)(.getSimpleName)) "(" pid ")" ))

  PipelineAPI

  (container [_] (.container job))

  (isActive [_] active)

  (onError [this err cur]
    (let [ h (:on-error delegate) ]
      (error err)
      (if (nil? h)
        (ac-reify-nihil this)
        (try
          (apply h err cur)
          (catch Throwable e#
            (do (error e#) (ac-reify-nihil this))
            )))))

  (start [this]
    (let [ f1 (ac-reify-nihil this)
           h (:on-start delegate) ]
      (debug "Pipeline " pid " starting...")
      (try
        (let [ a (apply h this)
               f2 (cond
                      (or (nil? a) (satisfies? Nihil a))
                      (ac-reify-nihil this)
                      :else
                      (ac-reify a f1)) ]
        (-> this (.container) (.core) (.run f2))
        (set! active true)
        (catch Throwable e#
          (onError this e f2))))) )

  (stop [_]
    (try
      (debug "Pipeline " pid " stopping...")
      (let [ h (:on-stop delegate) ]
        (when-not (nil? h)
          (apply h)))
      (catch Throwable e# (error e#))))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-pipelie [job delegate]
  (let [ pid (SN/next-long)
         pipe (Pipeline. job pid false delegate) ]
    (debug "Pipeline " pid " created.")
    (SU/test-nonil "the-job" job)))


(defn- no-flow [pipe job]
  (let [ f (fn [_] (ac-reify-nihil pipe)) ]
  (make-pipeline job { :on-start f })) )














(def ^:private sysobjs-eof nil)

