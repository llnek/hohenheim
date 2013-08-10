(ns @@APPDOMAIN@@.pipe )

(import '( com.zotoh.wflow 
  FlowPoint Activity Pipeline PipelineDelegate PTask Work))
(import '(com.zotoh.hohenheim.io HTTPEvent HTTPResult))
(import '(com.zotoh.wflow.core Job))
(use '[clojure.tools.logging :only (info warn error debug)])

(deftype Handler [] PipelineDelegate
  (getStartActivity [_  pipe] 
    (PTask. (reify Work
              (perform [_ fw job arg]
                (let [ ^HTTPEvent evt (.event job)
                       ^HTTPResult res (.getResultObj evt) ]
                  (.setStatus res 200)
                  (.setContent "hello world")
                  (.setHeader "content-type" "text/plain")
                  (.replyResut evt))))))

  (onStop [_ pipe]
    (info "nothing to be done here, just stop please."))

  (onError [ _ err curPt]
    (info "Oops, I got an error!")))




(def ^:private pipe-eof nil)




