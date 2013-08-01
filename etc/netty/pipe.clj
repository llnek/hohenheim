(ns @@APPDOMAIN@@.pipe )

(import '(org.jboss.netty.handler.codec.http HttpResponseStatus))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.hohenheim.core Job))

;;import com.zotoh.blason.mvc.RouteInfo
(use '[comzotohcljc.hohenheim.io.events])
(use '[comzotohcljc.wflow.core])
(use '[comzotohcljc.wflow.user])

(deftype Handler [] PipelineDelegateAPI

  (getStart [_ pipe]
    (fn [pipe]
      (make-ptask
        (fn [fw job arg]
          (let [ ev (.event job)
                 src (.emitter ev)
                 c (.parent job)
                 res (http-response HttpResponseStatus/OK) ]
            (.setResult ev res))))))

  (getStop [_ pipe] nil)
  (getError [_ pipe error cur] nil))


(def ^:private pipe-eof nil)

