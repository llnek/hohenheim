(ns @@APPDOMAIN@@.pipe )

(import '(com.zotoh.hohenheim.core Job))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.wflow.core])

(deftype Handler [] PipelineDelegateAPI

  (getStart [_ pipe]
    (info "pipeline calling getStart")
    (fn [pipe]
      (make-ptask (fn [fw job arg]
                    (info "handled one job!")))))

  (getStop [_ pipe ]
    (info "pipeline calling getStop")
    (fn [pipe] (info "nothing more to do, just stop.")))

  (getError [_ pipe error cur]
    (info "pipeline encountered error" )
    (fn [pipe error cur]
      (info "error happens!  just quit")
      nil))
)






(def ^:private pipe-eof nil)


