(ns @@APPDOMAIN@@.core )


(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.hohenheim.impl.ext :only (AppMainAPI) ])

(deftype AppMain [] AppMainAPI

  (contextualize [_ container]
    (info "My AppMain contextualized by container " container))

  (configure [_ options]
    (info "My AppMain configured with options " options))

  (initialize [_]
    (info "My AppMain initialized!"))

  (dispose [_]
    (info "My AppMain finz'ed"))
)














(def ^:private core-eof nil)

