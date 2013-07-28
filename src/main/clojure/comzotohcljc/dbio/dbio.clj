
(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.dbio.dbapi )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(require '[comzotohcljc.dbio.core :as DU])
(require '[comzotohcljc.dbio.composite])
(require '[comzotohcljc.dbio.simple])

(use '[comzotohcljc.dbio.sqlserver])
(use '[comzotohcljc.dbio.postgresql])
(use '[comzotohcljc.dbio.mysql])
(use '[comzotohcljc.dbio.oracle])
(use '[comzotohcljc.dbio.h2])

(import '(com.zotoh.frwk.dbio
  DBIOLocal DBIOError OptLockError))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol DBAPI
  (supportsOptimisticLock [_] )
  (vendor [_]  )
  (finz [_] )
  (open [_] )
  (newCompositeSQLr [_] )
  (newSimpleSQLr [_] ) )


(defn- hashJdbc [jdbc]
  (.hashCode
    (str (:driver jdbc) (:url jdbc)
         (:user jdbc) (SU/nsb (:pwd jdbc)))))

(defn- maybe-finz-pool [hc]
  (let [ tloc (DBIOLocal/getCache)
         c (.get tloc)
         m (.get c hc) ]
    (when-not (nil? m)
      (CU/Guard (.shutdown m))
      (.remove c hc))))

(defn- maybe-get-pool [hc jdbc options]
  (let [ tloc (DBIOLocal/getCache)
         c (.get tloc)
         m (.get c hc) ]
    (when (nil? m)
      (debug "no db pool found in thread-local, creating one...")
      (let [ p (DU/make-db-pool jdbc options) ]
        (.put c hc p)))
    (.get c hc)))


(defn dbio-simple "" [jdbc metaCache options]
  (let [ dbv (DU/resolve-vendor jdbc)
         hc (hashJdbc jdbc) ]
    (reify DBAPI
      (supportsOptimisticLock [_]
        (if (false? (:opt-lock options)) false true))
      (vendor [_] dbv)
      (finz [_] (maybe-finz-pool hc))

      (open [_]
        (let [ p (maybe-get-pool hc jdbc options) ]
          (if (nil? p)
            nil
            (.nextFree p))))

      (newCompositeSQLr [this]
        (compositeSQLr this metaCache))

      (newSimpleSQLr [this]
        (simpleSQLr this metaCache)) )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private dbapi-eof nil)

