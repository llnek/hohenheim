;;
;; Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
;;
;; This library is distributed in the hope that it will be useful
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;



(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.dbio.connect )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(java.util Map HashMap))

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.dbio.core :as DU])
(use '[comzotohcljc.dbio.composite])
(use '[comzotohcljc.dbio.simple])

(use '[comzotohcljc.dbio.sqlserver])
(use '[comzotohcljc.dbio.postgresql])
(use '[comzotohcljc.dbio.mysql])
(use '[comzotohcljc.dbio.oracle])
(use '[comzotohcljc.dbio.h2])

(import '(com.zotoh.frwk.dbio
  DBAPI JDBCPool
  DBIOLocal DBIOError OptLockError))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def POSTGRESQL-DRIVER "org.postgresql.Driver")
(def MYSQL-DRIVER "com.mysql.jdbc.Driver")
(def H2-DRIVER "org.h2.Driver" )
(def POSTGRESQL-URL "jdbc:postgresql://{{host}}:{{port}}/{{db}}" )
(def H2-SERVER-URL "jdbc:h2:tcp://host/path/db" )
(def H2-MEM-URL "jdbc:h2:mem:" )
(def H2-FILE-URL "jdbc:h2:{{path}};MVCC=TRUE" )
(def H2_MVCC ";MVCC=TRUE" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hashJdbc ^long [jdbc]
  (.hashCode
    (str (:driver jdbc) (:url jdbc)
         (:user jdbc) (SU/nsb (:pwdObj jdbc)))))

(defn registerJdbcTL "" [jdbc options]
  (let [ tloc (DBIOLocal/getCache)
         ^Map c (.get tloc)
         hc (hashJdbc jdbc) ]
    (when-not (.containsKey c hc)
      (debug "no db pool found in thread-local, creating one...")
      (let [ p (DU/make-db-pool hc jdbc options) ]
        (.put c hc p)))
    (.get c hc)))

(defn- maybe-finz-pool "" [ hc]
  (let [ tloc (DBIOLocal/getCache) ;; a thread local
         ^Map c (.get tloc) ;; c == java hashmap
         p (.get c hc) ]
    (when-not (nil? p)
      (CU/Try! (.shutdown ^JDBCPool p))
      (.remove c hc))))

(defn- maybe-get-pool ""

  ^JDBCPool
  [ hc jdbc options]

  (let [ tloc (DBIOLocal/getCache) ;; get the thread local
         ^Map c (.get tloc)
         rc (.get c hc) ]
    (if (nil? rc)
      (registerJdbcTL jdbc options)
      rc)))

(defn dbio-connect "Connect to a datasource."

  ^DBAPI
  [jdbc metaCache options]

  (let [ dbv (DU/resolve-vendor jdbc)
         hc (hashJdbc jdbc) ]
    (reify DBAPI

      (supportsOptimisticLock [_]
        (if (false? (:opt-lock options)) false true))
      (vendor [_] dbv)
      (finz [_] (maybe-finz-pool hc))

      (open [_]
        (let [ ^JDBCPool
               p (maybe-get-pool hc jdbc options) ]
          (if (nil? p)
            nil
            (.nextFree p))))

      (newCompositeSQLr [this]
        (compositeSQLr metaCache this))

      (newSimpleSQLr [this]
        (simpleSQLr metaCache this)) )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private connect-eof nil)

