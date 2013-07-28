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

  comzotohcljc.dbio.connect )

(use '[clojure.tools.logging :only (info warn error debug)])

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
  DBIOLocal DBIOError OptLockError))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def POSTGRESQL-DRIVER "org.postgresql.Driver")
(def MYSQL-DRIVER "com.mysql.jdbc.Driver")
(def H2-DRIVER "org.h2.Driver" )
(def POSTGRESQL-URL "jdbc:postgresql://{{host}}:{{port}}/{{db}}" )
(def H2-SERVER-URL "jdbc:h2:tcp://host/path/db" )
(def H2-MEM-URL "jdbc:h2:mem:" )
(def H2-FILE-URL "jdbc:h2:{{path}};MVCC=TRUE" )
(def H2_MVCC ";MVCC=TRUE" )

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
  (let [ tloc (DBIOLocal/getCache) ;; a thread local
         c (.get tloc) ;; c == java hashmap
         m (.get c hc) ]
    (when-not (nil? m)
      (CU/Guard (.shutdown m))
      (.remove c hc))))

(defn- maybe-get-pool [hc jdbc options]
  (let [ tloc (DBIOLocal/getCache) ;; get the thread local
         c (.get tloc) ] ;; c == java hashmap
    ;; check if pool is there
    (when-not (.containsKey c hc)
      (debug "no db pool found in thread-local, creating one...")
      (let [ p (DU/make-db-pool jdbc options) ]
        (.put c hc p)))
    (.get c hc)))


(defn dbio-connect "" [jdbc metaCache options]
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



(def ^:private connect-eof nil)

