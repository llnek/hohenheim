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

  comzotohcljc.dbio.core )

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[clojure.set])

(import '(java.sql
  SQLException DatabaseMetaData
  Connection Driver DriverManager))
(import '(java.util
  GregorianCalendar TimeZone Properties))
(import '(java.lang Math))
(import '(com.zotoh.frwk.dbio DBIOError))
(import '(com.jolbox.bonecp BoneCP BoneCPConfig))
(import '(org.apache.commons.lang3 StringUtils))

(use '[comzotohcljc.crypto.codec])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.str :as SU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:dynamic *GMT-CAL* (GregorianCalendar. (TimeZone/getTimeZone "GMT")) )
(def ^:dynamic *USE_DDL_SEP* true)
(def ^:dynamic *DDL_SEP* "-- :")
(def ^:dynamic *DDL_BVS* nil)

(defrecord JDBCInfo [^String driver ^String url ^String user
                     ^comzotohcljc.crypto.codec.Password pwdObj] )

(defprotocol DBAPI ""
  (supportsOptimisticLock [_] )
  (vendor [_]  )
  (finz [_] )
  (^Connection open [_] )
  (newCompositeSQLr [_] )
  (newSimpleSQLr [_] ) )


(defn make-jdbc
  ^comzotohcljc.dbio.core.JDBCInfo
  [^String driver ^String url ^String user
   ^comzotohcljc.crypto.codec.Password pwdObj]
  (JDBCInfo. driver url user pwdObj))

(def ^:dynamic *DBTYPES* {
    :sqlserver { :test-string "select count(*) from sysusers" }
    :postgresql { :test-string "select 1" }
    :mysql { :test-string "select version()" }
    :h2  { :test-string "select 1" }
    :oracle { :test-string "select 1 from DUAL" }
  })


(defn dbio-error [^String msg] (throw (DBIOError. msg)))

(defn- maybeGetVendor [^String product]
  (let [ lp (.toLowerCase product) ]
    (cond
      (SU/has-nocase? lp "microsoft") :sqlserver
      (SU/has-nocase? lp "postgres") :postgresql
      (SU/has-nocase? lp "h2") :h2
      (SU/has-nocase? lp "oracle") :oracle
      (SU/has-nocase? lp "mysql") :mysql
      :else (dbio-error (str "Unknown db product " product)))))

(defn match-dbtype "" [^String dbtype]
  (*DBTYPES* (keyword (.toLowerCase dbtype))))

(defn match-jdbc-url "" [^String url]
  (let [ ss (seq (.split url ":")) ]
    (if (> 1 (count ss))
      (match-dbtype (nth ss 1))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def BASEMODEL-MONIKER :dbio-basemodel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dbio-model [^String nm]
  {
    :id (keyword nm)
    :parent nil
    :table nm
    :abstract false
    :system false
    :indexes {}
    :uniques {}
    :fields {}
    :assocs {} })

(defmacro defmodel "" [model-name & body]
  `(let [ p#  (-> (dbio-model ~(name model-name))
               ~@body) ]
     (def ~model-name  p#)))

(defn with-db-parent-model [pojo par]
  (assoc pojo :parent par))

(defn with-db-table-name [pojo tablename]
  (assoc pojo :table tablename))

(defn with-db-indexes [pojo indices]
  (let [ a (:indexes pojo) ]
    (assoc pojo :indexes (merge a indices))))

(defn with-db-uniques [pojo uniqs]
  (let [ a (:uniques pojo) ]
    (assoc pojo :uniques (merge a uniqs))))

(defn with-db-field [pojo fid fdef]
  (let [ dft { :column (name fid)
               :size 255
               :domain :string
               :assoc-key false
               :pkey false
               :null true
               :auto false
               :dft false
               :dft-value ""
               :updatable true
               :system false
               :index "" }
         fd (assoc (merge dft fdef) :id fid)
         fm (:fields pojo)
         nm (assoc fm fid fd) ]
    (assoc pojo :fields nm)))

(defn with-db-fields [pojo flddefs]
  (with-local-vars [rcmap pojo]
    (doseq [ [k v] (seq flddefs) ]
      (var-set rcmap (with-db-field @rcmap k v)))
    @rcmap))

(defn with-db-assoc [pojo aid adef]
  (let [ dft { :kind nil :rhs nil :fkey "" :singly false }
         ad (merge dft adef)
         am (:assocs pojo)
         nm (assoc am aid ad) ]
    (assoc pojo :assocs nm)))

(defn with-db-assocs [pojo assocs]
  (with-local-vars [ rcmap pojo ]
    (doseq [ [k v] (seq assocs) ]
      (var-set rcmap (with-db-assoc @rcmap k v)))
    @rcmap))

(defn with-db-abstract [pojo] (assoc pojo :abstract true))

(defn- with-db-system [pojo] (assoc pojo :system true))

(defn- nested-merge [src des]
  (cond
    (and (map? src)(map? des)) (merge src des)
    (and (set? src)(set? des)) (union src des)
    :else des))

(defmodel dbio-basemodel
  (with-db-abstract)
  (with-db-system)
  (with-db-fields {
    :rowid {:column "dbio_rowid" :pkey true :domain :long
            :auto true :system true :updatable false}
    :verid {:column "dbio_version" :domain :long :system true
            :default true :default-value 0}
    :last-modify {:column "dbio_lastchanged" :domain :timestamp
               :system true :default true}
    :created-on {:column "dbio_created_on" :domain :timestamp
                  :system true :default true :updatable false}
    :created-by {:column "dbio_created_by" :system true :domain :string } }))

(defprotocol MetaCacheAPI (getMetas [_] ))

(defprotocol SchemaAPI (getModels [_] ))

(defn make-Schema

  ^comzotohcljc.dbio.core.SchemaAPI
  [theModels]

  (reify SchemaAPI
    (getModels [_] theModels)) )


(defn- resolve-local-assoc [ms zm]
  (let [ socs (:assocs zm)
         zid (:id zm) ]
    (if (or (nil? socs) (empty? socs))
      #{}
      (with-local-vars [rc (transient #{}) ]
        (doseq [ [id soc] (seq socs) ]
          (let [ kind (:kind soc)
                 rhs (:rhs soc)
                 ^String col (case kind
                        :o2m
                        (str (name rhs) "|" "fk_"
                             (name zid) "_" (name id))
                        :o2o
                        (str (name zid) "|" "fk_"
                             (name rhs) "_" (name id))
                        :m2m
                        (if (nil? (get ms (:joined soc)))
                          (dbio-error
                            (str "Missing joined model for m2m assoc " id))
                          "")

                        (dbio-error (str "Invalid assoc type " kind))) ]
            (when (SU/hgl? col)
              (var-set rc (conj! @rc col)))))
        (persistent! @rc) ))))

(defn- resolve-assoc [ms m]
  (let [ par (:parent m) ]
    (if (nil? par)
      (union #{} (resolve-local-assoc ms m))
      (union #{} (resolve-local-assoc ms m) (resolve-assoc ms (get ms par))))))

(defn- resolve-assocs [ms]
  (with-local-vars [ rc #{} ]
    (doseq [ en (seq ms) ]
      (var-set rc (union @rc (resolve-assoc ms (last en)) )))
    @rc))

(defn- inject-fkeys-models [ms fks]
  (with-local-vars [ rc (merge {} ms) ]
    (doseq [ ^String k (seq fks) ]
      (let [ ss (.split k "\\|")
             id (keyword (nth ss 0))
             fid (keyword (nth ss 1))
             pojo (get ms id) ]
        (var-set rc
                 (assoc @rc id
                        (with-db-field pojo fid
                                       { :domain :long :assoc-key true } )))))
    @rc))

(defn- resolve-parent [ms m]
  (let [ par (:parent m) ]
    (cond
      (keyword? par) (if (nil? (get ms par))
                       (dbio-error (str "Unknown model " par))
                       m)
      (nil? par)
      (assoc m :parent BASEMODEL-MONIKER)

      :else (dbio-error (str "Invalid parent " par)))))

(defn- resolve-parents [ms]
  (persistent! (reduce (fn [sum en]
                          (let [ rc (resolve-parent ms (last en)) ]
                            (assoc! sum (:id rc) rc)))
                       (transient {})
                       (seq ms))) )

(defn- mapize-models [ms]
  (persistent! (reduce (fn [sum n]
                         (assoc! sum (:id n) n))
                       (transient {})
                       (seq ms))) )

(defn- collect-db-xxx-filter [a b]
  (cond
    (keyword? b) :keyword
    (map? b) :map
    :else (dbio-error (str "Invalid arg " b))))

(defmulti collect-db-fields collect-db-xxx-filter)

(defmethod collect-db-fields :keyword [cache modelid]
  (collect-db-fields cache (get cache modelid)))

(defmethod collect-db-fields :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:fields zm))
      (merge {} (:fields zm) (collect-db-fields cache par)))))

(defmulti collect-db-indexes collect-db-xxx-filter)

(defmethod collect-db-indexes :keyword [cache modelid]
  (collect-db-indexes cache (get cache modelid)))

(defmethod collect-db-indexes :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:indexes zm))
      (merge {} (:indexes zm) (collect-db-indexes cache par)))))

(defmulti collect-db-uniques collect-db-xxx-filter)

(defmethod collect-db-uniques :keyword [cache modelid]
  (collect-db-uniques cache (get cache modelid)))

(defmethod collect-db-uniques :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:uniques zm))
      (merge {} (:uniques zm) (collect-db-uniques cache par)))))

(defn- colmap-fields [flds]
  (with-local-vars [ sum (transient {}) ]
    (doseq [ [k v] (seq flds) ]
      (let [ cn (.toUpperCase (SU/nsb (:column v))) ]
        (var-set sum (assoc! @sum cn v))))
    (persistent! @sum)) )

(defn- meta-models [cache]
  (with-local-vars [ sum (transient {}) ]
    (doseq [ [k m] (seq cache) ]
      (let [ flds (collect-db-fields cache m)
             cols (colmap-fields flds) ]
        (var-set sum
                 (assoc! @sum k
                         (with-meta m
                                    { :columns cols :fields flds } ) ))))
    (persistent! @sum)) )

(defn make-MetaCache ""

  ^comzotohcljc.dbio.core.MetaCacheAPI
  [^comzotohcljc.dbio.core.SchemaAPI schema]

  (let [ ms (if (nil? schema) {} (mapize-models (.getModels schema)))
         m1 (if (empty? ms) {} (resolve-parents ms))
         m2 (assoc m1 BASEMODEL-MONIKER dbio-basemodel)
         m3 (inject-fkeys-models m2 (resolve-assocs m2))
         m4 (meta-models m3) ]
    (reify MetaCacheAPI
      (getMetas [_] m4))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- safeGetConn

  ^Connection
  [^comzotohcljc.dbio.core.JDBCInfo jdbc]

  (let [ ^String user (:user jdbc)
         ^String url (:url jdbc)
         ^String dv (:driver jdbc)
         d (if (SU/hgl? url) (DriverManager/getDriver url))
         p (if (SU/hgl? user)
               (doto (Properties.) (.put "password" (SU/nsb (:pwdObj jdbc)))
                                   (.put "user" user)
                                   (.put "username" user))
               (Properties.)) ]
    (when (nil? d)
      (dbio-error (str "Can't load Jdbc Url: " url)))
    (when
      (and (SU/hgl? dv)
           (not= (-> d (.getClass) (.getName)) dv))
        (warn "Expected " dv ", loaded with driver: " (.getClass d)))
    (.connect d url p)))

(defn make-connection "" 

  ^Connection
  [^comzotohcljc.dbio.core.JDBCInfo jdbc]

  (let [ ^String url (:url jdbc)
         ^Connection conn (if (SU/hgl? (:user jdbc))
                (safeGetConn jdbc)
                (DriverManager/getConnection url)) ]
    (when (nil? conn)
      (dbio-error (str "Failed to create db connection: " url)))
    (doto conn
      (.setTransactionIsolation  Connection/TRANSACTION_READ_COMMITTED))))

(defn test-connection "" [jdbc]
  (CU/TryC (.close (make-connection jdbc))))

(defmulti resolve-vendor class)

(defmethod resolve-vendor JDBCInfo
  [jdbc]
  (with-open [ conn (make-connection jdbc) ]
    (resolve-vendor conn)))

(defmethod resolve-vendor Connection

  [^Connection conn]
  (let [ md (.getMetaData conn) ]
    (-> { :id (maybeGetVendor (.getDatabaseProductName md)) }
      (assoc :version (.getDatabaseProductVersion md))
      (assoc :name (.getDatabaseProductName md))
      (assoc :quote-string (.getIdentifierQuoteString md))
      (assoc :url (.getURL md))
      (assoc :user (.getUserName md))
      (assoc :lcis (.storesLowerCaseIdentifiers md))
      (assoc :ucis (.storesUpperCaseIdentifiers md))
      (assoc :mcis (.storesMixedCaseIdentifiers md)))))

(defmulti table-exist? (fn [a b] (class a)))

(defmethod table-exist? JDBCInfo
  [jdbc ^String table]
  (with-open [ conn (make-connection jdbc) ]
    (table-exist? conn table)))

(defmethod table-exist? Connection

  [^Connection conn ^String table]
  (with-local-vars [ rc false ]
    (CU/Try!
      (let [ mt (.getMetaData conn)
             tbl (cond
                    (.storesUpperCaseIdentifiers mt) (.toUpperCase table)
                    (.storesLowerCaseIdentifiers mt) (.toLowerCase table)
                    :else table) ]
        (with-open [ res (.getColumns mt nil nil tbl nil) ]
          (when (and (not (nil? res)) (.next res))
            (var-set rc true)))))
    @rc))

(defmulti row-exist? (fn [a b] (class a)))

(defmethod row-exist? JDBCInfo
  [jdbc ^String table]
  (with-open [ conn (make-connection jdbc) ]
    (row-exist? conn table)))

(defmethod row-exist? Connection
  [^Connection conn ^String table]
  (with-local-vars [ rc false ]
    (CU/Try!
      (let [ sql (str "SELECT COUNT(*) FROM  " (.toUpperCase table)) ]
        (with-open [ stmt (.createStatement conn) ]
          (with-open [ res (.executeQuery stmt sql) ]
            (when (and (CU/notnil? res) (.next res))
              (var-set rc (> (.getInt res (int 1)) 0)))))) )
    @rc))

(defn- load-columns [^DatabaseMetaData mt ^String catalog ^String schema ^String table]
  (with-local-vars [ pkeys #{} cms {} ]
    (with-open [ rs (.getPrimaryKeys mt catalog schema table) ]
      (loop [ sum (transient #{}) more (.next rs) ]
        (if (not more)
          (var-set pkeys (persistent! sum))
          (recur
            (conj! sum (.toUpperCase (.getString rs (int 4))) )
            (.next rs)))))
    (with-open [ rs (.getColumns mt catalog schema table "%") ]
      (loop [ sum (transient {}) more (.next rs) ]
        (if (not more)
          (var-set cms (persistent! sum))
          (let [ opt (not= (.getInt rs (int 11)) DatabaseMetaData/columnNoNulls)
                 cn (.toUpperCase (.getString rs (int 4)))
                 ctype (.getInt rs (int 5)) ]
            (recur
              (assoc! sum (keyword cn)
                  { :column cn :sql-type ctype :null opt
                    :pkey (clojure.core/contains? @pkeys cn) })
              (.next rs))))))

    (with-meta @cms { :supportsGetGeneratedKeys (.supportsGetGeneratedKeys mt)
                      :supportsTransactions (.supportsTransactions mt) } )))

(defn load-table-meta "" [^Connection conn ^String table]
  (let [ mt (.getMetaData conn) dbv (resolve-vendor conn)
         catalog nil
         schema (if (= (:id dbv) :oracle) "%" nil)
         tbl (cond
                (.storesUpperCaseIdentifiers mt) (.toUpperCase table)
                (.storesLowerCaseIdentifiers mt) (.toLowerCase table)
                :else table) ]
    ;; not good, try mixed case... arrrrrrrrrrhhhhhhhhhhhhhh
    ;;rs = m.getTables( catalog, schema, "%", null)
    (load-columns mt catalog schema tbl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JDBCPoolAPI ""
  (shutdown [_] )
  (nextFree [_] ))

(defn- makePool 

  ^comzotohcljc.dbio.core.JDBCPoolAPI
  [jdbc ^BoneCP impl]
  (reify JDBCPoolAPI

    (shutdown [_] (.shutdown impl))
    (nextFree  [_]
      (try
          (.getConnection impl)
        (catch Throwable e#
          (dbio-error (str "No free connection."))))) ))

(defn make-db-pool ""

  ([jdbc] (make-db-pool jdbc {}))
  ([jdbc options]
    (let [ bcf (BoneCPConfig.)
           ^String dv (:driver jdbc) ]
      (debug "Driver : " dv)
      (debug "URL : "  (:url jdbc))
      (when (SU/hgl? dv) (MU/for-name dv))
      (doto bcf
        (.setPartitionCount (Math/max 1 (CU/nnz (:partitions options))))
        (.setLogStatementsEnabled (CU/nbf (:debug options)))
        (.setPassword (SU/nsb (:pwdObj jdbc)))
        (.setJdbcUrl (:url jdbc))
        (.setUsername (:user jdbc))
        (.setIdleMaxAgeInSeconds (* 60 60 24)) ;; 1 day
        (.setMaxConnectionsPerPartition (Math/max 2 (CU/nnz (:max-conns options))))
        (.setMinConnectionsPerPartition (Math/max 1 (CU/nnz (:min-conns options))))
        (.setPoolName (CU/uid))
        (.setAcquireRetryDelayInMs 5000)
        (.setConnectionTimeoutInMs  (Math/max 5000 (CU/nnz (:max-conn-wait options))))
        (.setDefaultAutoCommit false)
        (.setAcquireRetryAttempts 1))
      (makePool jdbc (BoneCP. bcf))))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- splitLines [^String lines]
  (with-local-vars [ w (.length ^String *DDL_SEP*) rc [] s2 lines ]
    (loop [ sum (transient []) ddl lines pos (.indexOf ddl (SU/nsb *DDL_SEP*)) ]
      (if (< pos 0)
        (do (var-set rc (persistent! sum)) (var-set s2 (SU/strim ddl)))
        (let [ nl (SU/strim (.substring ddl 0 pos))
               d2 (.substring ddl (+ pos w))
               p2 (.indexOf d2 (SU/nsb *DDL_SEP*)) ]
          (recur (conj! sum nl) d2 p2))))
    (if (SU/hgl? @s2)
      (conj @rc @s2)
      @rc)) )

(defn- maybeOK [^String dbn ^Throwable e]
  (let [ oracle (SU/embeds? (SU/nsb dbn) "oracle")
         ee (CU/root-cause e)
         ec (if (instance? SQLException ee) (.getErrorCode ^SQLException ee) nil) ]
    (if (nil? ec)
      (throw e)
      (cond
        (and oracle (= 942 ec)(= 1418 ec)(= 2289 ec)(= 0 ec)) true
        :else (throw e)))))

(defmulti upload-ddl (fn [a b] (class a)))

(defmethod upload-ddl JDBCInfo
  [jdbc ^String ddl]
   (with-open [ conn (make-connection jdbc) ]
     (upload-ddl conn ddl)))

(defmethod upload-ddl Connection
  [^Connection conn ^String ddl]
    (let [ dbn (.toLowerCase (-> (.getMetaData conn)(.getDatabaseProductName)))
           lines (splitLines ddl) ]
      (.setAutoCommit conn true)
      (doseq [ ^String line (seq lines) ]
        (let [ ln (StringUtils/strip (SU/strim line) ";") ]
          (when (and (SU/hgl? ln) (not= (.toLowerCase ln) "go"))
            (try
              (with-open [ stmt (.createStatement conn) ]
                (.executeUpdate stmt ln))
              (catch SQLException e#
                (maybeOK dbn e#))))))))



(def ^:private core-eof nil)

