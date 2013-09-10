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

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.frwk.dbio
  MetaCache Schema SQLr JDBCPool JDBCInfo))
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

(def ^:dynamic *USE_DDL_SEP* true)
(def ^:dynamic *DDL_BVS* nil)
(def ^:dynamic *META-CACHE* nil)
(def DDL_SEP "-- :")


(defn uc-ent ^String [ent] (.toUpperCase (name ent)))
(defn lc-ent ^String [ent] (.toLowerCase (name ent)))

(defn ese "Escape string entity for sql."
  (^String [ent] (uc-ent ent))
  (^String [ch ent] (str ch (uc-ent ent) ch))
  (^String [c1 ent c2] (str c1 (uc-ent ent) c2)))

(defn merge-meta [m1 m2] (merge m1 m2))
(defn eseOID [] (ese "dbio_rowid"))
(defn eseVID [] (ese "dbio_verid"))
(defn eseLHS [] (ese "lhs_rowid"))
(defn eseRHS [] (ese "rhs_rowid"))



(defn make-jdbc "Make a JDBCInfo record."
  ^JDBCInfo
  [^String id cfg ^comzotohcljc.crypto.codec.Password pwdObj]
  ;;(debug "JDBC id= " id ", cfg = " cfg)
  (reify JDBCInfo
    (getId [_] id)
    (getDriver [_] (:d cfg))
    (getUrl [_] (:url cfg))
    (getUser [_] (:user cfg))
    (getPwd [_] (SU/nsb pwdObj)) ))

(def SQLServer :sqlserver)
(def Oracle :oracle)
(def MySQL :mysql)
(def H2 :h2)
(def Postgresql :postgresql)

(def DBTYPES {
    :sqlserver { :test-string "select count(*) from sysusers" }
    :postgresql { :test-string "select 1" }
    :mysql { :test-string "select version()" }
    :h2  { :test-string "select 1" }
    :oracle { :test-string "select 1 from DUAL" }
  })

(defn dbio-error [^String msg] (throw (DBIOError. msg)))
(defn dbio-scopeType [t]
  (keyword (str *ns* "/" t)))

(defn- maybeGetVendor [^String product]
  (let [ lp (.toLowerCase product) ]
    (cond
      (SU/has-nocase? lp "microsoft") :sqlserver
      (SU/has-nocase? lp "postgres") :postgresql
      (SU/has-nocase? lp "oracle") :oracle
      (SU/has-nocase? lp "mysql") :mysql
      (SU/has-nocase? lp "h2") :h2
      :else (dbio-error (str "Unknown db product " product)))))

(defn match-dbtype "" [^String dbtype]
  (let [ kw (keyword (.toLowerCase dbtype)) ]
    (if (nil? (get DBTYPES kw)) nil kw)))

(defn match-jdbc-url "" [^String url]
  (let [ ss (seq (StringUtils/split url \:)) ]
    (if (> (count ss) 1)
      (match-dbtype (nth ss 1))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data modelling
;;
(def JOINED-MODEL-MONIKER :czc.dbio.core/dbio-joined-model)
(def BASEMODEL-MONIKER :czc.dbio.core/dbio-basemodel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dbio-model
  ([^String nm] (dbio-model *ns* nm))
  ([^String nsp ^String nm]
    {
      :id (keyword (str nsp "/" nm))
      :table (.toUpperCase nm)
      :parent nil
      :abstract false
      :system false
      :mxm false
      :indexes {}
      :uniques {}
      :fields {}
      :assocs {} }) )

(defmacro defmodel! [ model-name & body]
  `(def ~model-name
    (-> (dbio-model "czc.dbio.core" ~(name model-name))
                 ~@body)))

(defmacro defmodel "Define a data model." [model-name & body]
  `(def ~model-name
    (-> (dbio-model ~(name model-name))
                 ~@body)))

(defmacro defjoined [model-name lhs rhs]
  `(def ~model-name
      (-> (dbio-model ~(name model-name))
                (with-db-parent-model JOINED-MODEL-MONIKER)
                (with-db-joined-model ~lhs ~rhs))))

(defn with-db-parent-model "" [pojo par]
  (assoc pojo :parent par))

(defn with-db-joined-model "" [pojo lhs rhs]
  (let [ a1 { :kind :MxM :rhs rhs :fkey "" }
         a2 { :kind :MxM :rhs lhs :fkey "" }
         am (:assocs pojo)
         m1 (assoc am "lhs" a1)
         m2 (assoc m1 "rhs" a2) ]
    (-> pojo
      (assoc :assocs m2)
      (assoc :mxm true)) ))

(defn with-db-table-name "" [pojo tablename]
  (assoc pojo :table tablename))

(defn with-db-indexes "" [pojo indices]
  (let [ m (:indexes pojo) ]
    (assoc pojo :indexes (merge m indices))))

(defn with-db-uniques "" [pojo uniqs]
  (let [ m (:uniques pojo) ]
    (assoc pojo :uniques (merge m uniqs))))

(defn with-db-field "" [pojo fid fdef]
  (let [ dft { :column (.toUpperCase (name fid))
               :size 255
               :domain :String
               :assoc-key false
               :pkey false
               :null true
               :auto false
               :dft nil
               :updatable true
               :system false
               :index "" }
         fd (assoc (merge dft fdef) :id (keyword fid))
         fm (:fields pojo)
         mm (assoc fm fid fd) ]
    (assoc pojo :fields mm)))

(defn with-db-fields "" [pojo flddefs]
  (with-local-vars [rcmap pojo]
    (doseq [ [k v] (seq flddefs) ]
      (var-set rcmap (with-db-field @rcmap k v)))
    @rcmap))

(defn with-db-assoc "" [pojo aid adef]
  (let [ dft { :kind nil :rhs nil :fkey "" }
         ad (merge dft adef)
         am (:assocs pojo)
         mm (assoc am aid ad) ]
    (assoc pojo :assocs mm)))

(defn with-db-assocs "" [pojo assocs]
  (with-local-vars [ rcmap pojo ]
    (doseq [ [k v] (seq assocs) ]
      (var-set rcmap (with-db-assoc @rcmap k v)))
    @rcmap))

(defn with-db-abstract "" [pojo] (assoc pojo :abstract true))

(defn- with-db-system "" [pojo] (assoc pojo :system true))

(defn- nested-merge "" [src des]
  (cond
    (and (map? src)(map? des)) (merge src des)
    (and (set? src)(set? des)) (union src des)
    :else des))

;; Defining the base model here.
(defmodel! dbio-basemodel
  (with-db-abstract)
  (with-db-system)
  (with-db-fields {
    :rowid {:column "DBIO_ROWID" :pkey true :domain :Long
            :auto true :system true :updatable false}
    :verid {:column "DBIO_VERSION" :domain :Long :system true
            :dft [ 0 ] }
    :last-modify {:column "DBIO_LASTCHANGED" :domain :Timestamp
               :system true :dft [""] }
    :created-on {:column "DBIO_CREATED_ON" :domain :Timestamp
                  :system true :dft [""] :updatable false}
    :created-by {:column "DBIO_CREATED_BY" :system true :domain :String } }))

(defmodel! dbio-joined-model
  (with-db-abstract)
  (with-db-system)
  (with-db-fields {
    :lhs-typeid {:column "LHS_TYPEID" :null false}
    :lhs-oid {:column "LHS_ROWID" :domain :Long :null false}
    :rhs-typeid {:column "RHS_TYPEID" :null false}
    :rhs-oid {:column "RHS_ROWID" :domain :Long :null false} }) )

(defn make-Schema "" ^Schema [theModels]
  (reify Schema
    (getModels [_] theModels)) )

(defn- resolve-local-assoc "" [ms zm]
  (let [ socs (:assocs zm)
         zid (:id zm) ]
    (if (or (nil? socs) (empty? socs))
      #{}
      (with-local-vars [rc (transient #{}) ]
        (doseq [ [id soc] (seq socs) ]
          (let [ kind (:kind soc)
                 rhs (:rhs soc)
                 ^String col (case kind
                        :O2M
                        (str (CU/stripNSPath rhs) "|" "fk_"
                             (name zid) "_" (name id))
                        :O2O
                        (str (CU/stripNSPath zid) "|" "fk_"
                             (name rhs) "_" (name id))
                        :M2M
                        (if (nil? (get ms (:joined soc)))
                          (dbio-error
                            (str "Missing joined model for m2m assoc " id))
                          "")
                        :MxM
                        ""
                        (dbio-error (str "Invalid assoc type " kind))) ]
            (when (SU/hgl? col)
              (var-set rc (conj! @rc col)))))
        (persistent! @rc) ))))

(defn- resolve-assoc "" [ms m]
  (let [ par (:parent m) ]
    (if (nil? par)
      (union #{} (resolve-local-assoc ms m))
      (union #{} (resolve-local-assoc ms m) (resolve-assoc ms (get ms par))))))

(defn- resolve-assocs "" [ms]
  (with-local-vars [ rc #{} ]
    (doseq [ en (seq ms) ]
      (var-set rc (union @rc (resolve-assoc ms (last en)) )))
    @rc))

(defn- inject-fkeys-models "" [ms fks]
  (with-local-vars [ rc (merge {} ms) ]
    (doseq [ ^String k (seq fks) ]
      (let [ ss (.split k "\\|")
             id (keyword (nth ss 0))
             fid (keyword (nth ss 1))
             pojo (get ms id) ]
        (var-set rc
                 (assoc @rc id
                        (with-db-field pojo fid
                                       { :domain :Long :assoc-key true } )))))
    @rc))

(defn- resolve-parent "" [ms m]
  (let [ par (:parent m) ]
    (cond
      (keyword? par) (if (nil? (get ms par))
                       (dbio-error (str "Unknown model " par))
                       m)
      (nil? par)
      (assoc m :parent BASEMODEL-MONIKER)

      :else (dbio-error (str "Invalid parent " par)))))

(defn- resolve-parents "" [ms]
  (persistent! (reduce (fn [sum en]
                          (let [ rc (resolve-parent ms (last en)) ]
                            (assoc! sum (:id rc) rc)))
                       (transient {})
                       (seq ms))) )

(defn- mapize-models "" [ms]
  (persistent! (reduce (fn [sum n]
                         (assoc! sum (:id n) n))
                       (transient {})
                       (seq ms))) )

(defn- collect-db-xxx-filter "" [a b]
  (cond
    (keyword? b) :keyword
    (map? b) :map
    :else (dbio-error (str "Invalid arg " b))))

(defmulti collect-db-fields collect-db-xxx-filter)

(defmethod collect-db-fields :keyword [cache modelid]
  (let [ mm (get cache modelid) ]
    (when (nil? mm) (warn "unknown model id " modelid))
    (collect-db-fields cache mm)))

(defmethod collect-db-fields :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:fields zm))
      (merge {} (:fields zm) (collect-db-fields cache par)))))

(defmulti collect-db-indexes collect-db-xxx-filter)

(defmethod collect-db-indexes :keyword [cache modelid]
  (let [ mm (get cache modelid) ]
    (when (nil? mm) (warn "unknown model id " modelid))
    (collect-db-indexes cache mm)))

(defmethod collect-db-indexes :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:indexes zm))
      (merge {} (:indexes zm) (collect-db-indexes cache par)))))

(defmulti collect-db-uniques collect-db-xxx-filter)

(defmethod collect-db-uniques :keyword [cache modelid]
  (let [ mm (get cache modelid) ]
    (when (nil? mm) (warn "unknown model id " modelid))
    (collect-db-uniques cache mm)))

(defmethod collect-db-uniques :map [cache zm]
  (let [ par (:parent zm) ]
    (if (nil? par)
      (merge {} (:uniques zm))
      (merge {} (:uniques zm) (collect-db-uniques cache par)))))

(defn- colmap-fields "" [flds]
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

  ^MetaCache [^Schema schema]

  (let [ ms (if (nil? schema) {} (mapize-models (.getModels schema)))
         m0 (assoc ms JOINED-MODEL-MONIKER dbio-joined-model)
         m1 (if (empty? m0) {} (resolve-parents m0))
         m2 (assoc m1 BASEMODEL-MONIKER dbio-basemodel)
         m3 (inject-fkeys-models m2 (resolve-assocs m2))
         m4 (meta-models m3) ]
    (reify MetaCache
      (getMetas [_] m4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- safeGetConn

  ^Connection
  [^JDBCInfo jdbc]

  (let [ user (.getUser jdbc)
         url (.getUrl jdbc)
         dv (.getDriver jdbc)
         d (if (SU/hgl? url) (DriverManager/getDriver url))
         p (if (SU/hgl? user)
               (doto (Properties.) (.put "password" (SU/nsb (.getPwd jdbc)))
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
  [^JDBCInfo jdbc]

  (let [ url (.getUrl jdbc)
         ^Connection conn (if (SU/hgl? (.getUser jdbc))
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

(defn- makePool ^JDBCPool [jdbc ^BoneCP impl]
  (let [ dbv (resolve-vendor jdbc) ]
    (reify

      Object

      (finalize [this]
        (CU/Try! (.shutdown this)))

      JDBCPool

      (shutdown [_] (.shutdown impl))
      (vendor [_] dbv)
      (nextFree  [_]
        (try
            (.getConnection impl)
          (catch Throwable e#
            (dbio-error (str "No free connection."))))) )))

(defn make-db-pool ""

  ([^JDBCInfo jdbc] (make-db-pool jdbc {}))
  ([^JDBCInfo jdbc options]
    (let [ bcf (BoneCPConfig.)
           ^String dv (.getDriver jdbc) ]
      ;;(debug "Driver : " dv)
      ;;(debug "URL : "  (.getUrl jdbc))
      (when (SU/hgl? dv) (MU/for-name dv))
      (doto bcf
        (.setPartitionCount (Math/max 1 (CU/nnz (:partitions options))))
        (.setLogStatementsEnabled (CU/nbf (:debug options)))
        (.setPassword (SU/nsb (.getPwd jdbc)))
        (.setJdbcUrl (.getUrl jdbc))
        (.setUsername (.getUser jdbc))
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
  (with-local-vars [ w (.length ^String DDL_SEP)
                     rc []
                     s2 lines ]
    (loop [ sum (transient [])
            ddl lines
            pos (.indexOf ddl ^String DDL_SEP) ]
      (if (< pos 0)
        (do (var-set rc (persistent! sum)) (var-set s2 (SU/strim ddl)))
        (let [ nl (SU/strim (.substring ddl 0 pos))
               d2 (.substring ddl (+ pos @w))
               p2 (.indexOf d2 ^String DDL_SEP) ]
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
    (debug "\n" ddl)
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
                (maybeOK dbn e#))))
          ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dbio-create-obj [model]
  (with-meta
    {}
    { :typeid model } ))

(defn dbio-set-fld "" [pojo fld value]
  (assoc pojo (keyword fld) value))

(defn dbio-clr-fld "" [pojo fld]
  (dissoc pojo (keyword fld)))

(defn dbio-get-fld "" [pojo fld]
  (get pojo (keyword fld)))

(defn- maybeGetAssoc [mc zm id]
  (if (nil? zm)
    nil
    (let [ m (:assocs zm)
           rc (get m id) ]
      (if (nil? rc)
        (maybeGetAssoc mc (get mc (:parent zm)) id)
        rc))))

(defn- fmtfkey [p1 p2]
  (keyword (str "fk_" (name p1) "_" (name p2))) )

(defn- clrM2M [mc sql jt obj]
  (let [ pt (CU/stripNSPath (:typeid (meta obj)))
         pkey (:rowid (meta obj))
         zm (get mc jt) ]
    (when (nil? zm)
      (throw (DBIOError. (str "Unknown model type " jt))))
    (.execute sql
              (str "delete from " (ese (:table zm))
                   " where "
                   (ese "lhs_rowid") " =? and "
                   (ese "lhs_typeid") " =?") [pkey pt] )
    (.execute sql
              (str "delete from " (ese (:table zm))
                   " where "
                   (ese "rhs_rowid") " =? and "
                   (ese "rhs_typeid") " =?") [pkey pt] )
    ))

(defn- setM2M [mc sql jt lhs rhs]
  (let [ zm (get mc jt)
         lm (meta lhs)
         rm (meta rhs) ]
    (when (nil? zm)
      (throw (DBIOError. (str "Unknown model type " jt))))
    (.insert sql
      (-> (dbio-create-obj jt)
          (dbio-set-fld :lhs-typeid (CU/stripNSPath (:typeid lm)))
          (dbio-set-fld :lhs-oid (:rowid lm))
          (dbio-set-fld :rhs-typeid (CU/stripNSPath (:typeid rm)))
          (dbio-set-fld :rhs-oid (:rowid rm)) )) ))

(defn- getM2M [mc sql jt obj]
  (let [ pt (CU/stripNSPath (:typeid (meta obj)))
         pkey (:rowid (meta obj))
         zm (get mc jt) ]
    (when (nil? zm)
      (throw (DBIOError. (str "Unknown model type " jt))))
    (.select sql
             (str "select * from "
                  (ese (:table zm)) " where "
                  (ese "lhs_rowid") " =? and "
                  (ese "lhs_typeid") " =?" ) [pkey pt])

    ))


(defn- clrO2M [mc sql rt col]
  (let [ zm (get mc rt) ]
    (when (nil? zm)
      (throw (DBIOError. (str "Unknown model type " rt))))
    (let [ tbl (ese (:table zm))
           rset (.select sql (str "select "
                                  (eseOID) "," (eseVID)
                                  " from " tbl
                                  " where " (ese col) " =?") [pkey]) ]
      (doseq [ r (seq rset) ]
        (let [ oid (:DBIO_ROWID r)
               vid (:DBIO_VERID r)
               nid (inc vid) ]
          (.execute sql
                    (str "update " tbl " set "
                         (ese col) " = NULL,"
                         (eseVID) " =?"
                         " where "
                         (eseOID) " =? and " (eseVID) " =?")
                    [nid oid vid] )))
      )))

(defn- clrO2O [mc sql lhs col]
  (let [ ma (meta lhs)
         oid (:rowid ma)
         vid (:verid ma)
         nid (inc vid)
         mt (:typeid ma)
         zm (get mc mt) ]
    (when (nil? zm)
      (throw (DBIOError. (str "Unknown model type " mt))))
    (.execute sql
              (str "update " (ese (:table zm))
                   " set " (ese col) " = NULL, "
                   (eseVID) " =?"
                   " where "
                   (eseOID) " =? and " (eseVID) " =?")
              [nid oid vid] )
    (vary-meta lhs merge-meta { :verid nid })))

;; { :as :associd :with sqlr :cast :x.y/z }
(defn dbio-clr-assoc [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         model (:typeid (meta lhs))
         zm (get mc model)
         ^SQLr sql (:with ctx)
         c (:cast ctx)
         a (:as ctx)
         rc (maybeGetAssoc mc zm a) ]
    (when (nil? rc)
      (throw (DBIOError. (str "Unknown assoc " a))))
    (case (:kind rc)
      :O2M (do (clrO2M mc sql c (fmtfkey (:id zm) a)) lhs)
      :M2M (do (clrM2M mc sql (:joined rc) lhs) lhs)
      :O20 (clrO2O mc sql lhs (fmtfkey (:rhs rc) a))
      (throw (DBIOError. (str "Bad assoc " a ", invalid type.")))) ))


(defn dbio-set-assoc [ctx lhs rhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         model (:typeid (meta lhs))
         zm (get mc model)
         ^SQLr sql (:with ctx)
         c (:cast ctx)
         a (:as ctx)
         rc (maybeGetAssoc mc zm a) ]
    (when (nil? rc)
      (throw (DBIOError. (str "Unknown assoc " a))))
    (case (:kind rc)
      :O20
      [ (.update sql
                 (dbio-set-fld lhs
                               (fmtfkey (:rhs rc) a)
                               (:rowid (meta rhs)))) rhs]
      :02M
      [ lhs (.update sql
                     (dbio-set-fld rhs
                                   (fmtfkey (:id zm) a)
                                   (:rowid (meta lhs)))) ]
      :M2M
      (-> (dbio-create-obj (:joined rc))
        (dbio-set-fld :lhs-oid (:rowid lhs))
        (dbio-set-fld :rhs-oid (:rowid rhs)))
      (throw (DBIOError. (str "Bad assoc " a ", invalid type.")))) ))

(defn dbio-get-assoc [ctx lhs]
  (let [ mc (.getMetas ^MetaCache *META-CACHE*)
         model (:typeid (meta lhs))
         zm (get mc model)
         ^SQLr sql (:with ctx)
         c (:cast ctx)
         a (:as ctx)
         rc (maybeGetAssoc mc zm a) ]
    (when (nil? rc)
      (throw (DBIOError. (str "Unknown assoc " a))))
    (case (:kind rc)
      :O20
      (.findOne sql
                (if (nil? c) (:rhs rc) c)
                { (:column (:rowid (:fields (meta zm))))
                  (get lhs (fmtfkey (:rhs rc) a)) } )
      :02M
      (.findSome sql
                 (if (nil? c) (:rhs rc) c)
                 { (fmtfkey (:id zm) a) (:rowid (meta lhs)) })
      :M2M
      []
      (throw (DBIOError. (str "Bad assoc " a ", invalid type.")))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private core-eof nil)

