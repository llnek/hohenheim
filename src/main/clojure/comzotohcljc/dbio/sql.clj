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

  comzotohcljc.dbio.sql)

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.dbio.core :as DU])

(import '(java.util Calendar GregorianCalendar TimeZone))
(import '(com.zotoh.frwk.dbio DBIOError OptLockError))
(import '(java.math BigDecimal BigInteger))
(import '(java.io Reader InputStream))

(import '(java.sql
  ResultSet Types SQLException
  DatabaseMetaData ResultSetMetaData Date Timestamp Blob Clob
  Statement PreparedStatement Connection))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn- uc-ent ^String [ent] (.toUpperCase (name ent)))
(defn- lc-ent ^String [ent] (.toLowerCase (name ent)))

(defn ese "Escape string entity for sql."
  (^String [ent] (uc-ent ent))
  (^String [ch ent] (str ch (uc-ent ent) ch))
  (^String [c1 ent c2] (str c1 (uc-ent ent) c2)))

(defn table-name
  (^String [mdef] (:table mdef))
  (^String [mid cache] (table-name (get cache mid))))

(defn col-name
  (^String [fdef] (:column fdef))
  (^String [fid zm] (col-name (get zm fid))))

(defn- merge-meta [m1 m2] (merge m1 m2))

(defn- fmtUpdateWhere ^String [lock zm]
  (str (ese (col-name :rowid zm)) "=?"
       (if lock
          (str " AND " (ese (col-name :verid zm)) "=?")
          "")))

(defn- lockError [^String opcode ^long cnt ^String table ^long rowID]
  (when (= cnt 0)
    (throw (OptLockError. opcode table rowID))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sql-filter-clause "" [filters]
  (let [ wc (reduce (fn [^StringBuilder sum en]
                      (SU/add-delim! sum " AND "
                        (str (ese (first en))
                             (if (nil? (last en)) " IS NULL " " = ? "))))
                    (StringBuilder.)
                    (seq filters)) ]
    [ (SU/nsb wc) (CU/flatten-nil (vals filters)) ] ))

(defn- readCol ^Object [sqlType ^long pos ^ResultSet rset]
  (let [ obj (.getObject rset (int pos))
         ^InputStream inp (cond
                  (instance? Blob obj) (.getBinaryStream ^Blob obj)
                  (instance? InputStream obj) obj
                  :else nil)
         ^Reader rdr (cond
                  (instance? Clob obj) (.getCharacterStream ^Clob obj)
                  (instance? Reader obj) obj
                  :else nil) ]
    (cond
      (CU/notnil? rdr) (with-open [r rdr] (IO/read-chars r))
      (CU/notnil? inp) (with-open [p inp] (IO/read-bytes p))
      :else obj)))

(defn- readOneCol [^long sqlType ^long pos ^ResultSet rset]
  (case sqlType
      Types/TIMESTAMP (.getTimestamp rset (int pos) ^Calendar DU/*GMT-CAL*)
      Types/DATE (.getDate rset (int pos) ^Calendar DU/*GMT-CAL*)
      (readCol sqlType pos rset)) )

;; row is a transient object.
(defn- model-injtor [cache zm row cn ct cv]
  (let [ cols (:columns (meta zm))
         fdef (get cols cn) ]
    (if (nil? fdef)
      row
      (assoc! row (:id fdef) cv))))

;; generic resultset, no model defined.
;; row is a transient object.
(defn- std-injtor [row ^String cn ct cv]
  (assoc! row (keyword (.toUpperCase cn)) cv))

(defn- row2obj [finj ^ResultSet rs ^ResultSetMetaData rsmeta]
  (let [ cc (.getColumnCount rsmeta)
         rr (range 1 (inc cc)) ]
    (with-local-vars [ row (transient {}) ]
      (doseq [ ^long pos (seq rr) ]
        (let [ cn (.getColumnName rsmeta (int pos))
               ct (.getColumnType rsmeta (int pos))
               cv (readOneCol ct pos rs) ]
          (var-set row (finj @row cn ct cv))))
      (persistent! @row) )) )

(defn- insert? [^String sql]
  (.startsWith (.toLowerCase (SU/strim sql)) "insert"))

(defn- setBindVar [^PreparedStatement ps ^long pos p]
  (cond
    (instance? String p) (.setString ps pos p)
    (instance? Long p) (.setLong ps pos p)
    (instance? Integer p) (.setInt ps pos p)
    (instance? Short p) (.setShort ps pos p)

    (instance? BigDecimal p) (.setBigDecimal ps pos p)
    (instance? BigInteger p) (.setBigDecimal ps pos (BigDecimal. ^BigInteger p))

    (instance? InputStream p) (.setBinaryStream ps pos p)
    (instance? Reader p) (.setCharacterStream ps pos p)
    (instance? Blob p) (.setBlob ps pos ^Blob p)
    (instance? Clob p) (.setClob ps pos ^Clob p)

    (instance? (MU/chars-class) p) (.setString ps pos (String. ^chars p))
    (instance? (MU/bytes-class) p) (.setBytes ps pos ^bytes p)

    (instance? Boolean p) (.setInt ps pos (if p 1 0))
    (instance? Double p) (.setDouble ps pos p)
    (instance? Float p) (.setFloat ps pos p)

    (instance? Timestamp p) (.setTimestamp ps pos p ^Calendar DU/*GMT-CAL*)
    (instance? Date p) (.setDate ps pos p ^Calendar DU/*GMT-CAL*)
    (instance? Calendar p) (.setTimestamp ps pos 
                                          (Timestamp. (.getTimeInMillis ^Calendar p))
                                          ^Calendar DU/*GMT-CAL*)

    :else (DU/dbio-error (str "Unsupported param type: " (type p)))) )

(defn- mssql-tweak-sqlstr [^String sqlstr ^String token ^String cmd]
  (loop [ stop false sql sqlstr ]
    (if stop
      sql
      (let [ lcs (.toLowerCase sql) pos (.indexOf lcs (name token))
             rc (if (< pos 0)
                  []
                  [(.substring sql 0 pos) (.substring sql pos)]) ]
        (if (empty? rc)
          (recur true sql)
          (recur false (str (first rc) " WITH (" cmd ") " (last rc)) ))))))

(defn- jiggleSQL [^comzotohcljc.dbio.core.DBAPI db ^String sqlstr]
  (let [ sql (SU/strim sqlstr)
         lcs (.toLowerCase sql)
         v (.vendor db)   ]
    (if (= :sqlserver (:id v))
      (cond
        (.startsWith lcs "select") (mssql-tweak-sqlstr sql :where "NOLOCK")
        (.startsWith lcs "delete") (mssql-tweak-sqlstr sql :where "ROWLOCK")
        (.startsWith lcs "update") (mssql-tweak-sqlstr sql :set "ROWLOCK")
        :else sql)
      sql)))

(defn- build-stmt 
  
  ^PreparedStatement
  [db ^Connection conn ^String sqlstr params]
  (let [ sql sqlstr ;; (jiggleSQL db sqlstr)
         ps (if (insert? sql)
              (.prepareStatement conn sql Statement/RETURN_GENERATED_KEYS)
              (.prepareStatement conn sql)) ]
    (debug "SQL: {}" sql)
    (doseq [n (seq (range 0 (count params))) ]
      (setBindVar ps (inc n) (nth params n)))))

(defn- handleGKeys [^ResultSet rs ^long cnt options]
  (let [ rc (cond
              (= cnt 1) (.getObject rs 1)
              :else (.getLong rs (SU/nsb (:pkey options)))) ]
    { :1 rc }))

(defprotocol ^:private SQueryAPI
  (sql-select [_ sql pms row-provider-func] [_ sql pms] )
  (sql-executeWithOutput [_  sql pms options] )
  (sql-execute [_  sql pms] ) )

(defn- make-sql

  ^comzotohcljc.dbio.sql.SQueryAPI
  [db ^comzotohcljc.dbio.core.MetaCacheAPI metaCache ^Connection conn]

  (reify SQueryAPI

    (sql-executeWithOutput [this sql pms options]
      (with-open [ stmt (build-stmt db conn sql pms) ]
        (if (> (.executeUpdate stmt) 0)
          (with-open [ rs (.getGeneratedKeys stmt) ]
            (let [ cnt (if (nil? rs)
                          0
                          (-> (.getMetaData rs) (.getColumnCount))) ]
              (if (and (> cnt 0) (.next rs))
                (handleGKeys rs cnt options)
                {}
                ))))))

    (sql-select [this sql pms ]
      (sql-select this sql pms (partial row2obj std-injtor)))

    (sql-select [this sql pms func]
      (with-open [ stmt (build-stmt db conn sql pms) ]
        (with-open [ rs (.executeQuery stmt) ]
          (let [ rsmeta (.getMetaData rs) ]
            (loop [ sum (transient []) ok (.next rs) ]
              (if (not ok)
                (persistent! sum)
                (recur (conj! sum (func rs rsmeta)) (.next rs))))))))

    (sql-execute [this sql pms]
      (with-open [ stmt (build-stmt db conn sql pms) ]
        (.executeUpdate stmt)))  ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol SQLProcAPI ""
  (doQuery [_ conn sql params model] [_ conn sql params] )
  (doExecuteWithOutput [_ conn sql params options] )
  (doExecute [_ conn sql params] )
  (doCount [_  conn model] )
  (doPurge [_  conn model] )
  (doDelete [_  conn pojo] )
  (doInsert [_  conn pojo] )
  (doUpdate [_  conn pojo] ) )

(defn- insert-fields [flds obj s1 s2]
  (with-local-vars [ ps (transient []) ]
    (doseq [ [k v] (seq obj) ]
      (let [ fdef (get flds k)
             ^String cn (:column fdef) ]
        (when (and (CU/notnil? fdef)
                 (not (:auto fdef))
                 (not (:system fdef)))
          (SU/add-delim! s1 "," (ese cn))
          (SU/add-delim! s2 "," (if (nil? v) "NULL" "?"))
          (when-not (nil? v)
            (var-set ps (conj! @ps v))))))

    (persistent! @ps) ))

(defn- update-fields [flds obj ^StringBuilder sb1]
  (with-local-vars [ ps (transient []) ]
    (doseq [ [k v] (seq obj) ]
      (let [ fdef (get flds k)
             cn (col-name fdef) ]
        (when (and (CU/notnil? fdef)
                   (:updatable fdef)
                   (not (:auto fdef)) (not (:system fdef)) )
          (doto sb1
            (SU/add-delim! "," (ese cn))
            (.append (if (nil? v) "=NULL" "=?")))
          (when-not (nil? v)
            (var-set ps (conj! @ps v))))))
    (persistent! @ps)) )

(defn make-proc ""

  ^comzotohcljc.dbio.sql.SQLProcAPI

  [^comzotohcljc.dbio.core.DBAPI db 
   ^comzotohcljc.dbio.core.MetaCacheAPI metaCache]

  (reify SQLProcAPI

    (doQuery [_ conn sql pms model]
      (let [ zm (get metaCache model) ]
        (when (nil? zm)
          (DU/dbio-error (str "Unknown model " model)))
        (let [ px (partial model-injtor metaCache zm) 
               pf (partial row2obj px) ]
          (-> (make-sql db metaCache conn) (.sql-select sql pms pf )))))

    (doQuery [_ conn sql pms]
      (let [ pf (partial row2obj std-injtor) ]
        (-> (make-sql db metaCache conn) (.sql-select sql pms pf ))) )

    (doCount [this conn model]
      (let [ rc (doQuery this conn
                  (str "SELECT COUNT(*) FROM "
                       (ese (table-name model metaCache))) [] ) ]
        (if (empty? rc)
          0
          (val (first rc)))))

    (doPurge [_ conn model]
      (let [ sql (str "DELETE FROM " (ese (table-name model metaCache))) ]
        (do (-> (make-sql db metaCache conn) (.sql-execute sql [])) nil)))

    (doDelete [this conn obj]
      (let [ info (meta obj) model (:typeid info)
             zm (get metaCache model) ]
        (when (nil? zm) (DU/dbio-error (str "Unknown model " model)))
        (let [ lock (.supportsOptimisticLock db)
               table (table-name zm)
               rowid (:rowid info)
               verid (:verid info)
               p (if lock [rowid verid] [rowid] )
               w (fmtUpdateWhere lock zm)
               cnt (doExecute this conn
                     (str "DELETE FROM " (ese table) " WHERE " w)
                     p) ]
          (when lock (lockError "delete" cnt table rowid))
          cnt)))

    (doInsert [this conn obj]
      (let [ info (meta obj) model (:typeid info)
             zm (get metaCache model) ]
        (when (nil? zm) (DU/dbio-error (str "Unknown model " model)))
        (let [ lock (.supportsOptimisticLock db)
               flds (:fields (meta zm))
               table (table-name zm)
               now (CU/now-jtstamp)
               s2 (StringBuilder.)
               s1 (StringBuilder.)
               pms (insert-fields flds obj s1 s2) ]
          (if (= (.length s1) 0)
            nil
            (let [ out (doExecuteWithOutput this conn
                          (str "INSERT INTO " (ese table) "(" s1 ") VALUES (" s2 ")" ) 
                          pms { :pkey (col-name :rowid zm) } ) ]
              (when (empty? out)
                (DU/dbio-error (str "Insert requires row-id to be returned.")))
              (let [ wm { :rowid (:pkey out) :verid 0 } ]
                (when-not (number? (:rowid wm))
                  (DU/dbio-error (str "RowID data-type must be Long.")))
                (vary-meta obj merge-meta wm))))
        )))

    (doUpdate [this conn obj]
      (let [ info (meta obj) model (:typeid info)
             zm (get metaCache model) ]
        (when (nil? zm) (DU/dbio-error (str "Unknown model " model)))
        (let [ lock (.supportsOptimisticLock db)
               cver (CU/nnz (:verid info))
               flds (:fields (meta zm))
               table (table-name zm)
               rowid (:rowid info)
               sb1 (StringBuilder.)
               now (CU/now-jtstamp)
               nver (inc cver)
               pms (update-fields flds obj sb1) ]
          (if (= (.length sb1) 0)
            nil
            (with-local-vars [ ps (transient pms) ]
              (-> (SU/add-delim! sb1 "," (ese (col-name :last-modify zm)))
                  (.append "=?"))
              (var-set  ps (conj! @ps now))
              (when lock ;; up the version
                (-> (SU/add-delim! sb1 "," (ese (col-name :verid zm)))
                    (.append "=?"))
                (var-set ps (conj! @ps nver)))
              ;; for the where clause
              (var-set ps (conj! @ps rowid))
              (when lock (var-set  ps (conj! @ps cver)))
              (let [ cnt (doExecute this conn
                            (str "UPDATE " (ese table) " SET " sb1 " WHERE "
                                (fmtUpdateWhere lock zm))
                                    (persistent! @ps)) ]
                (when lock (lockError "update" cnt table rowid))
                (vary-meta obj merge-meta 
                           { :verid nver :last-modify now }))))
      )))

      (doExecuteWithOutput [this conn sql pms options]
        (-> (make-sql db metaCache conn)
            (.sql-executeWithOutput sql pms options)))

      (doExecute [this conn sql pms]
        (-> (make-sql db metaCache conn) (.sql-execute sql pms)))
  ))

(defprotocol Transactable
  (execWith [_ func] )
  (^Connection begin [_] )
  (commit [_ conn] )
  (rollback [_ conn] ))

(defprotocol SQLr
  (findSome [_  model filters ordering] [_   model filters]  )
  (findAll [_ model ordering] [_ model] )
  (findOne [_  model filters] )
  (update [_  obj] )
  (delete [_  obj] )
  (insert [_  obj] )
  (select [_  sql params] )
  (executeWithOutput [_  sql params] )
  (execute [_  sql params] )
  (count* [_  model] )
  (purge [_  model] ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private sql-eof nil)

