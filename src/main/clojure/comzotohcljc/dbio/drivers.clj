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

  comzotohcljc.dbio.drivers)

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.dbio.core])

(import '(com.zotoh.frwk.dbio DBIOError))
(import '(java.util HashMap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defprotocol DBDriver ""
  (getTestString [_] )
  (getId [_] ))

(defn- getcolname ^String [flds fid]
  (let [ ^String c (:column (get flds fid)) ]
    (if (SU/hgl? c) (.toUpperCase c) c)))

(defn- getNotNull ^String [db] "NOT NULL")

(defn- getNull ^String [db] "NULL")

(defn getPad ^String [db] "    ")

(defn- nullClause [db opt?]
  (if opt? (getNull db) (getNotNull db)))

(defn- genSep ^String [db]
  (if *USE_DDL_SEP* *DDL_SEP* ""))

(defn genCol ^String [fld]
  (.toUpperCase ^String (:column fld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti genBegin (fn [a & more] (class a)))
(defmulti genExec (fn [a & more] (class a)))
(defmulti genDrop (fn [a & more] (class a)))

(defmulti genEndSQL (fn [a & more] (class a)))
(defmulti genGrant (fn [a & more] (class a)))
(defmulti genEnd (fn [a & more] (class a)))

(defmulti genAutoInteger (fn [a & more] (class a)))
(defmulti genDouble (fn [a & more] (class a)))
(defmulti genLong (fn [a & more] (class a)))
(defmulti genFloat (fn [a & more] (class a)))
(defmulti genAutoLong (fn [a & more] (class a)))
(defmulti getTSDefault (fn [a & more] (class a)))
(defmulti genTimestamp (fn [a & more] (class a)))
(defmulti genDate (fn [a & more] (class a)))
(defmulti genCal (fn [a & more] (class a)))
(defmulti genBool (fn [a & more] (class a)))
(defmulti genInteger (fn [a & more] (class a)))

(defmulti getFloatKeyword (fn [a & more] (class a)))
(defmulti getIntKeyword (fn [a & more] (class a)))
(defmulti getTSKeyword (fn [a & more] (class a)))
(defmulti getDateKeyword (fn [a & more] (class a)))
(defmulti getBoolKeyword (fn [a & more] (class a)))
(defmulti getLongKeyword (fn [a & more] (class a)))
(defmulti getDoubleKeyword (fn [a & more] (class a)))
(defmulti getStringKeyword (fn [a & more] (class a)))
(defmulti getBlobKeyword (fn [a & more] (class a)))
(defmulti genBytes (fn [a & more] (class a)))
(defmulti genString (fn [a & more] (class a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod genExec :default ^String [db] (str ";\n" (genSep db)))

(defmethod genDrop :default ^String [db table]
  (str "DROP TABLE " table (genExec db) "\n\n"))

(defmethod genBegin :default ^String [db table]
  (str "CREATE TABLE " table "\n(\n"))

(defmethod genEnd :default ^String [db table] (str "\n)" (genExec db) "\n\n"))

(defmethod genGrant :default ^String [db table] "")

(defmethod genEndSQL :default ^String [db] "")

(defn genColDef ^String [db ^String col ty opt? dft]
  (str (getPad db) (.toUpperCase col) " " ty " " (nullClause db opt?)
       (if (nil? dft) "" (str " DEFAULT " dft))))


(defmethod getFloatKeyword :default [db] "FLOAT")
(defmethod getIntKeyword :default [db] "INTEGER")
(defmethod getTSKeyword :default [db] "TIMESTAMP")
(defmethod getDateKeyword :default [db] "DATE")
(defmethod getBoolKeyword :default [db] "INTEGER")
(defmethod getLongKeyword :default [db] "BIGINT")
(defmethod getDoubleKeyword :default [db] "DOUBLE PRECISION")
(defmethod getStringKeyword :default [db] "VARCHAR")
(defmethod getBlobKeyword :default [db] "BLOB")

(defmethod genBytes :default [db fld]
  (genColDef db (:column fld) (getBlobKeyword db) (:null fld) nil))

(defmethod genString :default [db fld]
  (genColDef  db (:column fld)
    (str (getStringKeyword db) "(" (:size fld) ")")
    (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmethod genInteger :default [db fld]
  (genColDef db (:column fld) (getIntKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmethod genAutoInteger :default [db table fld] "")

(defmethod genDouble :default [db fld]
  (genColDef db (:column fld) (getDoubleKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))


(defmethod genFloat :default [db fld]
  (genColDef db (:column fld) (getFloatKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmethod genLong :default [db fld]
  (genColDef db (:column fld) (getLongKeyword db) (:null fld)
    (if (:default fld) (:default-value fld) nil)))

(defmethod genAutoLong :default [db table fld] "")

(defmethod getTSDefault :default [db] "CURRENT_TIMESTAMP")

(defmethod genTimestamp :default [db fld]
  (genColDef db (:column fld) (getTSKeyword db) (:null fld)
    (if (:default fld) (getTSDefault db) nil)))

(defmethod genDate :default [db fld]
  (genColDef db (:column fld) (getDateKeyword db) (:null fld)
    (if (:default fld) (getTSDefault db) nil)))

(defmethod genCal :default [db fld] (genTimestamp db fld))

(defmethod genBool :default [db fld]
  (genColDef db (:column fld) (getBoolKeyword db) (:null fld)
      (if (:default fld) (:default-value fld) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- genExIndexes ^String [db cache table flds zm]
  (let [ m (collect-db-indexes cache zm)
         bf (StringBuilder.) ]
    (doseq [ [nm nv] (seq m) ]
      (let [ cols (map #(getcolname flds %) nv) ]
        (when (empty? cols) (dbio-error (str "Cannot have empty index: " nm)))
        (.append bf (str "CREATE INDEX "
                         (.toLowerCase (str table "_" (name nm)))
                         " ON " table
                    " ( " (clojure.string/join "," cols) " )" (genExec db) "\n\n" ))))
    (.toString bf)))

(defn- genUniques [db cache flds zm]
  (let [ m (collect-db-uniques cache zm)
         bf (StringBuilder.) ]
    (doseq [ [nm nv] (seq m) ]
      (let [ cols (map #(getcolname flds %) nv) ]
        (when (empty? cols) (dbio-error (str "Cannot have empty unique: " (name nm))))
        (SU/add-delim! bf ",\n"
            (str (getPad db) "UNIQUE(" (clojure.string/join "," cols) ")"))))
    (.toString bf)))

(defn- genPrimaryKey [db zm pks]
    (str (getPad db) "PRIMARY KEY("
         (.toUpperCase (SU/nsb (clojure.string/join "," pks)) )
         ")"))

(defn- genBody [db cache table zm]
  (let [ flds (collect-db-fields cache zm)
         inx (StringBuilder.)
         bf (StringBuilder.) ]
    (with-local-vars [ pkeys (transient #{}) ]
      ;; 1st do the columns
      (doseq [ [fid fld] (seq flds) ]
        (let [ cn (.toUpperCase ^String (:column fld))
               dt (:domain fld)
               col (case dt
                    :boolean (genBool db fld)
                    :timestamp (genTimestamp db fld)
                    :date (genDate db fld)
                    :calendar (genCal db fld)
                    :int (if (:auto fld)
                           (genAutoInteger db table fld)
                           (genInteger db fld))
                    :long (if (:auto fld)
                            (genAutoLong db table fld)
                            (genLong db fld))
                    :double (genDouble db fld)
                    :float (genFloat db fld)
                    :string (genString db fld)
                    :bytes (genBytes db fld)
                    (dbio-error (str "Unsupported domain type " dt))) ]
          (when (:pkey fld) (var-set pkeys (conj! @pkeys cn)))
          (SU/add-delim! bf ",\n" col)))
      ;; now do the assocs
      ;; now explicit indexes
      (-> inx (.append (genExIndexes db cache table flds zm)))
      ;; now uniques, primary keys and done.
      (when (> (.length bf) 0)
        (when (> (count @pkeys) 0)
          (.append bf (str ",\n" (genPrimaryKey db zm (persistent! @pkeys)))))
        (let [ s (genUniques db cache flds zm) ]
          (when (SU/hgl? s)
            (.append bf (str ",\n" s)))))

    [ (.toString bf) (.toString inx) ] )) )

(defn- genOneTable [db ms zm]
  (let [ table (.toUpperCase ^String (:table zm))
           b (genBegin db table)
           d (genBody db ms table zm)
           e (genEnd db table)
           s1 (str b (first d) e)
           inx (last d) ]
      (str s1 (if (SU/hgl? inx) inx "") (genGrant db table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn getDDL  "" 

  ^String 
  [^comzotohcljc.dbio.core.DBAPI db
   ^comzotohcljc.dbio.core.MetaCacheAPI metaCache]

  (binding [ *DDL_BVS* (HashMap.) ]
    (let [ ms (.getMetas metaCache)
           drops (StringBuilder.)
           body (StringBuilder.) ]
      (doseq [ [id tdef] (seq ms) ]
        (let [ ^String tbl (:table tdef) ]
          (when (and (not (:abstract tdef)) (SU/hgl? tbl))
            (debug "model id: " (name id) " table: " tbl)
            (-> drops (.append (genDrop db (.toUpperCase tbl) )))
            (-> body (.append (genOneTable db ms tdef))))))
      (str "" drops body (genEndSQL db)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private drivers-eof nil)

