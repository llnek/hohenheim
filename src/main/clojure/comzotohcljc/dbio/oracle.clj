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
  comzotohcljc.dbio.oracle)

(import '(java.util HashMap))

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn- create_sequence_trigger [db table col]
  (str "CREATE OR REPLACE TRIGGER TRIG_" table "\n"
        "BEFORE INSERT ON " table "\n"
        "REFERENCING NEW AS NEW\n"
        "FOR EACH ROW\n"
        "BEGIN\n"
        "SELECT SEQ_" table ".NEXTVAL INTO :NEW."
        col " FROM DUAL;\n"
        "END" (genExec db) "\n\n"))

(defn- create_sequence [db table]
  (str "CREATE SEQUENCE SEQ_" table
       " START WITH 1 INCREMENT BY 1"
          (genExec db) "\n\n"))

(deftype Oracle [] DBDriver
  (getId [_] :oracle)
  (getTestString [_] "select 1 from DUAL" ))

;; Oracle
(defmethod getStringKeyword Oracle [db] "VARCHAR2")
(defmethod getTSDefault Oracle [db] "DEFAULT SYSTIMESTAMP")
(defmethod getLongKeyword Oracle [db] "NUMBER(38)")
(defmethod getDoubleKeyword Oracle [db] "BINARY_DOUBLE")
(defmethod getFloatKeyword Oracle [db] "BINARY_FLOAT")

(defmethod genAutoInteger Oracle [db table fld]
  (do
    (.put ^HashMap *DDL_BVS* table (:column fld))
    (genInteger db fld)))

(defmethod genAutoLong Oracle [db table fld]
  (do
    (.put ^HashMap *DDL_BVS* table (:column fld))
    (genLong db fld)))

(defmethod genEndSQL Oracle [db]
  (let [ m (into {} *DDL_BVS*) bf (StringBuilder.) ]
    (doseq [ en (seq m) ]
      (doto bf
        (.append (create_sequence db (first en)))
        (.append (create_sequence_trigger db (first en) (last en)))))
    (.toString bf)))

(defmethod genDrop Oracle [db table]
  (str "DROP TABLE " table " CASCADE CONSTRAINTS PURGE" (genExec db) "\n\n"))


;;(println (getDDL (Oracle.) (make-MetaCache testschema)))

(def ^:private oracle-eof nil)

