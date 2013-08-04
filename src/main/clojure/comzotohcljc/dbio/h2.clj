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

  comzotohcljc.dbio.h2)

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

(import '(com.zotoh.frwk.dbio DBIOError))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(deftype H2 [] DBDriver
  (getId [_] :h2)
  (getTestString [_] "select 1" ))

;; H2
(defmethod getDateKeyword H2 [db] "TIMESTAMP")
(defmethod getDoubleKeyword H2 [db] "DOUBLE")
(defmethod getBlobKeyword H2 [db] "BLOB")
(defmethod getFloatKeyword H2 [db] "FLOAT")

(defmethod genAutoInteger H2 [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))

(defmethod genAutoLong H2 [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db)
            (if (:pkey fld) " IDENTITY(1) " " AUTO_INCREMENT(1) ")))

(defmethod genBegin H2 [db table]
  (str "CREATE CACHED TABLE " table "\n(\n" ))

(defmethod genDrop H2 [db table]
  (str "DROP TABLE " table " IF EXISTS CASCADE" (genExec db) "\n\n"))


;;(println (getDDL (H2.) (make-MetaCache testschema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private h2-eof nil)



