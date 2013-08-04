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
  comzotohcljc.dbio.sqlserver)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(deftype SQLServer [] DBDriver
  (getId [_] :sqlserver)
  (getTestString [_] "select count(*) from sysusers" ))

;; SQLServer
(defmethod getBlobKeyword SQLServer [db] "IMAGE")
(defmethod getTSKeyword SQLServer [db] "DATETIME")
(defmethod getDoubleKeyword SQLServer [db] "FLOAT(53)")
(defmethod getFloatKeyword SQLServer [db] "FLOAT(53)")

(defmethod genAutoInteger SQLServer [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genAutoLong SQLServer [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db)
    (if (:pkey fld) " IDENTITY (1,1) " " AUTOINCREMENT ")))

(defmethod genDrop SQLServer [db table]
  (str "IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id=object_id('"
       table "')) DROP TABLE " table (genExec db) "\n\n"))


;;(println (getDDL (SQLServer.) (make-MetaCache testschema)))


(def ^:private sqlserver-eof nil)

