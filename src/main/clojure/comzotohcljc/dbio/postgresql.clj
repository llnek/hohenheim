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
  comzotohcljc.dbio.postgresql)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(deftype Postgresql [] DBDriver
  (getId [_] :postgresql)
  (getTestString [_] "select 1" ))

;; Postgresql
(defmethod getTSKeyword Postgresql [db] "TIMESTAMP WITH TIME ZONE")
(defmethod getBlobKeyword Postgresql [db] "BYTEA")
(defmethod getDoubleKeyword Postgresql [db] "DOUBLE PRECISION")
(defmethod getFloatKeyword Postgresql [db] "REAL")

(defmethod genCal Postgresql [db fld] (genTimestamp db fld))

(defmethod genAutoInteger Postgresql [db table fld]
  (genColDef db (genCol fld) "SERIAL" false nil))

(defmethod genAutoLong Postgresql [db table fld]
  (genColDef db (genCol fld) "BIGSERIAL" false nil))

(defmethod genDrop Postgresql [db table]
  (str "DROP TABLE IF EXISTS " table " CASCADE" (genExec db) "\n\n"))

;;(def XXX (.getMetas (make-MetaCache testschema)))
;;(println (getDDL (Postgresql.) (make-MetaCache testschema)))


(def ^:private postgresql-eof nil)

