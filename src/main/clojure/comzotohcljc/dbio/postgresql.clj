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
;;(println (getDDL (make-MetaCache testschema) (Postgresql.) ))


(def ^:private postgresql-eof nil)

