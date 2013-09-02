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


;;(println (getDDL (make-MetaCache testschema) (H2.) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private h2-eof nil)



