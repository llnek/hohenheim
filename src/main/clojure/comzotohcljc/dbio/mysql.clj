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
  comzotohcljc.dbio.mysql)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(deftype MySQL [] DBDriver
  (getId [_] :mysql)
  (getTestString [_] "select version()" ))

;; MySQL
(defmethod getBlobKeyword MySQL [db] "LONGBLOB")
(defmethod getTSKeyword MySQL [db] "TIMESTAMP")
(defmethod getDoubleKeyword MySQL [db] "DOUBLE")
(defmethod getFloatKeyword MySQL [db]  "DOUBLE")

(defmethod genEnd MySQL [db table]
  (str "\n) Type=InnoDB" (genExec db) "\n\n"))

(defmethod genAutoInteger MySQL [db table fld]
  (str (getPad db) (genCol fld) " " (getIntKeyword db) " NOT NULL AUTO_INCREMENT"))

(defmethod genAutoLong MySQL [db table fld]
  (str (getPad db) (genCol fld) " " (getLongKeyword db) " NOT NULL AUTO_INCREMENT"))

(defmethod genDrop MySQL [db table]
  (str "DROP TABLE IF EXISTS " table (genExec db) "\n\n"))


;;(println (getDDL (MySQL.) (make-MetaCache testschema)))

(def ^:private mysql-eof nil)

