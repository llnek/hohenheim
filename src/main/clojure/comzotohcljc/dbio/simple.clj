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

  comzotohcljc.dbio.simple )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.dbio.core :as DU])

(use '[comzotohcljc.dbio.sqlops])

(import '(java.util GregorianCalendar TimeZone))
(import '(java.sql Types))
(import '(java.math BigDecimal BigInteger))
(import '(java.sql Date Timestamp Blob Clob))
(import '(java.io Reader InputStream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn simpleSQLr "" [db metaCache proc]
  (reify SQLr

    (findAll [this model ordering] (findSome this model {} ordering))
    (findAll [this model] (findAll this model ""))

    (findOne [this model filters]
      (let [ rset (findSome this model filters "") ]
        (if (empty? rset) nil (first rset))))

    (findSome [this  model filters] (findSome this model filters ""))

    (findSome [this model filters ordering]
      (let [ conn (.open db) ]
        (try
          (let [ zm (get metaCache model)
                 tbl (table-name zm)
                 s (str "SELECT * FROM " (ese tbl))
                 [wc pms] (sql-filter-clause filters)
                 extra (if (SU/hgl? ordering) (str " ORDER BY " ordering) "") ]
            (if (SU/hgl? wc)
              (.doQuery proc conn (str s " WHERE " wc extra) pms model)
              (.doQuery proc conn (str s extra) [] model)))
          (finally
            (.close db conn)))))

    (update [this obj]
      (let [ conn (.open db) ]
        (try
          (.setAutoCommit conn true)
          (.doUpdate proc conn obj)
          (finally (.close db conn)))))

    (delete [this obj]
      (let [ conn (.open db) ]
        (try
          (.setAutoCommit conn true)
          (.doDelete proc conn obj)
          (finally (.close db conn)))))

    (insert [this obj]
      (let [ conn (.open db) ]
        (try
          (.setAutoCommit conn true)
          (.doInsert proc conn obj)
          (finally (.close db conn)))))

    (select [this sql params]
      (let [ conn (.open db) ]
        (try
          (.doQuery proc conn sql params)
        (finally (.close db conn)))))

    (executeWithOutput [this sql pms]
      (let [ conn (.open db) ]
        (try
          (.setAutoCommit conn true)
          (.doExecuteWithOutput proc conn sql pms)
        (finally (.close db conn)))))

    (execute [this sql pms]
      (let [ conn (.open db) ]
        (try
          (.setAutoCommit conn true)
          (doExecute proc conn sql pms)
        (finally (.close db conn)))))

    (count* [this model]
      (let [ conn (.open db) ]
        (try
          (.doCount proc conn model)
        (finally (.close db conn)))))

    (purge [this model]
      (let [ conn (.open db) ]
        (try
          (.doPurge proc conn model)
        (finally (.close db conn)))))   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private simple-eof nil)

