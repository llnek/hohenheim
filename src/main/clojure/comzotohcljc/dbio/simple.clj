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

(import '(java.sql Connection))

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(require '[comzotohcljc.dbio.core :as DU])
(use '[comzotohcljc.dbio.sql])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn simpleSQLr ""

  ^comzotohcljc.dbio.sql.SQLr

  [^comzotohcljc.dbio.core.DBAPI db
   ^comzotohcljc.dbio.core.MetaCacheAPI metaCache]

  (let [ ^comzotohcljc.dbio.sql.SQLProcAPI proc (make-proc db metaCache) ]
    (reify SQLr

      (findAll [this model ordering] (findSome this model {} ordering))
      (findAll [this model] (findAll this model ""))

      (findOne [this model filters]
        (let [ rset (findSome this model filters "") ]
          (if (empty? rset) nil (first rset))))

      (findSome [this  model filters] (findSome this model filters ""))

      (findSome [this model filters ordering]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
          (let [ zm (get metaCache model)
                 tbl (table-name zm)
                 s (str "SELECT * FROM " (ese tbl))
                 [wc pms] (sql-filter-clause filters)
                 extra (if (SU/hgl? ordering)
                           (str " ORDER BY " ordering)
                           "") ]
            (if (SU/hgl? wc)
              (.doQuery proc conn (str s " WHERE " wc extra) pms model)
              (.doQuery proc conn (str s extra) [] model))) )))

      (update [this obj]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
          (.setAutoCommit conn true)
          (.doUpdate proc conn obj) )) )

      (delete [this obj]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.setAutoCommit conn true)
            (.doDelete proc conn obj) )) )

      (insert [this obj]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.setAutoCommit conn true)
            (.doInsert proc conn obj) )) )

      (select [this sql params]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.doQuery proc conn sql params) )) )

      (executeWithOutput [this sql pms]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.setAutoCommit conn true)
            (.doExecuteWithOutput proc conn sql pms { :pkey "dbio_rowid" } ) )) )

      (execute [this sql pms]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.setAutoCommit conn true)
            (doExecute proc conn sql pms) )) )

      (count* [this model]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.doCount proc conn model) )) )

      (purge [this model]
        (let [ ^Connection dbc (.open db) ]
        (with-open [ conn dbc ]
            (.setAutoCommit conn true)
            (.doPurge proc conn model) )) ) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private simple-eof nil)

