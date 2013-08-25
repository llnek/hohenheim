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

