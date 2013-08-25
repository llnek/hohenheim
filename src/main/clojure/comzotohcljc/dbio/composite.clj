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

  comzotohcljc.dbio.composite )

(import '(java.sql Connection))

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(require '[comzotohcljc.dbio.core :as DU])
(use '[comzotohcljc.dbio.sql])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)



(defn- mk-tx 
  
  ^comzotohcljc.dbio.sql.SQLr
  [^comzotohcljc.dbio.core.DBAPI db
   ^comzotohcljc.dbio.core.MetaCacheAPI metaCache 
   ^comzotohcljc.dbio.sql.SQLProcAPI proc 
   ^Connection conn]

  (reify SQLr

    (findAll [this model ordering] (findSome this model {} ordering))
    (findAll [this model] (findAll this model ""))

    (findOne [this model filters]
      (let [ rset (findSome this model filters "") ]
        (if (empty? rset) nil (first rset))))

    (findSome [this  model filters] (findSome this model filters ""))
    (findSome [_ model filters ordering]
      (let [ zm (get metaCache model)
             tbl (table-name zm)
             s (str "SELECT * FROM " (ese tbl))
             [wc pms] (sql-filter-clause filters)
             extra (if (SU/hgl? ordering) (str " ORDER BY " ordering) "") ]
        (if (SU/hgl? wc)
          (.doQuery proc conn (str s " WHERE " wc extra) pms model)
          (.doQuery proc conn (str s extra) [] model))) )

    (select [_ sql params] (.doQuery proc conn sql params) )

    (update [_ obj] (.doUpdate proc conn obj) )
    (delete [_ obj] (.doDelete proc conn obj) )
    (insert [_ obj] (.doInsert proc conn obj) )

    (executeWithOutput [_ sql pms]
      (.doExecuteWithOutput proc conn sql pms { :pkey "dbio_rowid" } ) )

    (execute [_ sql pms] (.doExecute proc conn sql pms) )

    (count* [_ model] (.doCount proc conn model) )
    (purge [_ model] (.doPurge proc conn model) )  ) )


(defn compositeSQLr 
  
  ^comzotohcljc.dbio.sql.Transactable
  [^comzotohcljc.dbio.core.DBAPI db 
   ^comzotohcljc.dbio.core.MetaCacheAPI metaCache]

  (let [ proc (make-proc db metaCache) ]
    (reify Transactable

      (execWith [this func]
        (with-local-vars [ rc nil ]
          (with-open [ conn (begin this) ]
            (try
              (var-set rc (func (mk-tx db metaCache proc conn)))
              (commit this conn)
              @rc
              (catch Throwable e#
                (do (rollback this conn) (warn e# "") (throw e#))) ))))

      (rollback [_ conn] (CU/Try! (.rollback ^Connection conn)))
      (commit [_ conn] (.commit ^Connection conn))
      (begin [_]
        (let [ ^Connection conn (.open db) ]
          (.setAutoCommit conn false)))   )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composite-eof nil)

