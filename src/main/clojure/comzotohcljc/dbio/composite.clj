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

  comzotohcljc.dbio.composite )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(require '[comzotohcljc.dbio.core :as DU])
(use '[comzotohcljc.dbio.sql])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mk-tx [db metaCache proc conn]
  (reify SQLr

    (findAll [this model ordering] (findSome this model {} ordering))
    (findAll [this model] (findAll this model ""))

    (findOne [this model filters]
      (let [ rset (findSome this model filters "") ]
        (if (empty? rset) nil (first rset))))

    (findSome [this  model filters] (findSome this model filters ""))
    (findSome [this model filters ordering]
      (let [ zm (get metaCache model)
             tbl (table-name zm)
             s (str "SELECT * FROM " (ese tbl))
             [wc pms] (sql-filter-clause filters)
             extra (if (SU/hgl? ordering) (str " ORDER BY " ordering) "") ]
        (if (SU/hgl? wc)
          (.doQuery proc conn (str s " WHERE " wc extra) pms model)
          (.doQuery proc conn (str s extra) [] model))) )

    (select [this sql params] (.doQuery proc conn sql params) )

    (update [this obj] (.doUpdate proc conn obj) )
    (delete [this obj] (.doDelete proc conn obj) )
    (insert [this obj] (.doInsert proc conn obj) )

    (executeWithOutput [this sql pms]
      (.doExecuteWithOutput proc conn sql pms) )

    (execute [this sql pms] (.doExecute proc conn sql pms) )

    (count* [this model] (.doCount proc conn model) )
    (purge [this model] (.doPurge proc conn model) )  ) )


(defn compositeSQLr [db metaCache]
  (let [ proc (make-proc db metaCache) ]
    (reify Transactable

      (execWith [this func]
        (with-local-vars [ rc nil ]
          (let [ conn (begin this) ]
            (try
              (var-set rc (func (mk-tx db metaCache proc conn)))
              (commit this conn)
              @rc
              (catch Throwable e#
                (do (rollback this conn) (warn e#) (throw e#)))
              (finally (.close db conn))))) )

      (rollback [_ conn] (CU/Guard (.rollback conn)))
      (commit [_ conn] (.commit conn))
      (begin [_]
        (doto (.open db)
          (.setAutoCommit false)))   )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private composite-eof nil)

