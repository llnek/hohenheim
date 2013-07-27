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
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.dbio.core :as DU])

(use '[comzotohcljc.dbio.sqlops])

(import '(java.util GregorianCalendar TimeZone))
(import '(java.sql Types))
(import '(java.math BigDecimal BigInteger))
(import '(java.sql Date Timestamp Blob Clob))
(import '(java.io Reader InputStream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Transaction [_db _metaCache _proc _conn] SQLr

  (findAll [this model ordering] (findSome this model {} ordering))
  (findAll [this model] (findAll this model ""))

  (findOne [this model filters]
    (let [ rset (findSome this model filters "") ]
      (if (empty? rset) nil (first rset))))

  (findSome [this  model filters] (findSome this model filters ""))
  (findSome [this model filters ordering]
    (let [ zm (get _metaCache model)
           tbl (table-name zm)
           s (str "SELECT * FROM " (ese tbl))
           [wc pms] (sql-filter-clause filters)
           extra (if (SU/hgl? ordering) (str " ORDER BY " ordering) "") ]
      (if (SU/hgl? wc)
        (.doQuery _proc _conn (str s " WHERE " wc extra) pms model)
        (.doQuery _proc _conn (str s extra) [] model))) )

  (select [this sql params] (.doQuery _proc _conn sql params) )

  (update [this obj] (.doUpdate _proc _conn obj) )
  (delete [this obj] (.doDelete _proc _conn obj) )
  (insert [this obj] (.doInsert _proc _conn obj) )

  (executeWithOutput [this sql pms]
    (.doExecuteWithOutput _proc _conn sql pms) )

  (execute [this sql pms] (.doExecute _proc _conn sql pms) )

  (count* [this model] (.doCount _proc _conn model) )
  (purge [this model] (.doPurge _proc _conn model) )   )

(deftype CompositeSQLr [_db _metaCache] Transactable

  (execWith [this func]
    (let [ proc (comzotohcljc.dbio.sqlops.SQLProc. _db _metaCache)
           conn (begin this)
           rc (atom nil)
           tx (Transaction. _db _metaCache proc conn) ]
      (try
        (reset! rc (apply func tx))
        (commit this conn)
        @rc
        (catch Throwable e#
          (do (rollback this conn) (warn e#) (throw e#)))
        (finally (.close _db conn)))))

  (rollback [_ conn] (CU/TryC (.rollback conn)))

  (commit [_ conn] (.commit conn))

  (begin [_]
    (doto (.open _db)
      (.setAutoCommit false)))   )


(def ^:private composite-eof nil)

