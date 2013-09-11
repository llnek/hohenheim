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

  comzotohcljc.hhh.auth.dms )

(import '(com.zotoh.frwk.dbio DBIOError JDBCInfo Schema))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.postgresql])
(use '[comzotohcljc.dbio.h2])
(use '[comzotohcljc.dbio.mysql])
(use '[comzotohcljc.dbio.sqlserver])
(use '[comzotohcljc.dbio.oracle])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodel StdAddress
  (with-db-fields {
    :addr1 { :size 200 :null false }
    :addr2 { :size 64}
    :city { :null false}
    :state {:null false}
    :zip {:null false}
    :country {:null false}
                   })
  (with-db-indexes { :i1 #{ :city :state :country }
    :i2 #{ :zip :country }
    :state #{ :state }
    :zip #{ :zip } } ))

(defmodel AuthRole
  (with-db-fields
    { :name { :column "role_name" :null false }
      :desc { :column "description" :null false }
     })
  (with-db-uniques
    { :u1 #{ :name }
     }) )

(defmodel LoginAccount
  (with-db-fields
    { :acctid { :null false }
      :passwd { :null false :domain :Password }
     })
  (with-db-assocs
    { :roles { :kind :M2M
               :joined :comzotohcljc.hhh.auth.dms/AccountRole
               :rhs :comzotohcljc.hhh.auth.dms/AuthRole }
      :addr { :kind :O2M
              :singly true
              :rhs :comzotohcljc.hhh.auth.dms/StdAddress }
     })
  (with-db-uniques
    { :u2 #{ :acctid }
     }) )

(defjoined AccountRole
           :comzotohcljc.hhh.auth.dms/LoginAccount
           :comzotohcljc.hhh.auth.dms/AuthRole)

(deftype ModuleSchema []
  Schema
  (getModels [_] [ StdAddress AuthRole LoginAccount AccountRole] ))

(defn generate-ddl ^String [dbtype]
  (getDDL (make-MetaCache (ModuleSchema.))
    (case dbtype
      (:postgres :postgresql) Postgresql
      :mysql MySQL
      :h2 H2
      (:sqlserver :mssql) SQLServer
      :oracle Oracle
      (throw (DBIOError. (str "Unsupported database type: " dbtype)))) ))

(defn apply-ddl [^JDBCInfo jdbc]
  (let [ dbtype (match-jdbc-url (.getUrl jdbc)) ]
    (upload-ddl jdbc (generate-ddl dbtype))) )

(defn export-ddl [dbtype ^File file]
  (FileUtils/writeStringToFile file (generate-ddl dbtype) "utf-8"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dms-eof nil)

