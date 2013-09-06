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

(import '(com.zotoh.frwk.dbio Schema))
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.core])

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
      :desc { :null false }
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
    { :roles { :kind :M2M  :joined :AccountRole  :rhs :AuthRole }
      :addr { :kind :O2M :singly true :rhs :StdAddress }
     })
  (with-db-uniques
    { :u2 #{ :acctid }
     }) )

(defjoined AccountRole)


(deftype ModuleSchema []
  Schema
  (getModels [_] [ StdAddress AuthRole LoginAccount AccountRole] ))

(println
(getDDL (make-MetaCache (ModuleSchema.)) nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dms-eof nil)

