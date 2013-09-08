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


(ns testcljc.dbio.dbstuff)

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io File))

(require '[comzotohcljc.crypto.codec :as CE])
(use '[comzotohcljc.dbio.drivers])
(use '[comzotohcljc.dbio.connect])
(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.h2])
(use '[clojure.test])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodel Address
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

(defmodel Person
  (with-db-fields {
    :first_name { :null false }
    :last_name { :null false }
    :iq { :domain :Int }
    :bday {:domain :Calendar :null false }
    :sex {:null false}
                   })
  (with-db-assocs {
    :spouse { :kind :O2O :rhs :Person }
                   })
  (with-db-indexes { :i1 #{ :first_name :last_name }
    :i2 #{ :bday }
    } ))

(defjoined EmpDepts)

(defmodel Employee
  (with-db-parent-model  :Person)
  (with-db-fields {
    :salary { :domain :Float :null false }
    :pic { :domain :Bytes }
    :passcode { :domain :Password }
    :descr {}
    :login {:null false}
                   })
  (with-db-assocs {
    :depts { :kind :M2M :rhs :Department :joined :EmpDepts }
                   })
  (with-db-indexes { :i1 #{ :login }
    } ))

(defmodel Department
  (with-db-fields {
    :dname { :null false }
                   })
  (with-db-assocs {
    :emps { :kind :M2M :rhs :Employee :joined :EmpDepts }
                   })
  (with-db-uniques {
    :u1 #{ :dname }
    } ))

(defmodel Company
  (with-db-fields {
    :cname { :null false }
    :revenue { :domain :Double :null false }
    :logo { :domain :Bytes }
                   })
  (with-db-assocs {
    :depts { :kind :O2M :rhs :Department }
    :emps { :kind :O2M :rhs :Employee }
    :hq { :kind :O2M :rhs :Address :singly true }
                   })
  (with-db-uniques {
    :u1 #{ :cname }
    } ))

(def METAC (atom nil))

(defn init-test [f]
  (reset! METAC
    (make-MetaCache (make-Schema
                      [Address Person EmpDepts Employee Department Company])))
  (let [ dir (File. (System/getProperty "java.io.tmpdir"))
         db (str "" (System/currentTimeMillis))
         url (make-h2-db dir db "sa" (CE/pwdify "")) ]
    (upload-ddl
      (make-jdbc "x"
               { :d H2-DRIVER :url url :user "sa" :passwd "" }
               (CE/pwdify ""))
      (getDDL @METAC :h2))
  (f)
    ))

(defn- mk-employee []
  (let [ emp (dbio-new-xxx :Employee)
  (dbio-set-field obj :f "")





(deftest testdbio-dbstuff
  (is (= 1 1))
)

(use-fixtures :each init-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dbstuff-eof nil)

;;(clojure.test/run-tests 'testcljc.dbio.dbstuff)

