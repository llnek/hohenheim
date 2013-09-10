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
(import '(java.util GregorianCalendar Calendar))

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
    :spouse { :kind :O2O :rhs (dbio-scopeType "Person") }
                   })
  (with-db-indexes { :i1 #{ :first_name :last_name }
    :i2 #{ :bday }
    } ))

(defjoined EmpDepts
           (dbio-scopeType "Employee")
           (dbio-scopeType "Department"))

(defmodel Employee
  (with-db-parent-model  (dbio-scopeType "Person"))
  (with-db-fields {
    :salary { :domain :Float :null false }
    :pic { :domain :Bytes }
    :passcode { :domain :Password }
    :descr {}
    :login {:null false}
                   })
  (with-db-assocs {
    :depts { :kind :M2M :joined (dbio-scopeType "EmpDepts") }
                   })
  (with-db-indexes { :i1 #{ :login }
    } ))

(defmodel Department
  (with-db-fields {
    :dname { :null false }
                   })
  (with-db-assocs {
    :emps { :kind :M2M :joined (dbio-scopeType "EmpDepts") }
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
    :depts { :kind :O2M :rhs (dbio-scopeType "Department") }
    :emps { :kind :O2M :rhs (dbio-scopeType "Employee") }
    :hq { :kind :O2O :rhs (dbio-scopeType "Address") :singly true }
                   })
  (with-db-uniques {
    :u1 #{ :cname }
    } ))

(def METAC (atom nil))
(def JDBC (atom nil))
(def DB (atom nil))

(defn init-test [f]
  (reset! METAC
    (make-MetaCache (make-Schema
                      [Address Person EmpDepts Employee Department Company])))
  (let [ dir (File. (System/getProperty "java.io.tmpdir"))
         db (str "" (System/currentTimeMillis))
         url (make-h2-db dir db "sa" (CE/pwdify ""))
        jdbc (make-jdbc "x"
               { :d H2-DRIVER :url url :user "sa" :passwd "" }
               (CE/pwdify "")) ]
    (reset! JDBC jdbc)
    (upload-ddl jdbc (getDDL @METAC :h2))
    (reset! DB (dbio-connect jdbc @METAC {})))
  (f)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkt [t] (keyword (str "testcljc.dbio.dbstuff/" t)))


(defn- mkEmp [fname lname login]
  (let [ emp (dbio-create-obj (mkt "Employee")) ]
    (-> emp
      (dbio-set-fld :first_name fname)
      (dbio-set-fld :last_name  lname)
      (dbio-set-fld :iq 100)
      (dbio-set-fld :bday (GregorianCalendar.))
      (dbio-set-fld :sex "male")
      (dbio-set-fld :salary 1000000.00)
      (dbio-set-fld :pic (.getBytes "poo"))
      (dbio-set-fld :passcode "secret")
      (dbio-set-fld :desc "idiot")
      (dbio-set-fld :login login ))))

(defn- create-emp[fname lname login]
  (let [ obj (mkEmp fname lname login)
         sql (.newCompositeSQLr @DB)
         o2 (.execWith sql
             (fn [tx]
               (.insert tx obj))) ]
    o2))

(defn- fetch-all-emps []
  (let [ sql (.newSimpleSQLr @DB)
         o1 (.findAll sql (mkt "Employee")) ]
    o1))

(defn- fetch-emp [login]
  (let [ sql (.newSimpleSQLr @DB)
         o1 (.findOne sql (mkt "Employee") {:login login} ) ]
    o1))

(defn- change-emp [login]
  (let [ sql (.newCompositeSQLr @DB) ]
    (.execWith
      sql
      (fn [tx]
        (let [ o1 (.findOne tx (mkt "Employee") {:login login} )
               o2 (-> o1 (dbio-set-fld :salary 99.9234)
                         (dbio-set-fld :iq 0)) ]
          (.update tx o2))))))

(defn- delete-emp [login]
  (let [ sql (.newCompositeSQLr @DB) ]
    (.execWith
      sql
      (fn [tx]
        (let [ o1 (.findOne tx (mkt "Employee") {:login login} ) ]
          (.delete tx o1))))
    (.execWith
      sql
      (fn [tx]
        (.countAll tx (mkt "Employee"))))))

(defn- create-person [fname lname]
  (let [ p (-> (dbio-create-obj (mkt "Person"))
                (dbio-set-fld :first_name fname)
                (dbio-set-fld :last_name  lname)
                (dbio-set-fld :iq 100)
                (dbio-set-fld :bday (GregorianCalendar.))
                (dbio-set-fld :sex "female"))
         sql (.newCompositeSQLr @DB)
         o2 (.execWith sql
             (fn [tx]
               (.insert tx p))) ]
    o2))

(comment
(defn- wedlock [h w]
  (let [ sql (.newCompositeSQLr @DB) ]
    (binding [ *META-CACHE* (.getMetaCache sql) ]
      (let [ [h1 w1] (dbio-bind-assoc [:as :spouse] h w)
             [h2 w2] (dbio-bind-assoc [:as :spouse] w1 h1) ]
        (.execWith
            sql
            (fn [tx]
              (.update tx h2)
              (.update tx w2)))
          ))))
)

(deftest testdbio-dbstuff

         ;; basic CRUD
         ;;
  (is (let [ m (meta (create-emp "joe" "blog" "joeb"))
             r (:rowid m)
             v (:verid m) ]
        (and (> r 0) (== v 0))))
  (is (let [ a (fetch-all-emps) ]
        (== (count a) 1)))
  (is (let [ a (fetch-emp "joeb" ) ]
        (not (nil? a))))
  (is (let [ a (change-emp "joeb" ) ]
        (not (nil? a))))
  (is (let [ rc (delete-emp "joeb") ]
        (== rc 0)))
  (is (let [ a (fetch-all-emps) ]
        (== (count a) 0)))

         ;; one to one assoc
         ;;
  ()
)

(use-fixtures :each init-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private dbstuff-eof nil)

(clojure.test/run-tests 'testcljc.dbio.dbstuff)

