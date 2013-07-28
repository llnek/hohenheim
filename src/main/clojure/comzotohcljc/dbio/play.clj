
(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.dbio.play )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.crypto.codec :as CD])

(use '[comzotohcljc.dbio.core])
(use '[comzotohcljc.dbio.composite])
(use '[comzotohcljc.dbio.simple])
(use '[comzotohcljc.dbio.connect])

(use '[comzotohcljc.dbio.postgresql])


(def CFG (make-jdbc "org.postgresql.Driver" "jdbc:postgresql://localhost:5432/tracking" "narvar" (CD/pwdify "Helpme911")))

(defmodel address
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

(defmodel person
  (with-db-abstract)
  (with-db-fields {
    :fname { :null false }
    :lname { :null false }
    :age { :domain :int }
    :pic { :domain :bytes }
                   })
  (with-db-assocs {
    :addr { :kind :o2m :singly true :rhs :address }
    :spouse { :kind :o2o :rhs :person }
    :accts { :kind :o2m :rhs :bankacct }
                   })
  (with-db-indexes { :i1 #{ :age } })
  (with-db-uniques { :u2 #{ :fname :lname } }))

(defmodel president
  (with-db-parent-model :person))

(defmodel bankacct
  (with-db-fields {
    :acctid { :null false }
    :amount { :null false :domain :double }
                   })
  (with-db-uniques { :u2 #{ :acctid } }))


(def testschema (make-Schema [ president address person bankacct ]))

(def MCACHE (make-MetaCache testschema))






(def ^:private play-eof nil)

