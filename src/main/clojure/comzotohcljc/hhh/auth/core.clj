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

  comzotohcljc.hhh.auth.core )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-role [^SQLr db role desc]
  (.insert db { :name (SU/nsb role) :desc (SU/nsb desc) } ))

(defn remove-role [^SQLr db role]
  (.delete db (SU/nsb role)))

(defn list-roles [^SQLr db]
  (.findAll db :AuthRole))

(defn create-account [^SQLr db user passwd roles]
  (let [ v1 { :acctid (SU/nsb user) :passwd (SU/nsb passwd) }
         v2  (.insert db v1) ]
  ))

(defn get-account [^SQLr db user passwd]
  (.findOne db  { :acctid user } ))

(defn update-account [^SQLr db user details]
  )

(defn remove-account [^SQLr db user]
  (.delete db (SU/nsb user)))

(defn list-accounts [^SQLr db]
  (.findAll db :LoginAccount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private core-eof nil)


