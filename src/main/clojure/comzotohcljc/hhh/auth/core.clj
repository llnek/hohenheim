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

  comzotohcljc.hhh.auth.core
  (:gen-class))

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(com.zotoh.hohenheim.runtime AuthError UnknownUser))
(import '(com.zotoh.hohenheim.etc PluginFactory Plugin))
(import '(com.zotoh.hohenheim.core Container))

(import '(com.zotoh.frwk.dbio SQLr))
(import '(java.util Properties))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

(require '[clojure.data.json :as json])

(require '[comzotohcljc.crypto.codec :as CE])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hhh.auth.dms])
(use '[comzotohcljc.dbio.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-authRole [ ^SQLr sql ^String role ^String desc]
  (.insert sql (-> (dbio-create-obj :czc.hhh.auth/AuthRole)
                 (dbio-set-fld :name role)
                 (dbio-set-fld :desc desc)) ))

(defn remove-authRole [^SQLr sql role]
  (.execute sql
            (str "delete from "
                 (ese (:table AuthRole))
                 " where "
                 (ese (:column (:name (:fields (meta AuthRole)))))
                 " = ?")
            [(SU/nsb role)]))

(defn list-authRoles [^SQLr sql]
  (.findAll sql :czc.hhh.auth/AuthRole))

(defn create-loginAccount  ""
  [^SQLr sql user ^comzotohcljc.crypto.codec.Password pwdObj roleObjs]
  (let [ acc (.insert sql (-> (dbio-create-obj :czc.hhh.auth/LoginAccount)
                            (dbio-set-fld :acctid (SU/strim user))
                            (dbio-set-fld :passwd (.hashed pwdObj)))) ]
    (doseq [ r (seq roleObjs) ]
      (dbio-set-m2m { :as :roles :with sql } acc r))
    acc))

(defn get-loginAccount  ""
  [^SQLr sql ^String user ^comzotohcljc.crypto.codec.Password pwdObj]
  (let [ acct (.findOne sql :czc.hhh.auth/LoginAccount
                        { :acctid (SU/strim user) } ) ]
    (cond
      (nil? acct)
      (throw (UnknownUser. user))

      (= (.hashed pwdObj) (:passwd acct))
      acct

      :else
      (throw (AuthError. "Incorrect password"))) ))

(defn update-loginAccount [^SQLr sql userObj
                      ^comzotohcljc.crypto.codec.Password pwdObj details]
  (with-local-vars [ u (-> userObj (dbio-set-fld :passwd (.hashed pwdObj))) ]
    (doseq [ [f v] (seq details) ]
      (var-set u (dbio-set-fld @u f v)))
    (.update sql @u)))

(defn remove-loginAccount-role [^SQLr sql userObj roleObj]
  (dbio-clr-m2m {:as :roles :with sql } userObj roleObj))

(defn add-loginAccount-role [^SQLr sql userObj roleObj]
  (dbio-set-m2m {:as :roles :with sql } userObj roleObj))

(defn remove-loginAccount [^SQLr sql userObj]
  (.delete sql userObj))

(defn delete-loginAccount [^SQLr sql user]
  (.execute
    sql
    (str "delete from " (ese (:table LoginAccount))
         " where " (ese (:column (:acctid (:fields (meta LoginAccount)))))
         " =?")
    [ (SU/strim user) ]))

(defn list-loginAccounts [^SQLr sql]
  (.findAll sql :czc.hhh.auth/LoginAccount))

(defn make-auth-plugin ^Plugin []
  (let [ impl (CU/make-mmap) ]
    (reify
      Plugin
      (contextualize [_ ctr]
        (.mm-s impl :appDir (.getAppDir ^Container ctr))
        (.mm-s impl :appKey (.getAppKey ^Container ctr)))
      (configure [_ props]
        (let [ dbs (:databases (:env props)) ]
          (.mm-s impl :cfg (:jdbc dbs)) ))
      (initialize [_]
        (let [ pkey (.mm-g impl :appKey)
               cfg (get (.mm-g impl :cfg) (keyword "*"))
               j (make-jdbc "x" cfg (CE/pwdify (:passwd cfg) pkey)) ]
            (apply-authPlugin-ddl j)))
      (start [_]
        (info "AuthPlugin started."))
      (stop [_]
        (info "AuthPlugin stopped."))
      (dispose [_]
        (info "AuthPlugin disposed."))) ))

(deftype AuthPluginFactory []
  PluginFactory
  (createPlugin [_]
    (require 'comzotohcljc.hhh.auth.core)
    (make-auth-plugin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- doMain [& args]
  (let [ appDir (File. ^String (nth args 0))
         ^Properties mf (CU/load-javaprops (File. appDir "META-INF/MANIFEST.MF"))
         pkey (.getProperty mf "Implementation-Vendor-Id")
         ^String cmd (nth args 1)
         ^String db (nth args 2)
         env (json/read-str
               (FileUtils/readFileToString (File. appDir "conf/env.conf") "utf-8")
               :key-fn keyword)
         cfg (get (:jdbc (:databases env)) (keyword db)) ]
    (when-not (nil? cfg)
      (let [ j (make-jdbc db cfg (CE/pwdify (:passwd cfg) pkey))
             t (match-jdbc-url (SU/nsb (:url cfg))) ]
        (cond
          (= "init-db" cmd)
          (let []
            (apply-authPlugin-ddl j))

          (= "gen-sql" cmd)
          (if (> (count args) 3)
            (export-authPlugin-ddl t (File. ^String (nth args 3))))

          :else
          nil)) )))



;; home gen-sql alias outfile
;; home init-db alias
(defn -main "Main Entry" [& args]
  ;; for security, don't just eval stuff
  ;;(alter-var-root #'*read-eval* (constantly false))
  (if (< (count args) 3)
    nil
    (apply doMain args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private core-eof nil)


