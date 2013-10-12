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

(use '[clojure.tools.logging :only [info warn error debug] ])

(import '(com.zotoh.hohenheim.runtime AuthError UnknownUser))
(import '(com.zotoh.hohenheim.etc PluginFactory Plugin))
(import '(com.zotoh.hohenheim.core Container))

(import '(com.zotoh.frwk.dbio SQLr))
(import '(java.util Properties))
(import '(java.io File))
(import '(org.apache.commons.io FileUtils))

(import '(org.apache.shiro.config IniSecurityManagerFactory))
(import '(org.apache.shiro SecurityUtils))
(import '(org.apache.shiro.subject Subject))

(use '[comzotohcljc.util.core :only [make-mmap uid load-javaprops] ])
(use '[comzotohcljc.crypto.codec :only [pwdify] ])
(use '[comzotohcljc.util.str :only [nsb strim] ])
(use '[comzotohcljc.hhh.auth.dms])
(use '[comzotohcljc.dbio.core])
(require '[clojure.data.json :as json])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-authRole "" [^SQLr sql ^String role ^String desc]
  (.insert sql (-> (dbio-create-obj :czc.hhh.auth/AuthRole)
                 (dbio-set-fld :name role)
                 (dbio-set-fld :desc desc)) ))

(defn remove-authRole "" [^SQLr sql role]
  (.execute sql
            (str "delete from "
                 (ese (:table AuthRole))
                 " where "
                 (ese (:column (:name (:fields (meta AuthRole)))))
                 " = ?")
            [(nsb role)]))

(defn list-authRoles ""
  [^SQLr sql]
  (.findAll sql :czc.hhh.auth/AuthRole))

(defn create-loginAccount  "" [^SQLr sql
                               ^String user
                               ^comzotohcljc.crypto.codec.Password pwdObj roleObjs]
  (let [ [p s] (.hashed pwdObj)
         acc (.insert sql (-> (dbio-create-obj :czc.hhh.auth/LoginAccount)
                            (dbio-set-fld :acctid (strim user))
                            (dbio-set-fld :salt s)
                            (dbio-set-fld :passwd  p))) ]
    (doseq [ r (seq roleObjs) ]
      (dbio-set-m2m { :as :roles :with sql } acc r))
    acc))

(defn get-loginAccount  "" [^SQLr sql
                            ^String user
                            ^comzotohcljc.crypto.codec.Password pwdObj]
  (let [ acct (.findOne sql :czc.hhh.auth/LoginAccount
                        { :acctid (strim user) } ) ]
    (cond
      (nil? acct)
      (throw (UnknownUser. user))

      (.validateHash pwdObj (:passwd acct))
      acct

      :else
      (throw (AuthError. "Incorrect password"))) ))

(defn change-loginAccount "" [^SQLr sql
                              userObj
                              ^comzotohcljc.crypto.codec.Password pwdObj ]
  (let [ [p s] (.hashed pwdObj)
         u (-> userObj
              (dbio-set-fld :passwd p)
              (dbio-set-fld :salt s)) ]
    (.update sql u)))

(defn update-loginAccount "" [^SQLr sql userObj details]
  (if (empty? details)
    userObj
    (with-local-vars [ u userObj ]
      (doseq [ [f v] (seq details) ]
        (var-set u (dbio-set-fld @u f v)))
      (.update sql @u)) ))

(defn remove-loginAccountRole "" [^SQLr sql userObj roleObj]
  (dbio-clr-m2m {:as :roles :with sql } userObj roleObj))

(defn add-loginAccountRole "" [^SQLr sql userObj roleObj]
  (dbio-set-m2m {:as :roles :with sql } userObj roleObj))

(defn remove-loginAccount "" [^SQLr sql userObj]
  (.delete sql userObj))

(defn delete-loginAccount "" [^SQLr sql user]
  (.execute
    sql
    (str "delete from " (ese (:table LoginAccount))
         " where " (ese (:column (:acctid (:fields (meta LoginAccount)))))
         " =?")
    [ (strim user) ]))

(defn list-loginAccounts "" [^SQLr sql]
  (.findAll sql :czc.hhh.auth/LoginAccount))

(defn- init-shiro "" [^File appDir ^String appKey]
  (let [ ini (File. appDir "conf/shiro.ini")
         sm (-> (IniSecurityManagerFactory. (-> ini (.toURI)(.toURL)(.toString)))
              (.getInstance)) ]
    (SecurityUtils/setSecurityManager sm)
    (info "created shiro security manager: " sm)
  ))

(defn makeAuthPlugin ""
  ^Plugin
  []
  (let [ impl (make-mmap) ]
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
               cfg (get (.mm-g impl :cfg) (keyword "_"))
               j (make-jdbc (uid) cfg (pwdify (:passwd cfg) pkey)) ]
          (applyAuthPluginDDL j)
          (init-shiro (.mm-g impl :appDir)
                      (.mm-g impl :appKey))))
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
    (makeAuthPlugin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- doMain [& args]
  (let [ appDir (File. ^String (nth args 0))
         ^Properties mf (load-javaprops (File. appDir "META-INF/MANIFEST.MF"))
         pkey (.getProperty mf "Implementation-Vendor-Id")
         ^String cmd (nth args 1)
         ^String db (nth args 2)
         env (json/read-str
               (FileUtils/readFileToString (File. appDir "conf/env.conf") "utf-8")
               :key-fn keyword)
         cfg (get (:jdbc (:databases env)) (keyword db)) ]
    (when-not (nil? cfg)
      (let [ j (make-jdbc db cfg (pwdify (:passwd cfg) pkey))
             t (match-jdbc-url (nsb (:url cfg))) ]
        (cond
          (= "init-db" cmd)
          (let []
            (applyAuthPluginDDL j))

          (= "gen-sql" cmd)
          (if (> (count args) 3)
            (exportAuthPluginDDL t (File. ^String (nth args 3))))

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


