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
(import '(com.zotoh.hohenheim.etc
  PluginFactory Plugin PluginError))
(import '(com.zotoh.hohenheim.core Container))
(import '(org.apache.commons.codec.binary Base64))

(import '(com.zotoh.frwk.net ULFormItems ULFileItem))

(import '(com.zotoh.frwk.dbio
  DBAPI MetaCache SQLr JDBCPool JDBCInfo))

(import '(org.apache.commons.io FileUtils))
(import '(java.io File IOException))
(import '(java.util Properties))

(import '(org.apache.shiro.config IniSecurityManagerFactory))
(import '(org.apache.shiro SecurityUtils))
(import '(org.apache.shiro.subject Subject))

(import '( com.zotoh.wflow
  FlowPoint Activity Pipeline PipelineDelegate PTask Work))
(import '(com.zotoh.hohenheim.io HTTPEvent HTTPResult))
(import '(com.zotoh.wflow.core Scope))


(use '[comzotohcljc.util.core :only [stringify make-mmap uid load-javaprops] ])
(use '[comzotohcljc.crypto.codec :only [pwdify] ])
(use '[comzotohcljc.util.str :only [nsb hgl? strim] ])
(use '[comzotohcljc.hhh.core.constants])
(use '[comzotohcljc.hhh.auth.dms])
(use '[comzotohcljc.dbio.connect :only [dbio-connect] ])
(use '[comzotohcljc.dbio.core])
(require '[clojure.data.json :as json])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def LF-PASSWORD "password")
(def LF-USER "user")

(defprotocol AuthPlugin
  ""
  (getRoles [_ acctObj ] )
  (getAccount [_ user pwdObj]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-loginTask "" []
  (PTask. (reify Work
    (perform [_ fw scope arg]
      (let [ ^comzotohcljc.hhh.core.sys.Element ctr (.container ^Scope scope)
             ^comzotohcljc.hhh.auth.core.AuthPlugin
             pa (:auth (.getAttr ctr K_PLUGINS))
             ^HTTPEvent evt (.event ^Scope scope)
             data (.data evt)
             ^comzotohcljc.netty.ios.WebSession ss (.getSession evt) ]
        (debug "dude ! , data = " (type data))
        (when (nil? pa) (throw (PluginError. "AuthPlugin missing.")))
        (debug "auth-plugin - ok")
        (with-local-vars [user "" pwd ""]
          (cond
            (instance? ULFormItems data)
            (doseq [ ^ULFileItem
                     x (filter #(.isFormField ^ULFileItem %)
                               (.getAll ^ULFormItems data)) ]
              (debug "Field-name ===== " (.getFieldName x))
              (debug "Field-val ===== " (.getString x))
              (case (.getFieldName x)
                "password" (var-set pwd  (.getString x))
                "user" (var-set user (.getString x))
                nil))

            (hgl? (.getHeaderValue evt "authorization"))
            (let [ s (stringify (Base64/decodeBase64 (.getHeaderValue evt "authorization")))
                   pos (.indexOf s ":") ]
              (when (pos > 0)
                (var-set pwd (.substring s (inc pos)))
                (var-set user (.substring s 0 pos))))

            (and (hgl? (.getParameterValue evt LF-PASSWORD))
                 (hgl? (.getParameterValue evt LF-USER)))
            (do
              (var-set pwd (.getParameterValue evt LF-PASSWORD))
              (var-set user (.getParameterValue evt LF-USER)))

            :else
            nil)
          (debug "USER ==== " @user)
          (debug "PWD ==== " @pwd)
          (if (and (hgl? @user)
                   (hgl? @pwd))
            (let [ acct (.getAccount pa @user @pwd)
                   rs (.getRoles pa acct) ]
              (.setAttribute ss LF-PASSWORD @pwd)
              (.setAttribute ss LF-USER @user)
              (.setAttribute ss "roles" rs))
            (throw (AuthError. "Unknown user or bad password."))))

        )))))

(defn make-verifyAcctTask "" []
  (PTask. (reify Work
    (perform [_ fw scope arg]
      (let [ ^comzotohcljc.hhh.core.sys.Element ctr (.container ^Scope scope)
             ^comzotohcljc.hhh.auth.core.AuthPlugin
             pa (:auth (.getAttr ctr K_PLUGINS))
             ^HTTPEvent evt (.event ^Scope scope)
             ^comzotohcljc.netty.ios.WebSession ss (.getSession evt)
             ^String user (.getAttribute ss WS_USER)
             ^String pwd (.getAttribute ss WS_CRED) ]
      (when (nil? pa) (throw (PluginError. "AuthPlugin missing.")))
      (let [ acct (.getAccount pa user pwd)
             rs (.getRoles pa acct) ]
        ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkjdbc ^JDBCInfo [^comzotohcljc.util.core.MutableMapAPI impl]
  (let [ pkey (.mm-g impl :appKey)
         cfg (get (.mm-g impl :cfg) (keyword "_")) ]
    (make-jdbc "_" cfg (pwdify (:passwd cfg) pkey))))

(defn- makeAuthPlugin ""
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
        (let []
          (applyAuthPluginDDL (mkjdbc impl))
          (init-shiro (.mm-g impl :appDir)
                      (.mm-g impl :appKey))))
      (start [_]
        (info "AuthPlugin started."))
      (stop [_]
        (info "AuthPlugin stopped."))
      (dispose [_]
        (info "AuthPlugin disposed."))

      AuthPlugin
      (getAccount [_ user pwd]
        (let [ pkey (.mm-g impl :appKey)
               ^SQLr sql (-> (dbio-connect (mkjdbc impl) AUTH-MCACHE {})
                             (.newSimpleSQLr)) ]
          (get-loginAccount sql user (pwdify pwd pkey))))
      (getRoles [_ acct] [])

      )))





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


