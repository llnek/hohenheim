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

(require '[comzotohcljc.hhh.auth.dms :as DM])
(require '[comzotohcljc.dbio.core :as DB])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-role [^SQLr db role desc]
  (.insert db { :name (SU/nsb role) :desc (SU/nsb desc) } ))

(defn remove-role [^SQLr db role]
  (.delete db (SU/nsb role)))

(defn list-roles [^SQLr db]
  (.findAll db :AuthRole))

(defn create-account  ""
  [^SQLr db user ^comzotohcljc.crypto.codec.Password pwdObj roles]
  (let [ v1 { :acctid (SU/nsb user) :passwd (.hashed pwdObj) }
         v2  (.insert db v1) ]
    v2))

(defn get-account  ""
  [^SQLr db ^String user ^comzotohcljc.crypto.codec.Password pwdObj]
  (let [ acct (.findOne db :LoginAccount { :acctid user } ) ]
    (cond
      (nil? acct)
      (throw (UnknownUser. user))

      (= (.hashed pwdObj) (:passwd acct))
      acct

      :else
      (throw (AuthError. "Incorrect password"))) ))


(defn update-account [^SQLr db user details]
  )

(defn remove-account [^SQLr db user]
  (.delete db (SU/nsb user)))

(defn list-accounts [^SQLr db]
  (.findAll db :LoginAccount))


(defn make-plugin ^Plugin []
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
               j (DB/make-jdbc "x" cfg (CE/pwdify (:passwd cfg) pkey)) ]
            (DM/apply-ddl j)))
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
    (make-plugin)))

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
      (let [ j (DB/make-jdbc db cfg (CE/pwdify (:passwd cfg) pkey))
             t (DB/match-jdbc-url (SU/nsb (:url cfg))) ]
        (cond
          (= "init-db" cmd)
          (let []
            (DM/apply-ddl j))

          (= "gen-sql" cmd)
          (if (> (count args) 3)
            (DM/export-ddl t (File. ^String (nth args 3))))

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


