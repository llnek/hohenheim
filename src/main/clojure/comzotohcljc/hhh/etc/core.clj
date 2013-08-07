;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE,
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not, you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hhh.etc.core
  (:gen-class))

(use '[clojure.tools.logging :only (info debug)])

(require '[comzotohcljc.hhh.etc.cmdline :as CL])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.files :as FU])
(require '[comzotohcljc.i18n.resources :as LU])

(import '(com.zotoh.hohenheim.etc CmdHelpError))
(import '(java.util Locale))
(import '(java.io File))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)


(def ^:private CMDLINE-INFO [
  ["create web[/jetty]"  "e.g. create app as a webapp."]
  ["create"  "e.g. create an app."]
  ["podify <app-name>"  "e.g. package app as a pod file"]

  ["ide eclipse <app-name>" "Generate eclipse project files."]
  ["build <app-name> [target]" "Build app."]
  ["test <app-name>" "Run test cases."]

  ["debug" "Start & debug the application."]
  ["start [bg]" "Start the application."]

  ["generate serverkey" "Create self-signed server key (pkcs12)."]
  ["generate password" "Generate a random password."]
  ["generate csr" "Create a Certificate Signing Request."]
  ["encrypt <password> <clear-text>" "e.g. encrypt SomeSecretData"]
  ["decrypt <password> <cipher-text>" "e.g. decrypt Data"]
  ["hash <password>" "e.g. hash SomePassword"]
  ["testjce" "Check JCE  Policy Files."]

  ["demo samples" "Generate a set of samples."]
  ["version" "Show version info."] ])

(defn- drawHelpLines [^String fmt ^clojure.lang.IPersistentCollection arr]
  (doseq [ [k v] (seq arr) ]
    (print (String/format fmt (into-array Object [k v]) ))))

(defn- usage []
  (println (SU/make-string \= 78))
  (println "> hohenheim <commands & options>")
  (println "> -----------------")
  (drawHelpLines "> %-35s %s\n" CMDLINE-INFO)
  (println ">")
  (println "> help - show standard commands")
  (println (SU/make-string \= 78)) )


;; arg(0) is hohenheim-home
;;println("#### apprunner loader = " + getClass().getClassLoader().getClass().getName())
;;println("#### sys loader = " + ClassLoader.getSystemClassLoader().getClass().getName())
;;mkCZldrs(home)
(defn- parseArgs [rcb & args]
  (let [ h (File. ^String (first args)) ]
    (CU/test-cond (str "Cannot access Hohenheim home " h) (FU/dir-read? h))
      (if (not (contains? (CL/get-commands) (keyword (nth args 1))))
        false
        (fn [] (apply CL/eval-command h rcb (drop 1 args))))))

(defn -main "Main Entry" [& args]
  ;; for security, don't just eval stuff
  (alter-var-root #'*read-eval* (constantly false))
  (let [ rcpath (str "comzotohcljc/hohenheim/etc/Resources")
         rcb (LU/get-resource rcpath (Locale/getDefault)) ]
    (if (< (count args) 2)
      (usage)
      (let [ rc (apply parseArgs rcb args) ]
        (if (fn? rc)
          (rc)
          (usage))))))






(def ^:private core-eof nil)

