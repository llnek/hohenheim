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
  (println (SU/make-string \= 78))
  )


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
  ;;(alter-var-root #'*read-eval* (constantly false))
  (let [ rcpath (str "comzotohcljc/hhh/etc/Resources")
         rcb (LU/get-resource rcpath (Locale/getDefault)) ]
    (if (< (count args) 2)
      (usage)
      (let [ rc (apply parseArgs rcb args) ]
        (if (fn? rc)
          (rc)
          (usage))))))






(def ^:private core-eof nil)

