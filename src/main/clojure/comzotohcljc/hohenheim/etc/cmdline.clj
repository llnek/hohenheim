;;
;; COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
;;
;; THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
;; MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE
;; VERSION 2.0 (THE "LICENSE").
;;
;; THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL
;; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
;;
;; SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
;; AND LIMITATIONS UNDER THE LICENSE.
;;
;; You should have received a copy of the Apache License
;; along with this distribution; if not you may obtain a copy of the
;; License at
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hohenheim.etc.cmdline )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.hohenheim.etc CmdHelpError))
(import '(org.apache.commons.io FileUtils))
(import '(java.io File))
;;(import '(com.zotoh.hohenheim.core ))
;;(import '(org.apache.tools.ant Main))

(require '[comzotohcljc.hohenheim.core.climain :as CLI])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.cmdline :as CQ])
(require '[comzotohcljc.crypto.codec :as CE])
(require '[comzotohcljc.crypto.core :as CC])

(use '[comzotohcljc.hohenheim.core.constants])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *HOHENHEIM-RSBUNDLE* nil)
(def ^:dynamic *HOHENHEIM-HOME-DIR* "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- getHomeDir [] *HOHENHEIM-HOME-DIR*)
(defn- rcb [] *HOHENHEIM-RSBUNDLE*)

(defn- getBuildFilePath []
  (CU/nice-fpath (File. (File. (getHomeDir) (str DN_CFG "/app")) "ant.xml")))

(defn- runTarget [target]
  (org.apache.tools.ant.Main/main  (into-array String
      [ (str "-Dhohenheim_home=" (CU/nice-fpath (getHomeDir)))
        "-buildfile"
        (getBuildFilePath)
        ;; "-quiet"
        target ])))

(defn- runTargetExtra [target options]
  (let [ rc (persistent! (reduce (fn [sum en]
                      (conj! sum (str "-D" (first en) "=" (SU/nsb (last en) ))))
                 (transient []) (seq options))) ]
    (org.apache.tools.ant.Main/start
      (into-array String
        (-> rc
          (conj (str "-Dhohenheim_home=" (CU/nice-fpath (getHomeDir))) )
          (conj "-buildfile")
          (conj (getBuildFilePath))
          ;;(conj "-quiet")
          (conj target)) )
      nil
      (MU/get-cldr))))

(defn- onCreatePrompt []
  (let [ domain (-> (str "com." (SU/nsb (CU/getuser))) (.toLowerCase))
         q1 (CQ/make-CmdSeqQ "domain" "What is the application domain"
                       domain domain true
                       (fn [ans jps] (do (.put jps "app-domain" ans) "app" )))
         q2 (CQ/make-CmdSeqQ "app" "What is the application name"
                       "" "" true
                       (fn [ans jps] (do (.put jps "app-id" ans) "" )))
         ps (CQ/cli-converse {"domain" q1 "app" q2} "domain") ]
    [ (CU/notnil? ps) ps ] ))

(defn- onCreateApp [options & args]
  (let [ cb (fn [t ps] (runTargetExtra t ps)) ]
    (if (> (count args) 1)
      (case (nth args 1)
        "web/jetty" (apply cb "create-jetty" options)
        "web" (apply cb "create-web" options)
        (throw (CmdHelpError.)))
      (apply cb "create-app" options))))

(defn- onCreate [ & args]
  (if (< (count args) 1)
    (throw (CmdHelpError.))
    (let [ [ok x] (onCreatePrompt) ]
      (when ok
        (when (or (SU/nichts? (:app-domain x)) (SU/nichts? (:app-id x)))
          (throw (CmdHelpError.)))
        (apply onCreateApp x args)))))

(defn- onBuild [ & args]
  (if (>= (count args) 2)
    (runTargetExtra "build-app"
        { :app-id (nth args 1)
          :app-task (if (> (count args) 2) (nth args 2) "devmode") } )
    (throw (CmdHelpError.))))

(defn- onPodify [ & args]
  (if (> (count args) 1)
    (runTargetExtra "bundle-app" { :app-id (nth args 1) :app-task "release" })
    (throw (CmdHelpError.))))

(defn- onTest [ & args]
  (if (> (count args) 1)
    (runTargetExtra "test-code" { :app-id (nth args 1) })
    (throw (CmdHelpError.))))

(defn- onStart [ & args]
  (let [ s2 (if (> (count args) 1) (nth args 1) "") ]
    (cond
      (and (= s2 "bg") (CU/is-windows?))
      (runTarget "run-app-bg-w32")

      :else
      (CLI/start-main (CU/nice-fpath (getHomeDir))))))

(defn- onDebug [ & args]
  ;;runTarget( if (isWindows() ) "run-dbg-app-w32" else "run-dbg-app-nix")
  (onStart args))

(defn- onDemo [ & args]
  (if (> (count args) 1)
    (let [ s (nth args 1) ]
      (if (= "samples" s)
        (runTarget "create-samples")
        (runTargetExtra "create-demo" { :demo-id s})) )
    (throw (CmdHelpError.))))

(defn- generatePassword [] nil)
(defn- keyfile [] nil)
(defn- csrfile [] nil)

(defn- onGenerate [ & args]
  (if (> (count args) 1)
    (case (nth args 1)
      "password" (generatePassword)
      "serverkey" (keyfile)
      "csr" (csrfile)
      (throw (CmdHelpError.)))
    (throw (CmdHelpError.))))

(defn- encrypt [a b]
  (let [ c (CE/jasypt-cryptor)
         s (.encrypt c (CE/pwdify a) b) ]
    (println (str "CRYPT:" s))))

(defn- onEncrypt [ & args]
  (if (> (count args) 2)
    (encrypt  (nth args 1) (nth args 2))
    (throw (CmdHelpError.))))

(defn- decrypt [a b]
  (let [ c (CE/jasypt-cryptor)
         s (.decrypt c (CE/pwdify a) b) ]
    (println (str "CRYPT:" s))))


(defn- onDecrypt [ & args]
  (if (> (count args) 2)
    (decrypt (nth args 1) (nth args 2))
    (throw (CmdHelpError.))))

(defn- onTestJCE [ & args]
  (do
    (CC/assert-jce)
    (println "JCE is OK.")))

(defn- onVersion [ & args]
  (let [ s (FileUtils/readFileToString (File. (getHomeDir) "VERSION") "utf-8") ]
    (if (SU/hgl? s)
      (println s)
      (println "Unknown version."))))

(defn- onHelp [ & args]
  (throw (CmdHelpError.)))

(defn- scanJars [dir out]
  (let [ sep (System/getProperty "line.separator")
         fs (FileUtils/listFiles dir (into-array String ["jar"]) false) ]
    (doseq [ f (seq fs) ]
      (doto out
        (.append (str "<classpathentry  kind=\"lib\" path=\""
                      (CU/nice-fpath f) "\"/>" ))
        (.append sep)))))

(defn- genEclipseProj [app]
  (let [ cwd (File. (getHomeDir) (str DN_BOXX "/" app))
         ec (File. (CU/getcwd) "eclipse.projfiles")
         sb (StringBuilder.)
         lang "clojure"
         ulang (.toUpperCase lang) ]
    (.mkdirs ec)
    (FileUtils/cleanDirectory ec)
    (FileUtils/writeStringToFile (File. ec ".project")
      (-> (CU/rc-str (str "com/zotoh/hohenheim/eclipse/" lang "/project.txt") "utf-8")
          (StringUtils/replace "${APP.NAME}" app)
          (StringUtils/replace (str "${" ulang ".SRC}")
               (CU/nice-fpath (File. cwd (str "src/main/" lang))))
          (StringUtils/replace "${TEST.SRC}"
               (CU/nice-fpath (File. cwd (str "src/test/" lang)))))
      "utf-8")
    (scanJars (File. (getHomeDir) DN_DIST) sb)
    (scanJars (File. (getHomeDir) DN_LIB) sb)
    (scanJars (File. cwd POD_CLASSES) sb)
    (scanJars (File. cwd POD_LIB) sb)
    (FileUtils/writeStringToFile (File. ec ".classpath")
      (-> (CU/rc-str (str "com/zotoh/hohenheim/eclipse/" lang "/classpath.txt") "utf-8")
          (StringUtils/replace "${CLASS.PATH.ENTRIES}" (.toString sb)))
      "utf-8")))

(defn- onIDE [args]
  (if (> (count args) 2)
    (case (nth args 1)
      "eclipse" (genEclipseProj (nth args 2))
      (throw (CmdHelpError.)))
    (throw (CmdHelpError.))))


(def ^:private _ARGS {
  :create #'onCreate
  :ide #'onIDE
  :build #'onBuild
  :podify #'onPodify
  :test #'onTest
  :debug #'onDebug
  :start #'onStart
  :demo #'onDemo
  :generate #'onGenerate
  :encrypt #'onEncrypt
  :decrypt #'onDecrypt
  :testjce #'onTestJCE
  :version #'onVersion
  :help #'onHelp
  })


(defn eval-command "" [home rcb & args]
  (let [ v (get _ARGS (keyword (first args))) ]
    (when (nil? v)
      (throw (CmdHelpError.)))
    (binding [ *HOHENHEIM-HOME-DIR* home
               *HOHENHEIM-RSBUNDLE* rcb]
      (apply v args))))

(defn get-commands "" [] (set (keys _ARGS)))









(def ^:private cmdline-eof nil)


