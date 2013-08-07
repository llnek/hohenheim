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

  comzotohcljc.hhh.etc.cmdline )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.apache.commons.lang3 StringUtils))
(import '(com.zotoh.hohenheim.etc CmdHelpError))
(import '(org.apache.commons.io FileUtils))
(import '(java.util Calendar ResourceBundle Properties Date))
(import '(java.io File))

(require '[comzotohcljc.hhh.core.climain :as CLI])
(require '[comzotohcljc.i18n.resources :as RC])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.dates :as DU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.cmdline :as CQ])
(require '[comzotohcljc.crypto.codec :as CE])
(require '[comzotohcljc.crypto.core :as CC])

(use '[comzotohcljc.hhh.core.constants])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *HOHENHEIM-RSBUNDLE* nil)
(def ^:dynamic *HOHENHEIM-HOME-DIR* "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- rcb ^ResourceBundle [] *HOHENHEIM-RSBUNDLE*)
(defn- getHomeDir ^File [] *HOHENHEIM-HOME-DIR*)

(defn- getBuildFilePath ^String []
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
                      (conj! sum (str "-D" (name (first en)) "=" (SU/nsb (last en) ))))
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
                       (fn [ans ^Properties jps] (do (.put jps "hohenheim.app.domain" ans) "app" )))
         q2 (CQ/make-CmdSeqQ "app" "What is the application name"
                       "" "" true
                       (fn [ans ^Properties jps] (do (.put jps "hohenheim.appid" ans) "" )))
         ps (CQ/cli-converse {"domain" q1 "app" q2} "domain") ]
    [ (CU/notnil? ps) ps ] ))

(defn- onCreateApp [options & args]
  (if (> (count args) 1)
    (case (nth args 1)
      "web/jetty" (runTargetExtra "create-jetty" options)
      "web" (runTargetExtra "create-web" options)
      (throw (CmdHelpError.)))
    (runTargetExtra "create-app" options)))

(defn- onCreate [ & args]
  (if (< (count args) 1)
    (throw (CmdHelpError.))
    (let [ [ok x] (onCreatePrompt) ]
      (when ok
        (when (or (SU/nichts? (:hohenheim.app.domain x)) (SU/nichts? (:hohenheim.appid x)))
          (throw (CmdHelpError.)))
        (apply onCreateApp x args)))))

(defn- onBuild [ & args]
  (if (>= (count args) 2)
    (runTargetExtra "build-app"
        { :hohenheim.appid (nth args 1)
          :hohenheim.app.task (if (> (count args) 2) (nth args 2) "devmode") } )
    (throw (CmdHelpError.))))

(defn- onPodify [ & args]
  (if (> (count args) 1)
    (runTargetExtra "bundle-app" { :hohenheim.appid (nth args 1) :hohenheim.app.task "release" })
    (throw (CmdHelpError.))))

(defn- onTest [ & args]
  (if (> (count args) 1)
    (runTargetExtra "test-code" { :hohenheim.appid (nth args 1) })
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
        (runTargetExtra "create-demo" { :demo.id s})) )
    (throw (CmdHelpError.))))

(defn- generatePassword [len]
  (println (.text (CE/create-strong-pwd len))))

(defn- make-csr-qs [^ResourceBundle rcb]
  { "fname"
    (CQ/make-CmdSeqQ "fname" (RC/get-string rcb "cmd.save.file")
                   "" "csr-req" true
                   (fn [a ^Properties ps] (do (.put ps "fn" a) "")))

    "size"
    (CQ/make-CmdSeqQ "size" (RC/get-string rcb "cmd.key.size")
                   "" "1024" true
                   (fn [a ^Properties ps] (do (.put ps "size" a) "fname")))
    "c"
    (CQ/make-CmdSeqQ "c" (RC/get-string rcb "cmd.dn.c")
                   "" "US" true
                   (fn [a ^Properties ps] (do (.put ps "c" a) "size")))

    "st"
    (CQ/make-CmdSeqQ "st" (RC/get-string rcb "cmd.dn.st")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "st" a) "c")))

    "loc"
    (CQ/make-CmdSeqQ "loc" (RC/get-string rcb "cmd.dn.loc")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "l" a) "st")))

    "o"
    (CQ/make-CmdSeqQ "o" (RC/get-string rcb "cmd.dn.org")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "o" a) "loc")))

    "ou"
    (CQ/make-CmdSeqQ "ou" (RC/get-string rcb "cmd.dn.ou")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "ou" a) "o")))

    "cn"
    (CQ/make-CmdSeqQ "cn" (RC/get-string rcb "cmd.dn.cn")
                   "" "" true
                   (fn [a ^Properties ps] (do (.put ps "cn" a) "ou")))
  } )

(defn- make-key-qs [^ResourceBundle rcb]

  {
    "fname"
    (CQ/make-CmdSeqQ "fname" (RC/get-string rcb "cmd.save.file")
                     "" "test.p12" true
                     (fn [a ^Properties ps] (do (.put ps "fn" a) "")))

    "pwd"
    (CQ/make-CmdSeqQ "pwd" (RC/get-string rcb "cmd.key.pwd")
                     "" "" true
                     (fn [a ^Properties ps] (do (.put ps "pwd" a) "fname")))

    "duration"
    (CQ/make-CmdSeqQ "duration" (RC/get-string rcb "cmd.key.duration")
                     "" "12" true
                     (fn [a ^Properties ps] (do (.put ps "months" a) "pwd")))

    "size"
    (CQ/make-CmdSeqQ "size" (RC/get-string rcb "cmd.key.size")
                   "" "1024" true
                   (fn [a ^Properties ps] (do (.put ps "size" a) "duration")))

   } )


(defn- keyfile []
  (let [ csr (make-csr-qs *HOHENHEIM-RSBUNDLE*)
         k (merge csr (make-key-qs *HOHENHEIM-RSBUNDLE*))
         rc (CQ/cli-converse k "cn") ]
    (when-not (nil? rc)
      (let [ dn (clojure.string/join "," (CU/flatten-nil (map (fn [k]
                                   (let [ v (get rc k) ]
                                     (if (SU/hgl? v)
                                      (str (.toUpperCase (name k)) "=" v)
                                     nil)))
                                   [ :c :st :l :o :ou :cn ])) )
             ff (File. (:fn rc))
             now (Date.) ]
        (println (str "DN entered: " dn))
        (CC/make-ssv1PKCS12
          now
          (.getTime (DU/add-months (DU/make-cal now) (CU/conv-long (:months rc) 12)))
          dn
          (CE/pwdify (:pwd rc))
          (CU/conv-long (:size rc) 1024)
          ff)
        (println (str "Wrote file: " ff))))))


        (defn- csrfile []
  (let [ csr (make-csr-qs *HOHENHEIM-RSBUNDLE*)
         rc (CQ/cli-converse csr "cn") ]
    (when-not (nil? rc)
      (let [ dn (clojure.string/join "," (CU/flatten-nil (map (fn [k]
                                   (let [ v (get rc k) ]
                                     (if (SU/hgl? v)
                                      (str (.toUpperCase (name k)) "=" v)
                                     nil)))
                                   [ :c :st :l :o :ou :cn ])) )
             [req pkey] (CC/make-csrreq
                          (CU/conv-long (:size rc) 1024)
                          dn
                          CC/*PEM_CERT* ) ]
        (println (str "DN entered: " dn))
        (let [ ff (File. (str (:fn rc) ".key")) ]
          (FileUtils/writeByteArrayToFile ff pkey)
          (println (str "Wrote file: " ff)))
        (let [ ff (File. (str (:fn rc) ".csr")) ]
          (FileUtils/writeByteArrayToFile ff req)
          (println (str "Wrote file: " ff))) ))))

(defn- onGenerate [ & args]
  (let [ ok (if (> (count args) 1)
              (case (nth args 1)
                "password" (do (generatePassword 12) true)
                "serverkey" (do (keyfile) true)
                "csr" (do (csrfile) true)
                false)
              false) ]
    (when-not ok
      (throw (CmdHelpError.)))))

(defn- genHash [text]
  (let [ p (CE/pwdify text) ]
    (println (.hashed p))))

(defn- onHash [ & args]
  (if (> (count args) 1)
    (genHash (nth args 1))
    (throw (CmdHelpError.))))

(defn- encrypt [pkey text]
  (let [ p (CE/pwdify text pkey) ]
    (println (.encoded p))))

(defn- onEncrypt [ & args]
  (if (> (count args) 2)
    (encrypt  (nth args 1) (nth args 2))
    (throw (CmdHelpError.))))

(defn- decrypt [pkey secret]
  (let [ p (CE/pwdify secret pkey) ]
    (println (.text p))))

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
         lang "scala"
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
  :hash #'onHash
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


