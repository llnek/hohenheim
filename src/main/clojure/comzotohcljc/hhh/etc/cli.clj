(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.etc.tools )

(import '(org.apache.commons.io.filefilter FileFileFilter FileFilterUtils))
(import '(org.apache.commons.io FilenameUtils FileUtils))
(import '(java.util UUID))
(import '(java.io File))

(require '[comzotohcljc.util.files :as FS])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- copy-files [^File srcDir ^File destDir ext]
  (FileUtils/copyDirectory
    srcDir
    destDir
    (FileFilterUtils/andFileFilter FileFileFilter/FILE
                                   (FileFilterUtils/suffixFileFilter (str "." ext)))))

(defn cleanAppClasses "" [webzDir czDir]
  (FileUtils/cleanDirectory webzDir)
  (FileUtils/cleanDirectory czDir)
  (-> (File. czDir)(.mkdirs )) )

(defn createDemo "" [^File hhhHome demoId]
  (let [ fp (File. hhhHome (str "docs/samples/" demoId ".pod"))
         dest (File. hhhHome (str "apps/demo-" demoId)) ]
    (when (.exists fp)
      (.mkdirs dest)
      (FS/unzip fp dest))))

(defn createSamples "" [^File hhhHome]
  (let [ top (File. hhhHome (str "docs/samples"))
         fs (FileUtils/listFiles top (into-array String ["pod"]) false) ]
    (doseq [ ^File f (seq fs) ]
      (createDemo hhhHome (FilenameUtils/getBaseName (.toString f))))))

(defn post-create-app "" [^File hhhHome appId appDomain]
  (let [ appDir (File. hhhHome (str "apps/" appId))
         appDomainPath (.replace appDomain "." "/") ]

    (let [ fp (File. appDir (str "src/main/clojure/" appDomainPath "/core.clj")) ]
      (FileUtils/writeStringToFile fp
        (-> (FileUtils/readFileToString fp "utf-8")
          (StringUtils/replace @@APPDOMAIN@@ appDomain))
                                   "utf-8"))

    (let [ fp (File. appDir (str "src/main/clojure/" appDomainPath "/pipe.clj")) ]
      (FileUtils/writeStringToFile fp
        (-> (FileUtils/readFileToString fp "utf-8")
          (StringUtils/replace @@APPDOMAIN@@ appDomain))
                                   "utf-8"))

    (let [ fp (File. appDir "conf/env.conf") ]
      (FileUtils/writeStringToFile fp
        (-> (FileUtils/readFileToString fp "utf-8")
          (StringUtils/replace @@APPDOMAIN@@ appDomain))
                                   "utf-8"))

    (let [ fp (File. appDir "build.xml") ]
      (FileUtils/writeStringToFile fp
        (-> (FileUtils/readFileToString fp "utf-8")
          (StringUtils/replace @@APPCLJFILES@@ (str "<arg value=\""
                                                    appDomain
                                                    ".core\"/>"
                                                    "<arg value=\""
                                                    appDomain
                                                    ".pipe\"/>" ))
          (StringUtils/replace @@APPID@@ appId))
                                   "utf-8"))
    ))


(defn create-app-common "" [^File hhhHome appId appDomain]
  (let [ appDir (doto (File. hhhHome (str "apps/" appId)) (.mkdirs))
         mfDir (doto (File. appDir "META-INF")(.mkdirs))
         appDomainPath (.replace appDomain "." "/") ]
    (doseq [ s ["classes" "patch" "lib"]]
      (-> (File. appDir (str "POD-INF/" s)) (.mkdirs)))
    (doseq [ s ["RELEASE-NOTES.txt" "NOTES.txt" "LICENSE.txt" "README.md"]]
      (FileUtils/touch (File. mfDir ^String s)))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/MANIFEST.MF")
                                   mfDir)
    (-> (File. appDir "modules")(.mkdirs))
    (-> (File. appDir "conf")(.mkdirs))
    (-> (File. appDir "docs")(.mkdirs))

    (doseq [ s ["app.conf" "env.conf" "shiro.ini"]]
      (FileUtils/copyFileToDirectory (File. hhhHome (str "etc/app" s))
                                     (File. appDir "conf")))
    (let [ fp (File. appDir "conf/app.conf") ]
      (FileUtils/writeStringToFile
        (-> (FileUtils/readFileToString fp "utf-8")
          (StringUtils/replace "@@USER@@" (System/getProperty "user.name")))
        "utf-8"))

    (doseq [ s [ "scala" "java" (str "clojure/" appDomainPath) "resources" ]]
      (-> (File. appDir (str "src/main/" s)) (.mkdirs))
      (-> (File. appDir (str "src/test/" s)) (.mkdirs)))

    (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/core.clj")
                                   (File. appDir (str "src/main/clojure/" appDomainPath)))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/pipe.clj")
                                   (File. appDir (str "src/main/clojure/" appDomainPath)))

    (doseq [ s ["build.properties" "ivysettings.xml" "ivy.xml" "pom.xml"]]
      (FileUtils/copyFileToDirectory (File. hhhHome (str "etc/app/" s))
                                     appDir))

    (let [ mf (File. mfDir "MANIFEST.MF") ]
      (FileUtils/writeStringToFile mf
        (-> (FileUtils/readFileToString mf "utf-8")
          (StringUtils/replace @@APPKEY@@ (UUID/randomUUID))
          (StringUtils/replace @@APPMAINCLASS@@  (str appDomain ".core.MyAppMain")))
                                   "utf-8"))

    (let [ pom (File. appDir "pom.xml") ]
      (FileUtils/writeStringToFile pom
        (-> (FileUtils/readFileToString pom "utf-8")
          (StringUtils/replace @@APPDOMAIN@@ appDomain)
          (StringUtils/replace @@APPID@@ appId))
                                   "utf-8"))

    (let [ pom (File. appDir "ivy.xml") ]
      (FileUtils/writeStringToFile pom
        (-> (FileUtils/readFileToString pom "utf-8")
          (StringUtils/replace @@APPDOMAIN@@ appDomain)
          (StringUtils/replace @@APPID@@ appId))
                                   "utf-8"))

    (let [ bp (File. appDir "build.properties") ]
      (FileUtils/writeStringToFile bp
        (-> (FileUtils/readFileToString bp "utf-8")
          (StringUtils/replace @@HOHENHEIMHOME@@ (.getCanonicalPath hhhHome)))
                                   "utf-8"))
    ))


(defn createApp "" [^File hhhHome appId appDomain]
  (create-app-common hhhHome appId appDomain)
  (FileUtils/copyFileToDirectory (File. hhhHome "etc/app/build.xml")
                                 (File. hhhHome (str "apps/" appId)))
  (post-create-app hhhHome appId))


(defn create-web-common "" [^File hhhHome appId appDomain]
  (let [ appDir (File. hhhHome (str "apps/" appId)) ]
    (doseq [ s ["coffee" "js" "less"]]
      (-> (File. appDir (str "src/main/resources/" s)) (.mkdirs)))
    (doseq [ s ["images" "scripts" "styles"]]
      (-> (File. appDir (str "public/" s)) (.mkdirs)))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/favicon.png")
                                   (File. appDir "public/images"))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/pipe.clj")
                                   (File. appDir (str "src/main/clojure/" appDomainPath)))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/build.xml")
                                   appDir)
    (FileUtils/copyDirectory (File. hhhHome "etc/weblibs")
                             (File. appDir "public"))
    ))

(defn createJetty "" [^File hhhHome appId appDomain]
  (let [ appDir (File. hhhHome (str "apps/" appId)) ]
    (create-app-common hhhHome appId appDomain)
    (create-web-common hhhHome appId appDomain)
    (doseq [ s [ "classes" "lib" ]]
      (-> (File. appDir (str "WEB-INF/" s)) (.mkdirs)))
    (FileUtils/copyFile (File. hhhHome "etc/jetty/jetty.conf")
                        (File. appDir "conf/env.conf"))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/jetty/web.xml")
                                   (File. appDir "WEB-INF"))
    (post-create-app)))

(defn createWeb "" [^File hhhHome appId appDomain]
  (let [ appDir (File. hhhHome "apps") ]
    (create-app-common hhhHome appId appDomain)
    (create-web-common hhhHome appId appDomain)
    (copy-files (File. hhhHome "etc/netty") (File. appDir "conf") "conf")
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/static-routes.conf")
                                   (File. appDir "conf"))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/routes.conf")
                                   (File. appDir "conf"))
    (FileUtils/copyFileToDirectory (File. hhhHome "etc/web/favicon.png")
                                   (File. appDir "public/images"))
    (doseq [ s ["errors" "views"]]
      (-> (File. appDir (str "pages/" s)) (.mkdirs)))

    (copy-files (File. hhhHome "etc/netty") (File. appDir "pages/views") "ftl")
    (copy-files (File. hhhHome "etc/netty") (File. appDir "pages") "html")

    (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/index.html")
                                   (File. appDir "public"))

    (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/main.less")
                                   (File. appDir "src/main/resources/less"))

    (let [ fp (File. appDir "conf/routes-conf") ]
      (FileUtils/writeStringToFile fp
        (-> (FileUtils/readFileToString fp "utf-8")
          (StringUtils/replace @@APPDOMAIN@@ appDomain))
                                   "utf-8"))

    (FileUtils/copyFileToDirectory (File. hhhHome "etc/netty/pipe.clj")
                                   (File. appDir (str "src/main/clojure/" appDomainPath)))

    (post-create-app hhhHome appId appDomain) ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private tools-eof nil)

