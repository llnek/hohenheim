(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.etc.task )

(import '(org.apache.tools.ant.listener TimestampedLogger))
(import '(org.apache.tools.ant.taskdefs Ant Zip ExecTask Javac))
(import '(org.apache.tools.ant.types
  FileSet Path DirSet))
(import '(org.apache.tools.ant
  Project Target Task))
(import '(java.io File))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn execProj [^Project pj] (.executeTarget pj "mi6"))

(defn projAntTask "" ^Project [^Task taskObj]
  (let [ pj (doto (Project.)
              (.setName "hhh-project")
              (.init))
         lg (doto (TimestampedLogger.)
              (.setOutputPrintStream System/out)
              (.setErrorPrintStream System/err)
              (.setMessageOutputLevel Project/MSG_INFO))
         tg (doto (Target.)
              (.setName "mi6")) ]
    (doto pj
      (.addTarget tg)
      (.addBuildListener lg))
    (doto taskObj
      (.setProject pj)
      (.setOwningTarget tg))
    (.addTask tg taskObj)
    pj))

(defn make-AntTask "" [^File hhhHome appId taskId]
  (let [ tk (Ant.) 
         pj (projAntTask tk) ]
    (doto tk
      (.setDir (File. hhhHome (str "apps/" appId)))
      (.setAntfile "build.xml")
      (.setTarget taskId)
      ;;(.setOutput "/tmp/out.txt")
      (.setUseNativeBasedir true)
      (.setInheritAll false))
    pj))

(defn make-ExecTask "" [^String execProg ^File workDir args]
  (let [ tk (ExecTask.)
         pj (projAntTask tk) ]
    (doto tk
      (.setTaskName "hhh-exec-task")
      (.setExecutable execProg)
      (.setDir workDir))
    (doseq [ v (seq args) ]
      (-> (.createArg tk)(.setValue v)))
    pj))

(defn make-ZipTask "" [^File srcDir ^File zipFile includes excludes]
  (let [ tk (Zip.)
         pj (projAntTask tk)
         fs   (doto (FileSet.)
              (.setDir srcDir)) ]
    (doseq [ s (seq excludes) ]
      (-> (.createExclude fs) (.setName s)))
    (doseq [ s (seq includes) ]
      (-> (.createInclude fs) (.setName s)))
    (doto tk
      (.add fs)
      (.setDestFile zipFile))
    pj))

(defn make-AntJavac "" [^File srcPath ^File destDir]
  (let [ ct (Javac.)
         pj (projAntTask ct) ]
    (doto ct
      (.setTaskName "compile")
      (.setFork true)
      (.setDestdir destDir))

    (.setClassPath ct (Path. pj))

    (-> (.createSrc ct)
      (.addDirset (doto (DirSet.) (.setDir srcPath))))

    pj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private task-eof nil)

