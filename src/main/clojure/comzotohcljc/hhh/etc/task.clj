(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.etc.task )

(import '(org.apache.tools.ant.listener AnsiColorLogger Log4jListener))
(import '(org.apache.tools.ant.taskdefs Ant ExecTask Javac))
(import '(org.apache.tools.ant.types
  Path DirSet))
(import '(org.apache.tools.ant
  Project Target Task))
(import '(java.io File))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn execProj [^Project pj] (.executeTarget pj "z"))

(defn projAntTask "" ^Project [^Task taskObj]
  (let [ pj (doto (Project.)
              (.setName "hhh-project")
              (.init))
         tg (doto (Target.)
              (.setName "z")) ]
    (doto pj
      (.addTarget tg))
      ;;(.addBuildListener (AnsiColorLogger.)))
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
      (.setOutput "/tmp/poo.txt")
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

