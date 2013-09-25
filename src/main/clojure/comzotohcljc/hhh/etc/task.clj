(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.etc.task )

(import '(org.apache.tools.ant.listener Log4jListener))
(import '(org.apache.tools.ant.taskdefs Javac))
(import '(org.apache.tools.ant.types
  Path DirSet))
(import '(org.apache.tools.ant
  Project Target Task))
(import '(java.io File))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn projAntTask "" [taskObj]
  (let [ pj (doto (Project.)
              (.setName "hhh-project")
              (.init))
         tg (doto (Target.)
              (.setName "z")) ]
    (doto pj
      (.addTarget tg)
      (.addBuildListener (Log4jListener.)))
    (doto taskObj
      (.setProject pj)
      (.setOwningTarget tg))
    (.addTask tg taskObj)
    pj))



(defn make-AntJavac "" []
  (let [ ct (Javac.)
         pj (projAntTask ct) ]
    (doto ct
      (.setTaskName "compile")
      (.setFork true)
      (.setDestdir destDir))

    (.setClassPath ct
      (doto (Path. pj) ()))

    (-> (.createSrc ct)
      (.addDirset (doto (DirSet.) (.setDir srcRoot))))

    pj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private task-eof nil)

