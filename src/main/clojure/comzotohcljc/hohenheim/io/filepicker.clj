

(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hohenheim.io.filepicker )


(import '(org.apache.commons.lang3. StringUtils))
(import '(java.io. FileFilter File FilenameFilter IOException))
(import '(java.util Properties ResourceBundle))
(import '(org.apache.commons.io.filefilter ))
(import '(org.apache.commons.io. FileUtils))
(import '(org.apache.commons.io.monitor
 FileAlterationListener
 FileAlterationListenerAdaptor
 FileAlterationMonitor
 FileAlterationObserver))

(def FP_CREATED :FP-CREATED )
(def FP_CHANGED :FP-CHANGED )
(def FP_DELETED :FP-DELETED )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FilePicker

    ;;tlog().debug("{} : {} was {}" , "FilePicker", f, action)
(defn- postPoll [f action]
  (let [ des (.getAttr co :dest)
         fname (.getName f)
         cf  (if (and (not= action :FP-DELETED) (CU/notnil? des))
                (try
                    (FileUtils/moveFileToDirectory f des false)
                    (File. des fname)
                  (catch Throwable e# (warn e#) nil))
                f) ]
    (.dispatch co nil);;FILEEvent(this, fn, cf, action))
    ))


(defmethod comp-configure ::FilePicker [co cfg]
  (let [ root (CU/subs-var (SU/nsb (:target-folder cfg)))
         dest (CU/subs-var (SU/nsb (:recv-folder cfg)))
         mask (SU/nsb (:fmask cfg)) ]
    (cfg-loopable co cfg)
    (CU/test-nestr "file-root-folder" root)
    (.setAttr! co :target (doto (File. root) (.mkdirs)))
    (.setAttr! co :mask 
      (cond
        (.startsWith mask "*.")
        (SuffixFileFilter. (.substring mask 1))

        (.endsWith mask "*")
        (PrefixFileFilter. (.substring mask 0 (dec (.length mask))))

        (> (.length mask) 0)
        (RegexFileFilter. mask) ;;WildcardFileFilter(mask)

        :else
        FileFileFilter/FILE ) )
    (when (SU/hgl? dest)
      (.setAttr! co :dest (doto (File. dest) (.mkdirs))))

    (info "Monitoring folder: " root)
    (info "Recv folder: " (SU/nsn dest))

    co))

(defmethod comp-initialize ::FilePicker [co]
  (let [ obs (FileAlterationObserver.
               (.getAttr co :target)
               (.getAttr co :mask))
         intv (.getAttr co :intervalMillis)
         mon (FileAlterationMonitor. intv)
         lnr (proxy [FileAlterationListenerAdaptor][]
               (onFileCreate [f]
                 (postPoll co f :FP-CREATED))
               (onFileChange [f]
                 (postPoll co f :FP-CHANGED))
               (onFileDelete [f] 
                 (postPoll co f :FP-DELETED))) ]
    (.addListener obs lnr)
    (.addObserver mon obs)
    (.setAttr! co :monitor mon)
    co))


(defmethod loopable-wakeup ::FilePicker [co]
  (-> (.getAttr co :monitor) (.start)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private filepicker-eof nil)

