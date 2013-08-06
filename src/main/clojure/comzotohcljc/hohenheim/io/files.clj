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

  comzotohcljc.hohenheim.io.files )


(import '(java.io FileFilter File FilenameFilter IOException))
(import '(org.apache.commons.lang3 StringUtils))
(import '(java.util Properties ResourceBundle))
(import '(org.apache.commons.io.filefilter
  SuffixFileFilter
  PrefixFileFilter
  RegexFileFilter
  FileFileFilter))

(import '(org.apache.commons.io FileUtils))
(import '(org.apache.commons.io.monitor
 FileAlterationListener
 FileAlterationListenerAdaptor
 FileAlterationMonitor
 FileAlterationObserver))
(import '(com.zotoh.hohenheim.io FileEvent))



(use '[comzotohcljc.hohenheim.core.sys :rename { seq* rego-seq* has? rego-has? } ])
(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.hohenheim.io.loops :only (loopable-oneloop cfg-loopable) ])
(use '[comzotohcljc.hohenheim.io.events :only (make-filepicker-event) ])
(use '[comzotohcljc.hohenheim.io.core])

(use '[comzotohcljc.util.seqnum :as SN])
(use '[comzotohcljc.util.core :as CU])
(use '[comzotohcljc.util.str :as SU])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! *warn-on-reflection* false)



(def FP_CREATED :FP-CREATED )
(def FP_CHANGED :FP-CHANGED )
(def FP_DELETED :FP-DELETED )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FilePicker

(defn make-filepicker [container]
  (make-emitter container :czc.hhh.io/FilePicker))

(defmethod ioes-reify-event :czc.hhh.io/FilePicker
  [co & args]
  (let [ f (nth args 1)  eeid (SN/next-long) ]
    (with-meta 
      (reify FileEvent
        (getSession [_] nil)
        (emitter [_] co)
        (getId [_] eeid)
        (getFile [_] f))
      { :typeid :czc.hhh.io/FileEvent } )))

(defn- postPoll [^comzotohcljc.hohenheim.core.sys.Component co
                 ^File f 
                 action]
  (let [ ^File des (.getAttr co :dest)
        ^comzotohcljc.hohenheim.io.core.EmitterAPI src co
         fname (.getName f)
         cf  (if (and (not= action :FP-DELETED) (CU/notnil? des))
                (CU/TryC
                    (FileUtils/moveFileToDirectory f des false)
                    (File. des fname) )
                f) ]
    (.dispatch src (ioes-reify-event co fname cf action))))


(defmethod comp-configure :czc.hhh.io/FilePicker
  [^comzotohcljc.hohenheim.core.sys.Component co cfg]
  (let [ ^String root (CU/subs-var (SU/nsb (:target-folder cfg)))
         ^String dest (CU/subs-var (SU/nsb (:recv-folder cfg)))
         ^String mask (SU/nsb (:fmask cfg)) ]
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

(defmethod comp-initialize :czc.hhh.io/FilePicker
  [^comzotohcljc.hohenheim.core.sys.Component co]
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


(defmethod loopable-oneloop :czc.hhh.io/FilePicker
  [^comzotohcljc.hohenheim.core.sys.Component co]
  (let [ ^FileAlterationMonitor mon (.getAttr co :monitor) ]
    (.start mon)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(derive :czc.hhh.io/FilePicker :czc.hhh.io/ThreadedTimer)


(def ^:private files-eof nil)

