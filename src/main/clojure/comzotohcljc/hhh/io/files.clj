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

  comzotohcljc.hhh.io.files )


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
(import '(com.zotoh.frwk.core Identifiable))


(use '[comzotohcljc.hhh.core.sys :rename { seq* rego-seq* has? rego-has? } ])
(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.hhh.io.loops
       :only (loopable-schedule loopable-oneloop cfg-loopable) ])
(use '[comzotohcljc.hhh.io.core])

(use '[comzotohcljc.util.seqnum :as SN])
(use '[comzotohcljc.util.core :as CU])
(use '[comzotohcljc.util.str :as SU])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)



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
      (reify
        Identifiable
        (id [_] eeid)
        FileEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (emitter [_] co)
        (getFile [_] f))
      { :typeid :czc.hhh.io/FileEvent } )))

(defn- postPoll [^comzotohcljc.hhh.core.sys.Thingy co
                 ^File f
                 action]
  (let [ ^File des (.getAttr co :dest)
        ^comzotohcljc.hhh.io.core.EmitterAPI src co
         fname (.getName f)
         cf (cond
              (= action :FP-CREATED)
              (if (CU/notnil? des)
                (CU/TryC
                  (FileUtils/moveFileToDirectory f des false)
                  (File. des fname) )
                f)
              :else nil) ]
    (when-not (nil? cf)
      (.dispatch src (ioes-reify-event co fname cf action)))) )


(defmethod comp-configure :czc.hhh.io/FilePicker
  [^comzotohcljc.hhh.core.sys.Thingy co cfg]
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
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ obs (FileAlterationObserver.
               ^File (.getAttr co :target)
               ^FileFilter (.getAttr co :mask))
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
    (info "FilePicker's apache io monitor created - OK.")
    co))


(defmethod loopable-schedule :czc.hhh.io/FilePicker
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^FileAlterationMonitor mon (.getAttr co :monitor) ]
    (info "FilePicker's apache io monitor starting...")
    (.start mon)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private files-eof nil)

