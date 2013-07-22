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
  comzotohcljc.hohenheim.impl.jobcreator )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.wflow.core :as WC])

(import '(com.zotoh.hohenheim.core Job))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-job [event]
  (let [ impl (CU/make-mmap)
         jid (SN/next-long) ]
    (reify
      comzotohcljc.util.coreutils.MutableObjectAPI
      (setf! [_ k v] (.mm-s impl k v))
      (clear! [_] (.mm-c impl))
      (getf [_ k] (.mm-g impl k))
      (clrf! [_ k] (.mm-r impl k))
      Job
      (container [this] (getf this :container))
      (id [_] jid))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JobCreatorAPI
  (update [_ target & more ] ))

(deftype JobCreator
  [^comzotohcljc.hohenheim.impl.container.Container parent]
  JobCreatorAPI
  (update [event options]
    (let [ cz (if (.hasRouter event)
                (.routerClass event)
                (:router-class options))
           job (doto (make-job event)
                 (.setf! :container parent) ) ]
      (try
        (let [ p (WC/make-pipeline job cz)
               q (if (nil? p) (OrphanFlow. job) p) ]
          (.start q))
        (catch Throwable e#
          (-> (FatalErrorFlow. job) (.start)))))) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private jobcreator-eof nil)

