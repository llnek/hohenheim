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

;;(use '[comzotohcljc.wflow.core :only (FatalErrorFlow OrphanFlow) ])
(use '[comzotohcljc.util.coreutils :only (MutableObjectAPI) ])
(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.util.metautils :as MU])
(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.seqnumgen :as SN])
(require '[comzotohcljc.wflow.core :as WC])

(import '(com.zotoh.hohenheim.core Job))



(deftype FatalErrorFlow [job])
(deftype OrphanFlow [job])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-job "" [kontainer evt]
  (let [ impl (CU/make-mmap)
         jid (SN/next-long) ]
    (with-meta
      (reify

        MutableObjectAPI

          (setf! [_ k v] (.mm-s impl k v))
          (clear! [_] (.mm-c impl))
          (seq* [_] (seq (.mm-m impl)))
          (getf [_ k] (.mm-g impl k))
          (clrf! [_ k] (.mm-r impl k))

        Job

          (container [_] kontainer)
          (event [_] evt)
          (id [_] jid))

      { :typeid :Job } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JobCreatorAPI
  (update [_ event options] ))

(defn make-jobcreator "" [parObj]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        JobCreatorAPI

          (update [_ evt options]
            (let [ cz (if (.hasRouter evt)
                        (.routerClass evt)
                        (:router-class options))
                   job (make-job parObj evt) ]
              (try
                (let [ p (WC/make-pipeline job cz)
                       q (if (nil? p) (OrphanFlow. job) p) ]
                  (.start q))
                (catch Throwable e#
                  (-> (FatalErrorFlow. job) (.start)))))))

      { :typeid :JobCreator } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:private jobcreator-eof nil)

