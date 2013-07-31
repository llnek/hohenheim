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

  comzotohcljc.hohenheim.etc.misc )

(use '[comzotohcljc.wflow.core])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal flows

(defn- make-internal-flow [s]
  (fn [_]
    (make-ptask
      (fn [fw job arg]
        (let [ ev (.event job) ]
          (cond
            (satisfies? HttpEventAPI ev)
            (.setResult ev (doto (make-http-result) (.setStatus s)))
            :else
            (throw (FlowError.  (str "Unhandled event-type \"" (type ev) "\".")))))))))

(deftype FatalErrorFlow [] PipelineDelegateAPI
  (getStart [_] (make-internal-flow HTTPStatus/INTERNAL_SERVER_ERROR))
  (getStop [_] nil)
  (getError [_] nil))

(deftype OrphanFlow [] PipelineDelegateAPI
  (getStart [_] (make-internal-flow HTTPStatus/NOT_IMPLEMENTED))
  (getStop [_] nil)
  (getError [_] nil))


(defn- make-FatalErrorFlow [job]
  (make-pipeline job "comzotohcljc.hohenheim.etc.misc.FatalErrorFlow"))

(defn- make-OrphahFlow [job]
  (make-pipeline job "comzotohcljc.hohenheim.etc.misc.OrphanFlow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private misc-eof nil)

