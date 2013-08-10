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

  comzotohcljc.hhh.etc.misc )

(import '(com.zotoh.wflow.core FlowError))
(import '(com.zotoh.wflow Pipeline PipelineDelegate PTask Work))
(import '(com.zotoh.hohenheim.io IOEvent HTTPEvent HTTPResult))
(import '(com.zotoh.frwk.core Startable))

(require '[comzotohcljc.net.comms :as NC])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal flows

(defn- make-work [s]
  (reify Work
             (perform [_ cur job arg]
               (let [ ^HTTPEvent evt (.event job)
                      ^HTTPResult res (.getResultObj evt) ]
                 (.setStatus res s)
                 (.replyResult evt)))) )

(defn- make-internal-flow [^Pipeline pipe s]
  (let [ ev (.event (.job pipe)) ]
    (cond
      (instance? HTTPEvent ev)
      (PTask. (make-work s))
      :else
      (throw (FlowError.  
               (str "Unhandled event-type \"" (:typeid (meta ev))  "\"."))))))

(deftype FatalErrorFlow [] PipelineDelegate
  (getStartActivity [_ pipe] 
    (make-internal-flow pipe 500))
  (onStop [_ pipe ] nil)
  (onError [_ error cur] nil))

(deftype OrphanFlow [] PipelineDelegate
  (getStartActivity [_  pipe] 
    (make-internal-flow pipe 501))
  (onStop [_  pipe] nil)
  (onError [_  error cur] nil))


(defn make-FatalErrorFlow ^Startable [job]
  (Pipeline. job "comzotohcljc.hhh.etc.misc.FatalErrorFlow"))

(defn make-OrphanFlow ^Startable [job]
  (Pipeline. job "comzotohcljc.hhh.etc.misc.OrphanFlow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private misc-eof nil)

