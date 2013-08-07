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

(ns ^{ :doc "OS Process related utilities." 
       :author "kenl" }

  comzotohcljc.util.process)

(import '(java.lang.management ManagementFactory))
(import '(com.zotoh.frwk.util CoreUtils))
(import '(java.lang Thread Runnable))

(require '[ comzotohcljc.util.core :as CU])
(require '[ comzotohcljc.util.meta :as MU])
(require '[ comzotohcljc.util.str :as SU])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defn async-exec "Run the code (runnable) in a separate daemon thread."
  ([^Runnable runable] (async-exec runable (MU/get-cldr)))
  ([^Runnable runable ^ClassLoader cl]
    (if (nil? runable)
      nil
      (doto (Thread. runable)
        (.setContextClassLoader cl)
        (.setDaemon true)
        (.start))) ))

(defn coroutine "Run this function asynchronously."
  ([func] (coroutine func nil))
  ([func cl]
    (let [ r (reify Runnable
               (run [_] (when (fn? func) (func)))) ]
      (async-exec r cl))))

(defn safe-wait "Block current thread for some millisecs."
  [^long millisecs]
  (CU/Try!
    (when (> millisecs 0) (Thread/sleep millisecs))
    ))

(defn pid "Get the current process pid."
  ^String []
  (let [ ss (.split (SU/nsb (.getName (ManagementFactory/getRuntimeMXBean))) "@") ]
    (if (or (nil? ss) (empty ss)) "" (first ss))) )



(def ^:private process-eof nil)

