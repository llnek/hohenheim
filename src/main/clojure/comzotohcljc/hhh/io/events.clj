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

  comzotohcljc.hhh.io.events )

(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.util.core :only (MuObj) ])
(use '[comzotohcljc.hhh.io.core])

(import '(org.apache.commons.io IOUtils))
(import '(javax.mail.internet MimeMessage))
(import '(javax.jms Message))
(import '(java.net Socket))
(import '(java.io File))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.frwk.core Identifiable))
(import '(com.zotoh.hohenheim.io IOResult IOEvent Emitter))

(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)



(defmulti eve-set-session "" (fn [a b] (:typeid (meta a))))
(defmulti eve-set-result "" (fn [a b] (:typeid (meta a))))
(defmulti eve-destroy "" (fn [a] (:typeid (meta a))))

(defn make-event [src evtId]
  (let [ eeid (SN/next-long) impl (CU/make-mmap) ]
    (with-meta
      (reify

        MuObj

          (setf! [_ k v] (.mm-s impl k v) )
          (seq* [_] (seq (.mm-m* impl)))
          (getf [_ k] (.mm-g impl k) )
          (clrf! [_ k] (.mm-r impl k) )
          (clear! [_] (.mm-c impl))

        IOEvent
          (emitter [_] src) )

      { :typeid evtId } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eve-unbind [^comzotohcljc.util.core.MuObj ev]
  (.setf! ev :waitHolder nil))

(defn eve-bind [^comzotohcljc.util.core.MuObj ev obj]
  (.setf! ev :waitHolder obj))

(defmethod eve-set-session :czc.hhh.io/EmEvent [^comzotohcljc.util.core.MuObj obj s]
  (do
    (.setf! obj :session s)
    obj))

(defmethod eve-destroy :czc.hhh.io/EmEvent [^comzotohcljc.util.core.MuObj obj] nil)

(defmethod eve-set-result :czc.hhh.io/EmEvent [^IOEvent obj ^IOResult res]
  (let [ ^comzotohcljc.hhh.io.core.WaitEventHolder
         weh (.getf ^comzotohcljc.util.core.MuObj obj :waitEventHolder)
         s (.getSession obj)
         src (.emitter obj) ]
    (when-not (nil? s) (.handleResult s obj res))
    (.setf! ^comzotohcljc.util.core.MuObj obj :result res)
    (when-not (nil? weh)
      (try
          (.resumeOnResult weh res)
        (finally
          (.setf! ^comzotohcljc.util.core.MuObj obj :waitEventHolder nil)
          (.release ^comzotohcljc.hhh.io.core.EmitterAPI src weh))))))


(defmethod eve-destroy :czc.hhh.io/SocketEvent [^comzotohcljc.util.core.MuObj obj]
  (let [ ^Socket s (.getf obj :socket) ]
    (.setf! obj :socket nil)
    (when-not (nil? s) (IOUtils/closeQuietly s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive :czc.hhh.io/WebSockEvent :czc.hhh.io/EmEvent)
(derive :czc.hhh.io/SocketEvent :czc.hhh.io/EmEvent)
(derive :czc.hhh.io/TimerEvent :czc.hhh.io/EmEvent)
(derive :czc.hhh.io/JMSEvent :czc.hhh.io/EmEvent)
(derive :czc.hhh.io/EmailEvent :czc.hhh.io/EmEvent)
(derive :czc.hhh.io/FileEvent :czc.hhh.io/EmEvent)
(derive :czc.hhh.io/HTTPEvent :czc.hhh.io/EmEvent)
;;(derive :czc.hhh.io/MVCEvent :czc.hhh.io/HTTPEvent)

(def ^:private events-eof nil)

