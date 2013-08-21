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

(defn make-filepicker-event [src ^String origFile ^File f action]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/FileEvent) ]
    (.setf! e :originalFile (File. origFile))
    (.setf! e :file f)
    (.setf! e :action action)
    e))

(defn make-email-event [src ^MimeMessage msg]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/EmailEvent) ]
    (.setf! e :msg msg)
    e))

(defn make-jms-event [src ^Message msg]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/JMSEvent) ]
    (.setf! e :msg msg)
    e))

(defn make-timer-event [src repeating]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/TimerEvent) ]
    (.setf! e :repeating repeating)
    e))

(defn make-socket-event [src sock]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/SocketEvent) ]
    (.setf! e :socket sock)
    e))

(defn make-servlet-event [src req]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/HTTPEvent) ]
    (.setf! e :request req)
    e))

(defn make-netty-event [src msginfo ^XData xdata]
  (let [ ^comzotohcljc.util.core.MuObj e (make-event src :czc.hhh.io/HTTPEvent) ]
    (.setf! e :request-data xdata)
    (.setf! e :request msginfo)
    e))

(defn eve-unbind [^comzotohcljc.util.core.MuObj ev]
  (.setf! ev :waitHolder nil))

(defn eve-bind [^comzotohcljc.util.core.MuObj ev obj]
  (.setf! ev :waitHolder obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

