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

  comzotohcljc.hohenheim.io.events )

(use '[clojure.tools.logging :only (info warn error debug)])

(use '[comzotohcljc.util.core :only (MutableObjectAPI) ])
(use '[comzotohcljc.hohenheim.io.core])

(import '(org.apache.commons.io IOUtils))
(import '(javax.mail.internet MimeMessage))
(import '(javax.jms Message))
(import '(java.io File))
(import '(com.zotoh.frwk.io XData))
(import '(com.zotoh.hohenheim.core Identifiable))

(require '[comzotohcljc.util.core :as CU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti eve-set-session "" (fn [a b] (:typeid (meta a))))
(defmulti eve-set-result "" (fn [a b] (:typeid (meta a))))
(defmulti eve-destroy "" (fn [a] (:typeid (meta a))))

(defprotocol EventObj
  (emitter [_] ))

(defn make-event [src evtId]
  (let [ impl (CU/make-mmap) ]
    (with-meta
      (reify

        MutableObjectAPI

          (setf! [_ k v] (.mm-s impl k v) )
          (seq* [_] (seq (.mm-m* impl)))
          (getf [_ k] (.mm-g impl k) )
          (clrf! [_ k] (.mm-r impl k) )
          (clear! [_] (.mm-c impl))

        EventObj
          (emitter [_] src) )

      { :typeid evtId } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-filepicker-event [src ^String origFile ^File f action]
  (let [ e (make-event src :czc.hhh.io/FileEvent) ]
    (.setf! e :originalFile (File. origFile))
    (.setf! e :file f)
    (.setf! e :action action)
    e))

(defn make-email-event [src ^MimeMessage msg]
  (let [ e (make-event src :czc.hhh.io/EmailEvent) ]
    (.setf! e :msg msg)
    e))

(defn make-jms-event [src ^Message msg]
  (let [ e (make-event src :czc.hhh.io/JMSEvent) ]
    (.setf! e :msg msg)
    e))

(defn make-timer-event [src repeating]
  (let [ e (make-event src :czc.hhh.io/TimerEvent) ]
    (.setf! e :repeating repeating)
    e))

(defn make-socket-event [src sock]
  (let [ e (make-event src :czc.hhh.io/SocketEvent) ]
    (.setf! e :socket sock)
    e))

(defn make-servlet-event [src req]
  (let [ e (make-event src :czc.hhh.io/HTTPEvent) ]
    (.setf! e :request req)
    e))

(defn make-netty-event [src msginfo ^XData xdata]
  (let [ e (make-event src :czc.hhh.io/HTTPEvent) ]
    (.setf! e :request-data xdata)
    (.setf! e :request msginfo)
    e))

(defn eve-unbind [ev]
  (.setf! ev :waitHolder nil))

(defn eve-bind [ev obj]
  (.setf! ev :waitHolder obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod eve-set-session :czc.hhh.io/EmEvent [obj s]
  (do
    (.setf! obj :session s)
    obj))

(defmethod eve-destroy :czc.hhh.io/EmEvent [obj] nil)

(defmethod eve-set-result :czc.hhh.io/EmEvent [obj res]
  (let [ weh (.getf obj :waitEventHolder)
         s (.getf obj :session)
         src (.getf obj :emitter) ]
    (when-not (nil? s) (.handleResult s obj res))
    (.setf! obj :result res)
    (when-not (nil? weh)
      (try
          (.resumeOnResult weh res)
        (finally
          (.setf! obj :waitEventHolder nil)
          (.release src weh))))))


(defmethod eve-destroy :czc.hhh.io/SocketEvent [obj]
  (let [ s (.getf obj :socket) ]
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

(def ^:private events-eof nil)

