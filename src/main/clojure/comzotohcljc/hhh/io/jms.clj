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

  comzotohcljc.hhh.io.jms)

(import '(com.zotoh.frwk.core Identifiable))



(import '(org.apache.commons.lang3 StringUtils))
(import '(java.util
  Hashtable Properties ResourceBundle))
(import '(javax.jms Connection
  ConnectionFactory
  Destination
  Connection
  Message
  MessageConsumer
  MessageListener
  Queue
  QueueConnection
  QueueConnectionFactory
  QueueReceiver
  QueueSession
  Session
  Topic
  TopicConnection
  TopicConnectionFactory
  TopicSession
  TopicSubscriber))
(import '(javax.naming Context InitialContext))
(import '(java.io IOException))
(import '(com.zotoh.hohenheim.io JMSEvent))

(use '[comzotohcljc.util.core :only (MuObj) ])
(require '[comzotohcljc.crypto.codec :as CR])
(require '[comzotohcljc.util.seqnum :as SN])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hhh.io.events :only (make-jms-event) ])
(use '[comzotohcljc.hhh.core.sys])
(use '[comzotohcljc.hhh.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defn make-jmsclient "" [container]
  (make-emitter container :czc.hhh.io/JMS))

(defmethod ioes-reify-event :czc.hhh.io/JMS
  [co & args]
  (let [ msg (first args)  eeid (SN/next-long) ]
    (with-meta
      (reify 
        Identifiable
        (id [_] eeid)
        JMSEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (emitter [_] co)
        (getMsg [_] msg))
      { :typeid :czc.hhh.io/JMSEvent } )))

(defn- onMessage [^comzotohcljc.hhh.io.core.EmitterAPI co msg]
      ;;if (msg!=null) block { () => msg.acknowledge() }
  (.dispatch co (ioes-reify-event co msg)))

(defmethod comp-configure :czc.hhh.io/JMS
  [^comzotohcljc.hhh.core.sys.Thingy co cfg]
  (do
    (.setAttr! co :contextFactory (:contextfactory cfg))
    (.setAttr! co :connFactory (:connfactory cfg))
    (.setAttr! co :jndiUser (:jndiuser cfg))
    (.setAttr! co :jndiPwd (CR/pwdify (:jndipwd cfg)))
    (.setAttr! co :jmsUser (:jmsuser cfg))
    (.setAttr! co :jmsPwd (CR/pwdify (:jmspwd cfg)))
    (.setAttr! co :durable (:durable cfg))
    (.setAttr! co :providerUrl (:providerurl cfg))
    (.setAttr! co :destination (:destination cfg))
    co))

(defn- inizFac ^Connection [^comzotohcljc.hhh.core.sys.Thingy co
                ^InitialContext ctx
                ^ConnectionFactory cf]

  (let [ ^String des (.getAttr co :destination)
         c (.lookup ctx des)
         ju (.getAttr co :jmsUser)
         jp (SU/nsb (.getAttr co :jmsPwd))
         ^Connection conn (if (SU/hgl? ju)
                (.createConnection cf ju (if (SU/hgl? jp) jp nil))
                (.createConnection cf)) ]

    (if (instance? Destination c)
      ;;TODO ? ack always ?
      (->
        (.createSession conn false Session/CLIENT_ACKNOWLEDGE)
        (.createConsumer c)
        (.setMessageListener (reify MessageListener
          (onMessage [_ m] (onMessage co m)))))
      (throw (IOException. "Object not of Destination type.")))

    conn))

(defn- inizTopic ^Connection [^comzotohcljc.hhh.core.sys.Thingy co
                  ^InitialContext ctx 
                  ^TopicConnectionFactory cf]

  (let [ ^String jp (SU/nsb (.getAttr co :jmsPwd))
         ^String des (.getAttr co :destination)
         ^String ju (.getAttr co :jmsUser)
         conn (if (SU/hgl? ju)
                (.createTopicConnection cf ju (if (SU/hgl? jp) jp nil))
                (.createTopicConnection cf))
         s (.createTopicSession conn false Session/CLIENT_ACKNOWLEDGE)
         t (.lookup ctx des) ]

    (when-not (instance? Topic t)
      (throw (IOException. "Object not of Topic type.")))

    (-> (if (.getAttr co :durable)
          (.createDurableSubscriber s t (CU/uid))
          (.createSubscriber s t))
      (.setMessageListener (reify MessageListener
                              (onMessage [_ m] (onMessage co m))) ))
    conn))


(defn- inizQueue ^Connection [^comzotohcljc.hhh.core.sys.Thingy co
                  ^InitialContext ctx 
                  ^QueueConnectionFactory cf]

  (let [ ^String jp (SU/nsb (.getAttr co :jmsPwd))
         ^String des (.getAttr co :destination)
         ^String ju (.getAttr co :jmsUser)
         conn (if (SU/hgl? ju)
                (.createQueueConnection cf ju (if (SU/hgl? jp) jp nil))
                (.createQueueConnection cf))
         s (.createQueueSession conn false Session/CLIENT_ACKNOWLEDGE)
         q (.lookup ctx des) ]

    (when-not (instance? Queue q)
      (throw (IOException. "Object not of Queue type.")))

    (-> (.createReceiver s ^Queue q)
        (.setMessageListener (reify MessageListener
              (onMessage [_ m] (onMessage co m)))))
    conn))


(defmethod ioes-start :czc.hhh.io/JMS
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^String cf (.getAttr co :contextFactory)
         pl (.getAttr co :providerUrl)
         ^String ju (.getAttr co :jndiUser)
         ^String jp (SU/nsb (.getAttr co :jndiPwd))
         vars (Hashtable.) ]

    (when (SU/hgl? cf)
      (.put vars Context/INITIAL_CONTEXT_FACTORY cf))

    (when (SU/hgl? pl)
      (.put vars Context/PROVIDER_URL pl))

    (when (SU/hgl? ju)
      (.put vars "jndi.user" ju)
      (when (SU/hgl? jp)
        (.put vars "jndi.password" jp)))

    (let [ ctx (InitialContext. vars)
           obj (.lookup ctx ^String (.getAttr co :connFactory))
           c (cond
               (instance? QueueConnectionFactory obj)
               (inizQueue co ctx obj)

               (instance? TopicConnectionFactory obj)
               (inizTopic co ctx obj)

               (instance? ConnectionFactory obj)
               (inizFac co ctx obj)

               :else
               nil) ]
      (when (nil? c)
        (throw (IOException. "Unsupported JMS Connection Factory")) )

      (.setAttr! co :conn c)
      (.start c)
      (ioes-started co))))


(defmethod ioes-stop :czc.hhh.io/JMS
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^Connection c (.getAttr co :conn) ]
    (when-not (nil? c)
      (CU/TryC (.close c)))
    (.setAttr! co :conn nil)
    (ioes-stopped co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private jms-eof nil)

