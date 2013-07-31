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

  comzotohcljc.hohenheim.io.jms)


(import '(org.apache.commons.lang3 StringUtils))
(import '(java.util
  Hashtable Properties ResourceBundle))
(import '(javax.jms Connection
  ConnectionFactory
  Destination
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

(require '[comzotohcljc.crypto.codec :as CR])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])

(use '[comzotohcljc.hohenheim.io.core])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol JMSClient)

(defn make-jmsclient "" [container]
  (make-event-emitter container ::JMSClient))

(defmethod ioes-reify-event ::JMSClient
  [co & args]
  (make-jms-event co (first args)))

(defn- onMessage [co msg]
      ;;if (msg!=null) block { () => msg.acknowledge() }
  (.dispatch co (ioes-reify-event co msg)))

(defmethod comp-configure ::JMSClient
  [co cfg]
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

(defn- inizFac [co ctx ^ConnectionFactory cf]
  (let [ des (.getAttr co :destination)
         c (.lookup ctx des)
         ju (.getAttr co :jmsUser)
         jp (SU/nsb (.getAttr co :jmsPwd))
         conn (if (SU/hgl? ju)
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

(defn- inizTopic [co ctx cf]
  (let [ jp (SU/nsb (.getAttr co :jmsPwd))
         des (.getAttr co :destination)
         ju (.getAttr co :jmsUser)
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
                              (onMessage [_ m] (.onMessage x m))) ))
    conn))


(defn- inizQueue [co ctx cf]
  (let [ jp (SU/nsb (.getAttr co :jmsPwd))
         des (.getAttr co :destination)
         ju (.getAttr co :jmsUser)
         conn (if (SU/hgl? ju)
                (.createQueueConnection cf ju (if (SU/hgl? jp) jp nil))
                (.createQueueConnection cf))
         s (.createQueueSession conn false Session/CLIENT_ACKNOWLEDGE)
         q (.lookup ctx des) ]

    (when-not (instance? QueueConnectionFactory q)
      (throw (IOException. "Object not of Queue type.")))

    (-> (.createReceivers s q)
        (.setMessageListener (reify MessageListener
              (onMessage [_ m] (onMessage co m)))))
    conn))


(def ioes-start ::JMSClient
  [co]
  (let [ cf (.getAttr co :contextFactory)
         pl (.getAttr co :providerUrl)
         ju (.getAttr co :jndiUser)
         jp (SU/nsb (.getAttr co :jndiPwd))
         vars (Hashtable.) ]

    (when (SU/hgl? cf)
      (.put vars Context/INITIAL_CONTEXT_FACTORY cf))

    (when (SU/hgl? pl)
      (.put vars Context/PROVIDER_URL pl))

    (when (SU/hgl? ju)
      (.put vars "jndi.user" ju)
      (when (SU/hgl? jp)
        (.put vars "jndi.password" jp)))

    (let [ obj (-> (InitialContext. vars)
                   (.lookup (.getAttr co :connFactory)) )
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


(def ioes-stop ::JMSClient
  [co]
  (let [ c (.getAttr co :conn) ]
    (when-not (nil? c)
      (CU/TryC (.close c)))
    (.setAttr! co :conn nil)
    (ioes-stopped co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive ::JMSClient :comzotohcljc.hohenheim.io.core/EventEmitter)

(def ^:private jms-eof nil)

