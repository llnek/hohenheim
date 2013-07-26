(ns ^{ :doc ""
       :author "kenl" }

  comzotohcljc.hohenheim.io.events )

(use '[comzotohcljc.util.coreutils :only (MutableObjectAPI) ])
(import '[javax.mail MimeMessage])
(import '[javax.jms Message])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti evt-set-session (fn [a b] (:typeid (meta a))))
(defmulti evt-set-result (fn [a b] (:typeid (meta a))))

(defprotocol EventObj
  (emitter [_] ))

(defprotocol FileEvent )
(defprotocol EmailEvent)
(defprotocol JMSEvent)
(defprotocol TimerEvent)
(defprotocol SocketEvent)
(defprotocol HTTPEvent)


(defmacro make-event [id src]
  `(let [ impl# (CU/make-mmap) ]
    (with-meta
      (reify

        MutableObjectAPI

          (setf! [_ k# v#] (.mm-s impl# k# v#) )
          (seq* [_] (seq (.mm-m impl#)))
          (getf [_ k1#] (.mm-g impl# k1#) )
          (clrf! [_ k2#] (.mm-r impl# k2#) )
          (clear! [_] (.mm-c impl#))

        EventObj
          (emitter [_] ~src)
        ~id ))))


(defn make-file-event [src ^String origFile ^File f action]
  (let [ e (make-event FileEvent src) ]
    (.setf! e :originalFile (File. origFile))
    (.setf! e :file f)
    (.setf! e :action action)
    (with-meta e { :typeid ::FileEvent } )))

(defn make-email-event [src ^MimeMessage msg]
  (let [ e (make-event EmailEvent src) ]
    (.setf! e :msg msg)
    (with-meta e { :typeid ::EmailEvent } )))

(defn make-jms-event [src ^Message msg]
  (let [ e (make-event JMSEvent src) ]
    (.setf! e :msg msg)
    (with-meta e { :typeid ::JMSEvent } )))

(defn make-timer-event [src single]
  (let [ e (make-event TimerEvent src) ]
    (.setf! e :repeating (not single))
    (with-meta e { :typeid ::TimerEvent } )))

(defn make-socket-event [src sock]
  (let [ e (make-event SocketEvent src) ]
    (.setf! e :socket sock)
    (with-meta e { :typeid ::SocketEvent } )))

(defn make-servlet-event [src req]
  (let [ e (make-event HTTPEvent src) ]
    (with-meta e { :typeid ::HTTPEvent } )))

(defn make-netty-event [src msginfo]
  (let [ e (make-event HTTPEvent src) ]
    (with-meta e { :typeid ::HTTPEvent } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod evt-destroy ::SocketEvent [obj]
  (let [ s (.getf obj :socket) ]
    (.setf! obj :socket nil)
    (when-not (nil? s) (IOUtils/closeQuietly s))))

(defmethod evt-destroy ::EventObj [obj] nil)

(defmethod evt-set-session ::EventObj [obj s]
  (do
    (.setf obj :session s)
    obj))

(defmethod evt-set-result ::EventObj [obj res]
  (let [ s (.getf obj :session)
         src (.getf obj :emitter)
         weh (.getf obj :waitEventHolder) ]
    (when-not (nil? s) (.handleResult s obj res))
    (.setf! obj :result res)
    (when-not (nil? weh)
      (try
          (.resumeOnResult weh res)
        (finally
          (.setf! obj :waitEventHolder nil)
          (.release src weh))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(def ^:private events-eof nil)

