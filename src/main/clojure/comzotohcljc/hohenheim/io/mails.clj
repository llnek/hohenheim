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

  comzotohcljc.hohenheim.io.mails )

(use '[clojure.tools.logging :only (info warn error debug)])

(require '[comzotohcljc.crypto.codec :CR])
(require '[comzotohcljc.crypto.core :CU])
(require '[comzotohcljc.crypto.str :SU])

(use '[comzotohcljc.hohenheim.io.loops ])
(use '[comzotohcljc.hohenheim.io.core ])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn- closeFolder [fd]
  (CU/TryC
    (when-not (nil? fd)
      (when (.isOpen fd) (.close fd true))) ))

(defn- closeStore [co]
  (let [ conn (.getAttr co :store)
         fd (.getAttr co :folder) ]
    (closeFolder fd)
    (CU/TryC
      (when-not (nil? conn) (.close conn)) )
    (.setAttr! co :store nil)
    (.setAttr! co :folder nil)) )

(defn- resolve-provider [co ssl2 std2 demo mock]
  (let [ [pkey sn] (if (.getAttr co :ssl) ssl2 std2)
         props (doto (Properties.)
                     (.put props "mail.store.protocol" sn) )
         session (Session/getInstance props nil)
         ps (.getProviders session) ]
    (with-local-vars [proto sn sun nil]
      (var-set sun (some (fn [x] (if (= pkey (.getClassName x)) x nil)) (seq ps)))
      (when (nil? @sun)
        (throw (IOException. (str "Failed to find store: " pkey) )))
      (when (SU/hgl? demo)
        (var-set sun  (Provider. Provider$Type/STORE mock demo "test" "1.0.0"))
        (debug "using demo store " mock " !!!")
        (var-set proto mock) )

      (.setProvider session @sun)
      (.setAttr! co :proto @proto)
      (.setAttr! co :session session))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POP3

(def ST_POP3S  "com.sun.mail.pop3.POP3SSLStore" )
(def ST_POP3  "com.sun.mail.pop3.POP3Store")
(def POP3_MOCK "demopop3s")
(def POP3S "pop3s")
(def POP3C "pop3")

(defn make-pop3client "" [container]
  (make-event-emitter container :czc.hhh.io/POP3))

(defn ioes-reify-event :czc.hhh.io/POP3
  [co args]
  (make-email-event co (first args)))


(defn- connect-pop3 [co]
  (let [ pwd (SU/nsb (.getAttr co :pwd))
         session (.getAttr co :session)
         user (.getAttr co :user)
         host (.getAttr co :host)
         port (.getAttr co :port)
         proto (.getAttr co :proto)
         s (.getStore session proto) ]
    (when-not (nil? s)
      (.connect s host port user (if (SU/hgl? pwd) pwd nil))
      (.setAttr! co :store s)
      (.setAttr! co :folder (.getDefaultFolder s)))
    (if-let [ fd (.getAttr co :folder) ]
      (.setAttr! co :folder (.getFolder fd "INBOX")))
    (let [ fd (.getAttr co :folder) ]
      (when (or (nil? fd) (not (.exists fd)))
        (throw (IOException. "cannot find inbox.")) ))))


(defn- read-pop3 [co msgs]
  (doseq [ mm (seq msgs) ]
    (try
        (doto mm (.getAllHeaders)(.getContent))
        (.dispatch co (ioes-reify-event co mm))
      (finally
        (when delmsg
          (.setFlag mm Flags$Flag/DELETED true))))))

(defn- scan-pop3 [co]
  (let [ s (.getAttr co :store)
         fd (.getAttr co :folder) ]
    (when (and (CU/notnil? fd) (not (.isOpen fd)))
      (.open fd Folder/READ_WRITE) )
    (when (.isOpen fd)
      (let [ cnt (.getMessageCount fd) ]
        (debug "Count of new mail-messages: " cnt)
        (when (> cnt 0)
          (read-pop3 co (.getMessages fd)))))))


(defmethod loopable-oneloop :czc.hhh.io/POP3
  [co]
  (try
      (connect-pop3 co)
      (scan-pop3 co)
    (catch Throwable e# 
      (warn e# ""))
    (finally
      (closeStore co))) )


(defn- std-config [co cfg]
  (let [ port (:port cfg)
         pwd (:passwd cfg) ]
    (.setAttr! co :ssl (if (false? (:ssl cfg)) false true))
    (.setAttr! co :deleteMsg (true? (:deletemsg cfg)))
    (.setAttr! co :host (:host cfg))
    (.setAttr! co :port (if (number? port) port 995))
    (.setAttr! co :user (:username cfg))
    (.setAttr! co :pwd (CR/pwdify (if (SU/hgl? pwd) pwd "")) )
    co))

(defmethod comp-configure :czc.hhh.io/POP3
  [co cfg]
  (let [ demo (System/getProperty "hohenheim.demo.pop3" "") ]
    (std-config co cfg)
    (resolve-provider co
                      (if (.getAttr co :ssl)
                          [ST_POP3S POP3S]
                          [ST_POP3 POP3C])
                      demo POP3_MOCK)
    co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAP

(def ST_IMAPS "com.sun.mail.imap.IMAPSSLStore" )
(def ST_IMAP "com.sun.mail.imap.IMAPStore" )
(def IMAP_MOCK "demoimaps")
(def IMAPS "imaps" )
(def IMAP "imap" )

(defn make-imapclient "" [container]
  (make-event-emitter container :czc.hhh.io/IMAP))

(defmethod ioes-reify-event :czc.hhh.io/IMAP
  [co & args]
  (make-email-event co (first args)))

(defn- connect-imap [co] (connect-pop3 co))

(defn- read-imap [co msgs] (read-pop3 co msgs))

(defn- scan-imap [co] (scan-pop3 co))

(defmethod loopable-oneloop :czc.hhh.io/IMAP
  [co]
  (try
      (connect-imap co)
      (scan-imap co)
    (catch Throwable e# 
      (warn e# ""))
    (finally
      (closeStore co))) )


(defmethod comp-configure :czc.hhh.io/IMAP
  [co cfg]
  (let [ demo (System/getProperty "hohenheim.demo.imap" "") ]
    (std-confi co cfg)
    (resolve-provider co
                      (if (.getAttr co :ssl)
                          [ST_IMAPS IMAPS]
                          [ST_IMAP IMAP])
                      demo IMAP_MOCK)
    co))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(derive :czc.hhh.io/IMAP :czc.hhh.io/ThreadedTimer)
(derive :czc.hhh.io/POP3 :czc.hhh.io/ThreadedTimer)


(def ^:private mails-eof nil)

