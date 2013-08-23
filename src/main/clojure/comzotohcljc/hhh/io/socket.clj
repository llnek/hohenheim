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

  comzotohcljc.hhh.io.socket)

(import '(java.net InetAddress ServerSocket Socket))
(import '(org.apache.commons.io IOUtils))
(import '(com.zotoh.frwk.core Identifiable))
(import '(com.zotoh.hohenheim.io SocketEvent))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.hhh.io.core])
(use '[comzotohcljc.hhh.core.sys])

(require '[comzotohcljc.util.process :as PU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.seqnum :as SN])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(defmethod ioes-reify-event :czc.hhh.io/SocketIO
  [co & args]
  (let [ ^Socket soc (first args)  eeid (SN/next-long) ]
    (with-meta
      (reify
        Identifiable
        (id [_] eeid)
        SocketEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getSockOut [_] (.getOutputStream soc))
        (getSockIn [_] (.getInputStream soc))
        (emitter [_] co)
        (dispose [_] (IOUtils/closeQuietly soc)))
      { :typeid :czc.hhh.io/SocketEvent } )))

(defn make-socketio "" [container]
  (make-emitter container :czc.hhh.io/SocketIO))


(defmethod comp-configure :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Thingy co cfg]
  (let [ tout (:sock-timeout-millis cfg)
         host (:host cfg)
         port (:port cfg)
         blog (:backlog cfg) ]
    (CU/test-posnum "socket-io port" port)
    (.setAttr! co :timeoutMillis (CU/conv-long tout 0))
    (.setAttr! co :host (SU/nsb host))
    (.setAttr! co :port port)
    (.setAttr! co :backlog (CU/conv-long blog 100))
    co))


(defmethod comp-initialize :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Thingy co]

  (let [ backlog (.getAttr co :backlog)
         host (.getAttr co :host)
         port (.getAttr co :port)
         ip (if (SU/hgl? host) (InetAddress/getByName host) (InetAddress/getLocalHost))
         soc (ServerSocket. port backlog ip) ]
    (info "opened Server Socket: " port " on host: " host)
    (doto soc (.setReuseAddress true))
    (.setAttr! co :ssocket soc)))

(defn- sockItDown [^comzotohcljc.hhh.io.core.EmitterAPI co ^Socket soc]
  (let []
    (.dispatch co (ioes-reify-event co soc))))

(defmethod ioes-start :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^ServerSocket ssoc (.getAttr co :ssocket)
         cl (MU/get-cldr) ]
    (when-not (nil? ssoc)
      (PU/coroutine (fn []
                      (while (.isBound ssoc)
                        (try
                          (sockItDown (.accept ssoc))
                          (catch Throwable e#
                            (IOUtils/closeQuietly ssoc)
                            (.setAttr! co :ssocket nil)))))
                    cl))))

(defmethod ioes-stop :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Thingy co]
  (let [ ^ServerSocket ssoc (.getAttr co :ssocket) ]
    (IOUtils/closeQuietly ssoc)
    (.setAttr! co :ssocket nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private socket-eof nil)

