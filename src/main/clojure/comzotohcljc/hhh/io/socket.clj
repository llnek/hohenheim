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

(defn make-socketio "" [container]
  (make-emitter container :czc.hhh.io/SocketIO))

(defmethod ioes-reify-event :czc.hhh.io/SocketIO
  [co & args]
  (let [ ^Socket soc (first args)
         eeid (SN/next-long) ]
    (with-meta
      (reify

        Identifiable
        (id [_] eeid)

        SocketEvent
        (bindSession [_ s] nil)
        (getSession [_] nil)
        (getId [_] eeid)
        (getSockOut [_] (.getOutputStream soc))
        (getSockIn [_] (.getInputStream soc))
        (emitter [_] co)
        (dispose [_] (IOUtils/closeQuietly soc)))

      { :typeid :czc.hhh.io/SocketEvent } )))

(defmethod comp-configure :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Element co cfg]
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
  [^comzotohcljc.hhh.core.sys.Element co]

  (let [ backlog (.getAttr co :backlog)
         host (.getAttr co :host)
         port (.getAttr co :port)
         ip (if (SU/hgl? host) (InetAddress/getByName host) (InetAddress/getLocalHost))
         soc (ServerSocket. port backlog ip) ]
    (info "opened Server Socket " soc  " (bound?) " (.isBound soc))
    (doto soc (.setReuseAddress true))
    (.setAttr! co :ssocket soc)))

(defn- sockItDown [^comzotohcljc.hhh.io.core.EmitterAPI co ^Socket soc]
  (let []
    (.dispatch co (ioes-reify-event co soc))))

(defmethod ioes-start :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ ^ServerSocket ssoc (.getAttr co :ssocket)
         cl (MU/get-cldr) ]
    (when-not (nil? ssoc)
      (PU/coroutine (fn []
                      (while (.isBound ssoc)
                        (try
                          (sockItDown co (.accept ssoc))
                          (catch Throwable e#
                            (warn e# "")
                            (IOUtils/closeQuietly ssoc)
                            (.setAttr! co :ssocket nil)))))
                    cl))
    (ioes-started co)))

(defmethod ioes-stop :czc.hhh.io/SocketIO
  [^comzotohcljc.hhh.core.sys.Element co]
  (let [ ^ServerSocket ssoc (.getAttr co :ssocket) ]
    (IOUtils/closeQuietly ssoc)
    (.setAttr! co :ssocket nil)
    (ioes-stopped co)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private socket-eof nil)

