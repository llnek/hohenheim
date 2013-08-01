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

  comzotohcljc.hohenheim.io.netty)

(import '(java.net URI URL InetSocketAddress))
(import '(java.net InetAddress))

(use '[comzotohcljc.hohenheim.io.events :only (make-netty-event) ])
(use '[comzotohcljc.hohenheim.core.sys])
(use '[comzotohcljc.hohenheim.io.core])
(use '[comzotohcljc.hohenheim.io.http])
(use '[comzotohcljc.hohenheim.io.triggers])

(require '[comzotohcljc.crypto.ssl :as SS])
(require '[comzotohcljc.netty.comms :as NE])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ioes-reify-event :czc.hhh.io/NettyIO
  [co & args]
  (make-netty-event co
                    (nth args 0)
                    (nth args 1)))

(defmethod comp-configure :czc.hhh.io/NettyIO
  [co cfg]
  (let [ c (SU/nsb (:context cfg)) ]
    (.setAttr! co :contextPath (SU/strim c))
    (http-basic-config co cfg) ))

(defn- make-service-io [co]
  (reify comzotohcljc.netty.comms.NettyServiceIO
    (before-send [_ ch msg] nil)
    (onerror [_ ch msginfo exp] )
    (onreq [_ ch msginfo xdata]
      (let [ evt (ioes-reify-event co msginfo xdata)
             w (make-async-wait-holder evt
                 (make-netty-trigger ch evt co)) ]
        (.timeoutMillis w (.getAttr co :waitMillis))
        (.hold co w)
        (.dispatch co evt)))
    (onres [_ ch msginfo xdata] nil)) )

(defn- make-handler [co]
    (NE/netty-pipe-handler (make-service-io co)))

(defmethod comp-initialize :czc.hhh.io/NettyIO
  [co]
  (let [ [bs opts] (NE/server-bootstrap)
         file (.getAttr co :serverKey)
         ssl (CU/notnil? file)
         pwd (.getAttr co :pwd)
         ctx (if ssl (SS/make-sslContext file pwd)) ]
    (doto bs
      (.setPipelineFactory
        (NE/make-pipeServer ctx  (make-handler co))))
    (.setAttr! co :netty 
      (comzotohcljc.netty.comms.NettyServer. bs nil opts))
    co))

(defmethod ioes-start :czc.hhh.io/NettyIO
  [co]
  (let [ host (SU/nsb (.getAttr co :host))
         port (.getAttr co :port)
         nes (.getAttr co :netty)
         ip (if (SU/nichts? host)
              (InetAddress/getLocalHost)
              (InetAddress/getByName host))
         c (.bind (.server nes) (InetSocketAddress. ip port))
         cg (NE/channel-group) ]
;;    c.getConfig().setConnectTimeoutMillis(millis)
    (.add cg c)
    (.setAttr! co :netty
      (comzotohcljc.netty.comms.NettyServer. (.server nes)
                                           cg
                                           (.options nes)) )
    (ioes-started co)))

(defmethod ioes-stop :czc.hhh.io/NettyIO
  [co]
  (let [ nes (.getAttr co :netty) ]
    (NE/finz-server nes)
    (ioes-stopped co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive :czc.hhh.io/NettyIO :czc.hhh.io/HTTP)

(def ^:private netty-eof nil)

