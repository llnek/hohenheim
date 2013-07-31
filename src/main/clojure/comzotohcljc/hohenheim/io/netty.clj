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
  (reify comzotohcljc.netty.nettyio.NettyServiceIO
    (before-send [_ ch msg] nil)
    (onerror [_ ch msginfo exp] )
    (onreq [_ ch msginfo xdata]
      (let [ evt (ioes-reify-event co msginfo xdata)
             w (AsyncWaitEvent. evt (NettyTrigger. evt ch)) ]
        (.timeoutMillis w (.getAttr co :waitMillis))
        (.hold co w)
        (.dispatch co evt)))
    (onres [_ ch msginfo xdata] nil)) )

(defn- make-handler [co]
    (NE/make-pipelineHandler (make-service-io co)))

(defmethod comp-initialize :czc.hhh.io/NettyIO
  [co]
  (let [ [bs opts] (NE/make-server-bootstrap {} )
         file (.getAttr co :serverKey)
         ssl (CU/notnil? file)
         pwd (.getAttr co :pwd)
         ctx (if ssl (make-sslContext file pwd)) ]

    (doto bs
      (.setPipelineFactory
        (reify ChannelPipelineFactory
          (getPipeline [_]
            (let [ pl (org.jboss.netty.channel.Channels/pipeline) ]
              (when-not (nil? ctx)
                (let [ eng (.createSSLEngine ctx) ]
                  (.setUseClientMode eng false)
                  (.addLast pl "ssl" (SslHandler. eng))))
              ;;(.addLast pl "decoder" (HttpRequestDecoder.))
              ;;(.addLast pl "aggregator" (HttpChunkAggregator. 65536))
              ;;(.addLast pl "encoder" (HttpResponseEncoder.))
              (.addLast pl "codec" (HttpServerCodec.))
              ;;(.addLast pl "deflater" (HttpContentCompressor.))
              (.addLast pl "chunker" (ChunkedWriteHandler.))
              (.addLast pl "handler" (make-handler co))
              pl)))))
    (.setAttr! co :netty bs)
    co))


(defmethod ioes-start :czc.hhh.io/NettyIO
  [co]
  (let [ host (SU/nsb (.getAttr co :host))
         port (.getAttr co :port)
         bs (.getAttr co :netty)
         ip (if (SU/nichts? host)
              (InetAddress/getLocalHost)
              (InetAddress/getByName host))
         c (.bind bs (InetSocketAddress. ip port))
         cg (DefaultChannelGroup. (CU/uid)) ]
;;    c.getConfig().setConnectTimeoutMillis(millis)
    (.add cg c)
    (.setAttr! co :cg cg)
    (ioes-started co)))

(defmethod ioes-stop :czc.hhh.io/NettyIO
  [co]
  (let [ bs (.getAttr co :netty)
         cg (.getAttr co :cg)
         cf (.close cg) ]
    (.addListener cf
      (reify ChannelGroupFutureListener
        (operationComplete [_ cff]
          (-> (.getFactory bs)
            (.releaseExternalResources)))))
    (ioes-stopped co)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(derive :czc.hhh.io/NettyIO :czc.hhh.io/HTTP)

(def ^:private netty-eof nil)

