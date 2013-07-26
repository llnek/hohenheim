
(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hohenheim.io.netty)


(require '[comzotohcljc.util.coreutils :as CU])
(require '[comzotohcljc.util.strutils :as SU])
(require '[comzotohcljc.netty.nettyio :as NE])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod comp-configure ::NettyIO [co cfg]
  (let [ c (SU/nsb (:context cfg)) ]
    (.setAttr! co :contextPath (SU/strim c))
    (http-basic-config co cfg) ))

(defn- make-service-io [co]
  (reify comzotohcljc.netty.nettyio.NettyServiceIO
    (before-send [_ ch msg] nil)
    (onerror [_ ch msginfo exp] )
    (onreq [_ ch msginfo xdata]
      (let [ evt (HTTPEvent. msginfo xdata)
             w (AsyncWaitEvent. evt (NettyTrigger. evt ch)) ]
        (.timeoutMillis w (.getAttr co :waitMillis))
        (.hold co w)
        (.dispatch co evt)))
    (onres [_ ch msginfo xdata] nil)) )

(defn- make-handler [co]
    (NE/make-pipelineHandler (make-service-io co)))

(defmethod comp-initialize ::NettyIO [co]
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


(defmethod ioes-start ::NettyIO [co]
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

(defmethod ioes-stop ::NettyIO [co]
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






(def ^:private netty-eof nil)

