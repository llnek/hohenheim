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

  comzotohcljc.net.comms )

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(java.security.cert
  X509Certificate
  CertificateException))
(import '(java.security
  KeyStoreException
  KeyStore InvalidAlgorithmParameterException))
(import '(javax.net.ssl
  SSLContext SSLEngine X509TrustManager
  TrustManagerFactorySpi TrustManager
  ManagerFactoryParameters))
(import '(com.zotoh.frwk.net SSLTrustMgrFactory))
(import '(com.zotoh.frwk.io XData))
(import '(org.apache.commons.lang3 StringUtils))
(import '(org.apache.http.client HttpClient))
(import '(org.apache.http.client.methods HttpGet HttpPost))
(import '(org.apache.http.impl.client DefaultHttpClient))
(import '(org.apache.http
  Header
  StatusLine HttpEntity HttpResponse))
(import '(java.io File IOException))
(import '(org.apache.http.util EntityUtils))
(import '(java.net URL URI))
(import '(org.apache.http.params HttpConnectionParams))
(import '(org.apache.http.entity InputStreamEntity))
(import '(com.zotoh.frwk.io XData))


(use '[comzotohcljc.util.core :only (MutableMapAPI) ])

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.mime :as MM])
(require '[comzotohcljc.util.str :as SU])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)



(def ^:dynamic *socket-timeout* 5000)
(def ^:dynamic  *LHOST* "localhost")
(def ^:dynamic  *WP_HTTP* "HTTP")
(def ^:dynamic  *WP_SMTP* "SMTP")
(def ^:dynamic  *WP_SFTP* "SFTP")
(def ^:dynamic  *WP_FTP* "FTP")
(def ^:dynamic  *WP_FILE* "FILE")

(defrecord HTTPMsgInfo [^String protocol ^String method ^String uri
                        is-chunked keep-alive
                        clen headers params] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal functions to support apache http client.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- mkApacheClientHandle ^HttpClient []
  (let [ cli (DefaultHttpClient.)
         pms (.getParams cli) ]
    (HttpConnectionParams/setConnectionTimeout pms *socket-timeout*)
    (HttpConnectionParams/setSoTimeout pms *socket-timeout*)
    cli))

(defn- get-bits ^bytes [^HttpEntity ent] (if (nil? ent) nil (EntityUtils/toByteArray ent)) )
(defn- get-str ^String [^HttpEntity ent] (EntityUtils/toString ent "utf-8"))

(defn- p-ok [^HttpResponse rsp]
  (let [ ent (.getEntity rsp)
         ct (if (nil? ent) nil (.getContentType ent))
         cv (if (nil? ct) "" (SU/strim (.getValue ct)))
         cl (.toLowerCase ^String cv) ]
    (CU/Try!
      (debug "http-response: " "content-encoding: " (.getContentEncoding ent) "\n"
             "content-type: " cv))
    (let [ bits (get-bits ent)
           clen (if (nil? bits) 0 (alength bits)) ]
      { :encoding (MM/get-charset cv)
        :content-type cv
        :data (if (== clen 0) nil (XData. bits)) } )))
    ;;(cond
      ;;(or (.startsWith cl "text/")
          ;;(.startsWith cl "application/xml")
          ;;(.startsWith cl "application/json")) (get-bits ent) ;;(get-str ent)
      ;;:else (get-bits ent))) )

(defn- p-error [^HttpResponse rsp ^Throwable exp]
  (do
    (CU/Try! (EntityUtils/consumeQuietly (.getEntity rsp)))
    (throw exp)) )

(defn- p-redirect [^HttpResponse rsp]
  ;;TODO - handle redirect
  (p-error rsp (IOException. "Redirect not supported.")) )

(defn- p-reply [^HttpResponse rsp]
  (let [ st (.getStatusLine rsp)
         msg (if (nil? st) "" (.getReasonPhrase st))
         rc (if (nil? st) 0 (.getStatusCode st)) ]
    (cond
      (and (>= rc 200) (< rc 300))
      (p-ok rsp)

      (and (>= rc 300) (< rc 400))
      (p-redirect rsp)

      :else
      (p-error rsp (IOException. (str "Service Error: code = " rc ": " msg))))) )


(defn- do-post [^HttpClient cli ^URL targetUrl ^String contentType ^XData xdata beforeSendFunc]
  (try
    (let [ ent (InputStreamEntity. (.stream xdata) (.size xdata))
           p (HttpPost. (.toURI targetUrl)) ]
      (.setEntity p (doto ent
                          (.setContentType contentType)
                          (.setChunked true)))
      (when (fn? beforeSendFunc) (beforeSendFunc p))
      (p-reply (.execute cli p)))
    (finally
        (.. cli getConnectionManager shutdown))) )

(defn- do-get [^HttpClient cli ^URL targetUrl beforeSendFunc]
  (try
    (let [ g (HttpGet. (.toURI targetUrl)) ]
      (when (fn? beforeSendFunc) (beforeSendFunc g))
      (p-reply (.execute cli g)))
    (finally
      (.. cli getConnectionManager shutdown))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sync-post "Perform a http-post on the target url."
  ([^URL targetUrl contentType ^XData xdata] (sync-post targetUrl contentType xdata nil))
  ([^URL targetUrl contentType ^XData xdata beforeSendFunc]
    (do-post (mkApacheClientHandle) targetUrl contentType xdata beforeSendFunc)))

(defn sync-get "Perform a http-get on the target url."
  ([^URL targetUrl] (sync-get targetUrl nil))
  ([^URL targetUrl beforeSendFunc]
    (do-get (mkApacheClientHandle) targetUrl beforeSendFunc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-simpleClientSSLEngine "Simple minded, trusts everyone."
  []
  (let [ c (SSLContext/getInstance "TLS") ]
    (.init c nil (SSLTrustMgrFactory/getTrustManagers) nil)) )

(defn- clean-str [^String s]
  (StringUtils/stripStart (StringUtils/stripEnd s ";,") ";,"))

(defn parse-ie [^String line]
  (let [ p1 #".*(MSIE\s*(\S+)\s*).*"
         m1 (re-matches p1 line)
         p2 #".*(Windows\s*Phone\s*(\S+)\s*).*"
         m2 (re-matches p2 line)
         bw "IE"
         dt (if (SU/has-nocase? "iemobile") :mobile :pc) ]
    (let [ bv (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "")
           dev (if (and (not (empty? m2)) (> (count m2) 2))
                 { :device-version (clean-str (nth m1 2))
                  :device-moniker "windows phone"
                  :device-type :phone }
                 {} ) ]
      (merge {:browser :ie :browser-version bv :device-type dt}
             dev))))

(defn parse-chrome [^String line]
  (let [ p1 #".*(Chrome/(\S+)).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    {:browser :chrome :browser-version bv :device-type :pc }))

(defn parse-kindle [^String line]
  (let [ p1 #".*(Silk/(\S+)).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :silk :browser-version bv :device-type :mobile :device-moniker "kindle" } ))

(defn parse-android [^String line]
  (let [ p1 #".*(Android\s*(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :chrome :browser-version bv :device-type :mobile :device-moniker "android" } ))

(defn parse-ffox [^String line]
  (let [ p1 #".*(Firefox/(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "") ]
    { :browser :firefox :browser-version bv :device-type :pc } ))

(defn parse-safari [^String line]
  (let [ p1 #".*(Version/(\S+)\s*).*"
         m1 (re-matches p1 line)
         bv   (if (and (not (empty? m1)) (> (count m1) 2))
                (clean-str (nth m1 2))
                "")
         rc { :browser :safari :browser-version bv :device-type :pc } ]
    (cond
      (SU/has-nocase? line "mobile/") (merge rc { :device-type :mobile })
      (SU/has-nocase? line "iphone") (merge rc { :device-type :phone :device-moniker "iphone" } )
      (SU/has-nocase? line "ipad") (merge rc { :device-type :mobile :device-moniker "ipad" } )
      (SU/has-nocase? line "ipod") (merge rc { :device-type :mobile :device-moniker "ipod" } )
      :else rc )))


(defn parse-userAgentLine "Retuns a map of browser/device attributes."
  [^String agentLine]
  (let [ line (SU/strim agentLine) ]
    (cond
      (and (SU/embeds? line "Windows") (SU/embeds? line "Trident/"))
      (parse-ie line)

      (and (SU/embeds? line "AppleWebKit/")(SU/embeds? line "Safari/")
        (SU/embeds? line "Chrome/"))
      (parse-chrome line)

      (and (SU/embeds? line "AppleWebKit/") (SU/embeds? line "Safari/")
        (SU/embeds? line "Android"))
      (parse-android line)

      (and (SU/embeds? line "AppleWebKit/")(SU/embeds? line "Safari/")
      (SU/embeds? line "Silk/"))
      (parse-kindle line)

      (and (SU/embeds? line "Safari/")(SU/embeds? line "Mac OS X"))
      (parse-safari)

      (and (SU/embeds? line "Gecko/")(SU/embeds? line "Firefox/"))
      (parse-ffox)

      :else
      {} )))




(def ^:private comms-eof nil)

