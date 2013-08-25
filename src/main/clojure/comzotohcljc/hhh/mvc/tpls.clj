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
  comzotohcljc.hhh.mvc.tpls)

(import '(org.jboss.netty.handler.codec.http 
  HttpMethod HttpHeaders HttpResponseStatus
  HttpRequest HttpResponse))
(import '(org.jboss.netty.handler.stream
  ChunkedFile ChunkedStream ChunkedInput ))
(import '(org.jboss.netty.channel Channel))

(import '(org.apache.commons.io FileUtils))
(import '(com.zotoh.hohenheim.mvc 
  WebContent WebAsset
  HTTPRangeInput AssetCache))
(import '(java.io RandomAccessFile File))
(import '(java.util HashMap))

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.netty.comms :as NE])
(require '[comzotohcljc.util.mime :as MM])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.io :as IO])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-webcontent [^String cType bits]
  (reify
    WebContent
    (contentType [_] cType)
    (body [_] bits)))

(defn getLocalFile [^File appDir ^String fname]
  (let [ f (File. appDir fname) ]
    (if (.canRead f)
      (make-webcontent
        (MM/guess-contenttype f "utf-8")
        (FileUtils/readFileToByteArray f))
      nil)))

(defn- maybeCache [^File fp]
  (let [ ^String fpath (.toLowerCase (CU/nice-fpath fp)) ]
    (or (.endsWith fpath ".css")
        (.endsWith fpath ".gif")
        (.endsWith fpath ".jpg")
        (.endsWith fpath ".jpeg")
        (.endsWith fpath ".png")
        (.endsWith fpath ".js"))))

(defn- make-web-asset [^File file]
  (let [ ct (MM/guess-contenttype file "utf-8" "text/plain")
         ts (.lastModified file)
         bits (FileUtils/readFileToByteArray file) ]
    (reify
      WebAsset

      (contentType [_] ct)
      (getFile [_] file)
      (getTS [_] ts)
      (size [_] (alength bits))
      (getBytes [_] bits) )))

(defn- fetchAndSetAsset [^HashMap cache fp ^File file]
  (let [ wa (if (and (.exists file)
                    (.canRead file))
              (make-web-asset file)) ]
    (if (nil? wa)
      (do
        (warn "asset-cache: failed to read/find file: " fp)
        nil)
      (do
        (debug "asset-cache: cached new file: " fp)
        (.put cache fp wa)
        wa))))

(defn- getAsset [^File file]
  (let [ cache (AssetCache/get)
         fp (CU/nice-fpath file)
         ^WebAsset wa (.get cache fp)
         ^File cf (if (nil? wa) nil (.getFile wa)) ]
    (if (or (nil? cf)
            (> (.lastModified file)
               (.getTS wa)))
      (fetchAndSetAsset cache fp file)
      wa)))

(defn- getFileInput

  [ ^RandomAccessFile raf
    ^String ct
    ^HttpRequest req
    ^HttpResponse rsp ]

    (if (HTTPRangeInput/accepts req)
      (let [ inp (HTTPRangeInput. raf ct req) ]
        (.prepareNettyResponse inp rsp)
        inp)
      (ChunkedFile. raf)))

(defn replyFileAsset
  [ src ^Channel ch ^HttpRequest req ^HttpResponse rsp ^File file]
  (let [ ^WebAsset asset (if (not (maybeCache file))
                 nil
                 (getAsset file)) ]
    (with-local-vars [raf nil clen 0 inp nil ct ""]
      (if (nil? asset)
        (do
          (var-set ct (MM/guess-contenttype file "utf-8" "text/plain"))
          (var-set raf (RandomAccessFile. file "r"))
          (var-set clen (.length ^RandomAccessFile @raf))
          (var-set inp (getFileInput @raf @ct req rsp)))
        (do
          (var-set ct (.contentType asset))
          (var-set clen (.size asset))
          (var-set inp (ChunkedStream. (IO/streamify (.getBytes asset))))) )
      (debug "serving file: " (.getName file)
             " with clen= " @clen
             ", ctype= " @ct)
      (try
        (when (not= (.getStatus rsp) HttpResponseStatus/NOT_MODIFIED)
          (.setHeader rsp "Content-Length" (str "" @clen)))
        (.addHeader rsp "Accept-Ranges" "bytes")
        (.setHeader rsp "Content-Type" @ct)
        (if (= (.getMethod req) HttpMethod/HEAD)
          (try
            (.write ch rsp)
            (finally
              (CU/Try! (when (CU/notnil? @raf)(.close ^RandomAccessFile @raf)))
              (var-set raf nil)))
          (NE/closeCF
            (not (HttpHeaders/isKeepAlive req))
            (do
              (.write ch rsp)
              (.write ch @inp))) )
        (catch Throwable e#
          (error e# "")
          (CU/Try! (when (CU/notnil? @raf)(.close ^RandomAccessFile @raf)))
          (CU/Try! (.close ch))) ) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private tpls-eof nil)

