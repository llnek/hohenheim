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

