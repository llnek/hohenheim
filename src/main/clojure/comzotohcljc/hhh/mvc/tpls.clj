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

(import '(org.apache.commons.io FileUtils))
(import '(com.zotoh.hohenheim.mvc AssetCache))
(import '(java.io File))

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[comzotohcljc.util.mime :as MM])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol WebPage
  ""
  (contentType [_] )
  (body [_] ))

(defn- make-web-page [^String text ^String cType]
  (reify
    WebPage
    (contentType [_] cType)
    (body [_] text)))

(defn getTemplate [^File appDir ^String fname]
  (let [ f (File. appDir fname) ]
    (if (.canRead f)
      (make-web-page
        (FileUtils/readFileToString f "utf-8")
        (MM/guessContentType f "utf-8"))
      nil)))

(defn- maybeCache [^File fp]
  (let [ fpath (.toLowerCase (CU/nice-fpath fp)) ]
    (or (.endsWith fpath ".css")
        (.endsWith fpath ".gif")
        (.endsWith fpath ".jpg")
        (.endsWith fpath ".jpeg")
        (.endsWith fpath ".png")
        (.endsWith fpath ".js"))))


(defn- fetchAndSetAsset [cache fp file]

(defn- getAsset [^File file]
  (let [ cache (AssetCache/get)
         fp (CU/nice-fpath file) ]
    (if (.containsKey cache fp)
      (.get cache fp)
      (fetchAndSetAsset cache fp file))))

(defn getFile
  [ src ^Channel ch ^HTTPEvent evt ^HttpResponse rsp ^File file]
  (let [ asset (if (not (maybeCache file))
                 nil
                 (getAsset file)) ]
    (with-local-vars [raf nil clen 0 inp nil ct ""]
      (if (nil? asset)
        (do
          (var-set ct (MM/guessContentType file "utf-8" "text/plain"))
          (var-set raf (RandomAccessFile. file "r"))
          (var-set clen (.length @raf))
          (var-set inp (getFileInput @raf @ct evt rsp)))
        (do
          (var-set ct (contentType asset))
          (var-set clen (.size asset))
          (var-set inp (ChunkedStream. (CU/streamify (.bytes asset))))) )
      (debug "serving file: " (.getName file)
             " with clen= " @clen
             ", ctype= " @ct)
      (try
        (when (not= (.getStatus rsp) HttpResponseStatus/NOT_MODIFIED)
          (.setHeader rsp "Content-Length" (str "" @clen)))
        (.addHeader rsp "Accept-Ranges" "bytes")
        (.setHeader rsp "Content-Type" @ct)
        (if (= (.method evt) "HEAD")
          (try
            (.write ch rsp)
            (finally
              (CU/Try! (when (CU/notnil? @raf)(.close @raf)))
              (var-set raf nil)))
          (NE/closeCF
            (not (.isKeepAlive evt))
            (do
              (.write ch rsp)
              (.write ch inp))) )
        (catch Throwable e#
          (error e# "")
          (CU/Try! (when (CU/notnil? @raf)(.close @raf)))
          (CU/Try! (.close ch))) ) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private tpls-eof nil)

