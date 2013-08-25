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

(ns ^{ :doc "This is a utility class that provides various MIME related functionality." 
       :author "kenl" }

  comzotohcljc.util.mime)

(use '[clojure.tools.logging :only (info warn error debug)])

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.net URLDecoder URLEncoder))
(import '(java.io IOException InputStream File))
(import '(java.net URL))
(import '(java.util.regex Pattern Matcher))
(import '(java.util Properties))
(import '(javax.mail Message))

(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.meta :as MU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.io :as IO])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)



(def ^:dynamic *CTE_QUOTED* "quoted-printable")
(def ^:dynamic *CTE_7BIT* "7bit")
(def ^:dynamic *CTE_8BIT* "8bit")
(def ^:dynamic *CTE_BINARY* "binary")
(def ^:dynamic *CTE_BASE64* "base64")

(def ^:dynamic *MIME_USER_PROP*  "mime.rfc2822.user")
(def ^:dynamic *MIME_USER_JAVAMAIL*   "javamail")
(def ^:dynamic *DEF_USER*  "popeye")
(def ^:dynamic *MIME_USER_PREFIX*   "zotoh")
(def ^:dynamic *DEF_HOST*  "localhost")
(def ^:dynamic *MIME_HEADER_MSGID*  "Message-ID")
(def ^:dynamic *MIME_MULTIPART_BOUNDARY*  "boundary")
(def ^:dynamic *DOT*   ".")
(def ^:dynamic *AT*  "@")
(def ^:dynamic *CH_DOT*   \. )
(def ^:dynamic *CH_AT*  \@)
(def ^:dynamic *STR_LT*   "<")
(def ^:dynamic *STR_GT*  ">")
(def ^:dynamic *ALL*   -1)
(def ^:dynamic *ALL_ASCII*   1)
(def ^:dynamic *MOSTLY_ASCII*   2)
(def ^:dynamic *MOSTLY_NONASCII*   3)

;; Capitalized MIME constants to use when generating MIME headers)
;; for messages to be transmitted.)
(def ^:dynamic *AS2_VER_ID*    "1.1")
(def ^:dynamic *UA*  "user-agent")
(def ^:dynamic *TO*   "to")
(def ^:dynamic *FROM*  "from")
(def ^:dynamic *AS2_VERSION*    "as2-version")
(def ^:dynamic *AS2_TO*   "as2-to")
(def ^:dynamic *AS2_FROM*  "as2-from")
(def ^:dynamic *SUBJECT*    "subject")
(def ^:dynamic *CONTENT_TYPE*  "content-type")
(def ^:dynamic *CONTENT*     "content")
(def ^:dynamic *CONTENT_NAME*   "content-name")
(def ^:dynamic *CONTENT_LENGTH*  "content-length")
(def ^:dynamic *CONTENT_LOC*  "content-Location")
(def ^:dynamic *CONTENT_ID*    "content-id")
(def ^:dynamic *CONTENT_TRANSFER_ENCODING*  "content-transfer-encoding")
(def ^:dynamic *CONTENT_DISPOSITION*   "content-disposition")
(def ^:dynamic *DISPOSITION_NOTIFICATION_TO*  "disposition-notification-to")
(def ^:dynamic *DISPOSITION_NOTIFICATION_OPTIONS*  "disposition-notification-options")
(def ^:dynamic *SIGNED_REC_MICALG* "signed-receipt-micalg")
(def ^:dynamic *MESSAGE_ID*   "message-id")
(def ^:dynamic *ORIGINAL_MESSAGE_ID*   "original-message-id")
(def ^:dynamic *RECEIPT_DELIVERY_OPTION*   "receipt-delivery-option")
(def ^:dynamic *DISPOSITION*  "disposition")
(def ^:dynamic *DATE*    "date")
(def ^:dynamic *MIME_VERSION*   "mime-version")
(def ^:dynamic *FINAL_RECIPIENT*   "final-recipient")
(def ^:dynamic *ORIGINAL_RECIPIENT*   "original-recipient")
(def ^:dynamic *RECV_CONTENT_MIC*   "received-content-mic")

(def ^:dynamic *RFC822* "rfc822")
(def ^:dynamic *RFC822_PFX* (str *RFC822* "; "))

(def ^:dynamic *APP_XML* "application/xml")
(def ^:dynamic *TEXT_PLAIN* "text/plain")
(def ^:dynamic *APP_OCTET* "application/octet-stream")
(def ^:dynamic *PKCS7SIG* "pkcs7-signature")
(def ^:dynamic *TEXT_HTML* "text/html")
(def ^:dynamic *TEXT_XML* "text/xml")
(def ^:dynamic *MSG_DISP* "message/disposition-notification")

(def ^:dynamic *ERROR*   "error")
(def ^:dynamic *FAILURE* "failure")
(def ^:dynamic *WARNING*  "warning")
(def ^:dynamic *HEADERS*  "headers")

(def ^:dynamic *ISO_8859_1* "iso-8859-1")
(def ^:dynamic *US_ASCII* "us-ascii")

(def ^:dynamic *CRLF* "\r\n")


(def ^:private _extRegex (Pattern/compile "^.*\\.([^.]+)$"))
(def ^:private _mime_cache (atom {}))


(defn- is-pkcs7mime? ""
  [^String s]
  (>= (.indexOf s "application/x-pkcs7-mime") 0))

(defn mime-cache "Cache of most MIME types."
  []
  @_mime_cache)

(defn get-charset "Get charset from this content-type string."
  ^String [^String cType]
  (let [ pos (-> (SU/nsb cType) (.toLowerCase) (.indexOf "charset="))
         rc "utf-8" ]
         ;;rc "ISO-8859-1" ]
    (if (> pos 0)
      (let [ s (.substring cType (+ pos 8)) p (StringUtils/indexOfAny s "; \t\r\n") ]
        (if (> p 0) (.substring s 0 p) s))
      rc)) )

(defn is-signed? "Returns true if this content-type indicates signed."
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (or (>= (.indexOf ct "multipart/signed") 0)
        (and (is-pkcs7mime? ct) (>= (.indexOf ct "signed-data") 0)))) )

(defn is-encrypted? "Returns true if this content-type indicates encrypted."
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (and (is-pkcs7mime? ct) (>= (.indexOf ct "enveloped-data") 0))) )

(defn is-compressed? "Returns true if this content-type indicates compressed."
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (and (>= (.indexOf ct "application/pkcs7-mime") 0) (>= (.indexOf ct "compressed-data") 0))) )

(defn is-mdn? "Returns true if this content-type indicates MDN."
  [^String cType]
  (let [ ct (.toLowerCase (SU/nsb cType)) ]
    (and (>= (.indexOf ct "multipart/report") 0) (>= (.indexOf ct "disposition-notification") 0))) )

(defn maybe-stream "Turn this object into some form of stream, if possible."
  ^InputStream [^Object obj]
  (cond
    (instance? String obj)
    (IO/streamify (CU/bytesify obj))

    (instance? InputStream obj)
    obj

    (instance? (MU/bytes-class) obj)
    (IO/streamify obj)

    :else
    nil))

(defn url-decode "URL decode this string."
  [^String u]
  (if (nil? u)
    nil
    (CU/Try!
      (URLDecoder/decode u "utf-8") )))

(defn url-encode "URL encode this string."
  [^String u]
  (if (nil? u)
    nil
    (CU/Try!
      (URLEncoder/encode u "utf-8") )))

(defn guess-mimetype "Guess the MIME type of file."
  (^String [^File file] (guess-mimetype file ""))
  (^String [^File file ^String dft]
    (let [ ^Pattern rgx _extRegex
           ^Matcher mc (.matcher rgx (.toLowerCase (.getName file)))
           ex (if (.matches mc) (.group mc 1) "")
           p (if (SU/hgl? ex) ((keyword ex) (mime-cache))) ]
      (if (SU/hgl? p) p dft))) )

(defn guess-contenttype "Guess the content-type of file."
  (^String [^File file] (guess-contenttype file "utf-8" "application/octet-stream" ))
  (^String [^File file ^String enc ^String dft]
    (let [ mt (guess-mimetype file)
           ct (if (SU/hgl? mt) mt dft) ]
      (if (not (.startsWith ct "text/")) ct (str ct "; charset=" enc)))) )

(defn setup-cache "Load file mime-types as a map."
  [^URL fileUrl]
  (with-open [ inp (.openStream fileUrl) ]
    (let [ ps (Properties.) ]
      (.load ps inp)
      (reset! _mime_cache (CU/into-map ps)))))






(def ^:private mime-eof nil)

