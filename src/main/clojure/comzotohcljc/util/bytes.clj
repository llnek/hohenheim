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

(ns ^{ :doc "Utililties for handling byte[] conversions to/from numbers."
      :author "kenl" }
  comzotohcljc.util.bytes)

(use '[clojure.tools.logging :only (info warn error debug)])
(import '(java.nio ByteBuffer CharBuffer) )
(import '(java.nio.charset Charset) )
(import '(java.io
  ByteArrayOutputStream ByteArrayInputStream
  DataOutputStream DataInputStream) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti write-bytes "Write this long value out as byte[]." class )

(defn to-bytes "Convert char[] to byte[]."

  [^chars chArray ^Charset charSetObj]

  (.array (.encode charSetObj (CharBuffer/wrap chArray)) ) )


(defn to-chars "Convert byte[] to char[]."

  [^bytes byteArray ^Charset charSetObj]

  (.array (.decode charSetObj (ByteBuffer/wrap byteArray)) ) )


(defn read-long "Return a long by scanning the byte[]."

  [^bytes byteArray]

  (.readLong (DataInputStream. (ByteArrayInputStream. byteArray)) ))


(defn read-int "Return an int by scanning the byte[]."

  [^bytes byteArray]

  (.readInt (DataInputStream. (ByteArrayInputStream. byteArray)) ))


(defmethod write-bytes Integer
  [num]
  (with-open [ baos (ByteArrayOutputStream. (int 4096)) ]
    (let [ ds (DataOutputStream. baos ) ]
      (.writeInt ds num)
      (.flush ds )
      (.toByteArray baos ) )))


(defmethod write-bytes Long
  [num]
  (with-open [ baos (ByteArrayOutputStream. (int 4096)) ]
    (let [ ds (DataOutputStream. baos ) ]
      (.writeLong ds num)
      (.flush ds )
      (.toByteArray baos ) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private bytes-eof nil)

