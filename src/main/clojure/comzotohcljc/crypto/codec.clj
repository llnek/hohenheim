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

  comzotohcljc.crypto.codec)

(use '[clojure.tools.logging :only (info warn error debug)])
(require '[clojure.math.numeric-tower :as math])

(import '(org.apache.commons.codec.binary Base64))
(import '(javax.crypto.spec SecretKeySpec))
(import '(org.jasypt.encryption.pbe StandardPBEStringEncryptor))
(import '(org.jasypt.util.text StrongTextEncryptor))
(import '(java.io ByteArrayOutputStream))
(import '(java.security SecureRandom))
(import '(javax.crypto Cipher))
(import '(org.mindrot.jbcrypt BCrypt))
(import '(org.bouncycastle.crypto.params DESedeParameters KeyParameter))
(import '(org.bouncycastle.crypto.paddings PaddedBufferedBlockCipher))
(import '(org.bouncycastle.crypto KeyGenerationParameters))
(import '(org.bouncycastle.crypto.engines DESedeEngine))
(import '(org.bouncycastle.crypto.generators DESedeKeyGenerator))
(import '(org.bouncycastle.crypto.modes CBCBlockCipher))
(import '(org.apache.commons.lang3 StringUtils))

(require '[ comzotohcljc.util.core :as CU])
(require '[ comzotohcljc.util.io :as IO])
(require '[ comzotohcljc.util.str :as SU])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private VISCHS
  (.toCharArray (str " @N/\\Ri2}aP`(xeT4F3mt;8~%r0v:L5$+Z{'V)\"CKI_c>z.*"
       "fJEwSU7juYg<klO&1?[h9=n,yoQGsW]BMHpXb6A|D#q^_d!-")))
(def ^:private VISCHS_LEN (alength VISCHS))

;;(def ^:private PCHS "abcdefghijklmnopqrstuvqxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`1234567890-_~!@#$%^&*()" )
(def ^:private PCHS "Ha$4Jep8!`g)GYkmrIRN72^cObZ%oXlSPT39qLMD&iC*UxKWhE#F5@qvV6j0f1dyBs-~tAQn(z_u" )
;;(def ^:private ACHS "abcdefghijklmnopqrstuvqxyz1234567890-_ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
(def ^:private ACHS "nhJ0qrIz6FmtPCduWoS9x8vT2-KMaO7qlgApVX5_keyZDjfE13UsibYRGQ4NcLBH" )

(def ^:private s_asciiChars (.toCharArray ACHS))
(def ^:private s_pwdChars (.toCharArray PCHS))

(def ^:private PWD_PFX "CRYPT:" )
(def ^:private PWD_PFXLEN (.length PWD_PFX))

(def ^:private T3_DES "TripleDES" ) ;; AES/ECB/PKCS5Padding/TripleDES
(def ^:private C_KEY "ed8xwl2XukYfdgR2aAddrg0lqzQjFhbs" )
(def ^:private C_ALGO T3_DES) ;; default javax supports this
;;(def ^:private ALPHA_CHS 26)


(defn- ensure-key-size [keystr algo]
  (let [ len (alength (CU/bytesify keystr)) ]
    (when (and (= T3_DES algo) (< len 24))
      (CU/throw-badarg "Encryption key length must be 24, when using TripleDES"))
    keystr))

(defn- keyAsBits [^String pwd algo]
  (let [ bits (CU/bytesify pwd) ]
    (if (and (= T3_DES algo) (> (alength bits) 24))
      (into-array Byte/TYPE (take 24 bits)) ;; only 24 bits wanted
      bits)))

(defprotocol BaseCryptor
  (decrypt [_ pkey cipherText] [_ cipherText] )
  (encrypt [_ pkey clearText] [_ clearText] )
  (algo [_] ))

;; BCrypt.checkpw(candidate, hashed)
(defprotocol PasswordAPI
  (toCharArray [_] )
  (encoded [_] )
  (stronglyHashed [_] )
  (hashed [_] )
  (text [_] ) )

(declare pwdify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caesar cipher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- identify-ch [pos] (aget VISCHS pos))

(defn- locate-ch [ch]
  (let [ idx (some (fn [i] (if (= ch (aget VISCHS i)) i nil)) (range VISCHS_LEN)) ]
    (if (nil? idx) -1 idx)))

(defn- slide-forward [delta cpos]
  (let [ ptr (+ cpos delta)
         np (if (>= ptr VISCHS_LEN) (- ptr VISCHS_LEN) ptr) ]
    (identify-ch np)))

(defn- slide-back [delta cpos]
  (let [ ptr (- cpos delta)
         np (if (< ptr 0) (+ VISCHS_LEN ptr) ptr) ]
    (identify-ch np)))

(defn- shiftenc [shiftpos delta cpos]
  (if (< shiftpos 0)
    (slide-forward delta cpos)
    (slide-back delta cpos)))

(defn- shiftdec [shiftpos delta cpos]
  (if (< shiftpos 0)
    (slide-back delta cpos))
    (slide-forward delta cpos) )

(defn caesar-encrypt "Encrypt clear text by character rotation."
  [^String text ^long shiftpos]
  (if (or (StringUtils/isEmpty text) (= shiftpos 0))
    text
    (let [ delta (mod (math/abs shiftpos) VISCHS_LEN)
           ca (.toCharArray text)
           out (amap ^chars ca pos ret
                  (let [ ch (aget ^chars ca pos)
                         p (locate-ch ch) ]
                    (if (< p 0)
                      ch
                      (shiftenc shiftpos delta p)))) ]
      (String. out))))

(defn caesar-decrypt "Decrypt text which was encrypted by the caesar method."
  [^String text ^long shiftpos]
  (if (or (StringUtils/isEmpty text) (= shiftpos 0))
    text
    (let [ delta (mod (math/abs shiftpos) VISCHS_LEN)
           ca (.toCharArray text)
           out (amap ^chars ca pos ret
                  (let [ ch (aget ^chars ca pos)
                         p (locate-ch ch) ]
                    (if (< p 0)
                      ch
                      (shiftdec shiftpos delta p)))) ]
      (String. out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jasypt cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- jaDecr [pkey text]
  (let [ ec (doto (StrongTextEncryptor.) (.setPassword pkey)) ]
    (.decrypt ec text)) )

(defn- jaEncr [pkey text]
  (let [ ec (doto (StrongTextEncryptor.) (.setPassword pkey)) ]
    (.encrypt ec text)) )

(defn jasypt-cryptor []
  (reify BaseCryptor

    (decrypt [this cipherText] (decrypt this C_KEY cipherText))
    (decrypt [_ pkey cipherText]
      (do
        (jaDecr pkey cipherText)) )
    (encrypt [this clearText] (encrypt this C_KEY clearText))
    (encrypt [_ pkey clearText]
      (do
        (jaEncr pkey clearText)) )
    (algo [_] "PBEWithMD5AndTripleDES") ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- getCipher [pkey mode algo]
  (let [ spec (SecretKeySpec. (keyAsBits pkey algo) algo)
         c (Cipher/getInstance algo) ]
    (.init c mode spec)
    c))

(defn- jcEncr [pkey text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ c (getCipher pkey (Cipher/ENCRYPT_MODE) algo )
           baos (IO/make-baos)
           p (CU/bytesify text)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (Base64/encodeBase64URLSafeString (.toByteArray baos)))) )

(defn- jcDecr [pkey encoded algo]
  (if (StringUtils/isEmpty encoded)
    encoded
    (let [ c (getCipher pkey (Cipher/DECRYPT_MODE) algo )
           baos (ByteArrayOutputStream. (int 4096))
           p (Base64/decodeBase64 encoded)
           out (byte-array (max 4096 (.getOutputSize c (alength p))))
           n (.update c p 0 (alength p) out 0) ]
      (when (> n 0) (.write baos out 0 n))
      (let [ n2 (.doFinal c out 0) ]
        (when (> n2 0) (.write baos out 0 n2)))
      (CU/stringify (.toByteArray baos)))) )

(defn java-cryptor []
  (reify BaseCryptor

    (decrypt [this cipherText] (decrypt this C_KEY cipherText))
    (decrypt [this pkey cipherText]
      (do
        (ensure-key-size pkey (algo this))
        (jcDecr pkey cipherText (algo this))) )
    (encrypt [this clearText] (encrypt this C_KEY clearText))
    (encrypt [this pkey clearText]
      (do
        (ensure-key-size pkey (algo this))
        (jcEncr pkey clearText (algo this))) )
    (algo [_] T3_DES) ) )
  ;;PBEWithMD5AndDES


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BC cryptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- bcDecr [pkey text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ cipher (doto (PaddedBufferedBlockCipher. (CBCBlockCipher. (DESedeEngine.)))
                    (.init false (KeyParameter. (keyAsBits pkey algo))))
           p (Base64/decodeBase64 text)
           out (byte-array 1024)
           baos (IO/make-baos)
           c (.processBytes cipher p 0 (alength p) out 0) ]
      (when (> c 0) (.write baos out 0 c))
      (let [ c2 (.doFinal cipher out 0) ]
        (when (> c2 0) (.write baos out 0 c2)))
      (CU/stringify (.toByteArray baos)))) )

(defn- bcEncr [pkey text algo]
  (if (StringUtils/isEmpty text)
    text
    (let [ cipher (doto (PaddedBufferedBlockCipher. (CBCBlockCipher. (DESedeEngine.)))
                    (.init true (KeyParameter. (keyAsBits pkey algo))))
           out (byte-array 4096)
           baos (IO/make-baos)
           p (CU/bytesify text)
           c (.processBytes cipher p 0 (alength p) out 0) ]
      (when (> c 0) (.write baos out 0 c))
      (let [ c2 (.doFinal cipher out 0) ]
        (when (> c2 0) (.write baos out 0 c2)) )
      (Base64/encodeBase64String (.toByteArray baos)))) )

(defn bouncy-cryptor []
  (reify BaseCryptor
    (decrypt [this cipherText] (decrypt this C_KEY cipherText))
    (decrypt [this pkey cipherText]
      (do
        (ensure-key-size pkey (algo this))
        (bcDecr pkey cipherText (algo this))) )
    (encrypt [this clearText] (encrypt this C_KEY clearText))
    (encrypt [this pkey clearText]
      (do
        (ensure-key-size pkey (algo this))
        (bcEncr pkey clearText (algo this))) )
    (algo [_] T3_DES) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- createXXX [^long len ^chars chArray]
  (cond
    (< len 0)
    nil

    (= len 0)
    ""

    :else
    (let [ r (SecureRandom/getInstance "SHA1PRNG")
           ostr (char-array len)
           bits (byte-array 4)
           cl (alength chArray)
           rc (amap ^chars ostr pos ret
                    (let [ n (mod (.nextInt r Integer/MAX_VALUE) cl) ]
                    (aget chArray n))) ]
      (String. rc))) )

(deftype Password [pwdStr pkey]
  Object
  (equals [this obj] (and (instance? Password obj) (= (.pwdStr this) (.pwdStr obj))) )
  (hashCode [this] (.hashCode (SU/nsb pwdStr)))
  (toString [this] (.text this))
  PasswordAPI
  (toCharArray [_] (if (nil? pwdStr) (char-array 0) (.toCharArray pwdStr)))
  (stronglyHashed [_]
    (if (nil? pwdStr)
      ""
      (BCrypt/hashpw pwdStr (BCrypt/gensalt 12))))
  (hashed [_]
    (if (nil? pwdStr)
      ""
      (BCrypt/hashpw pwdStr (BCrypt/gensalt 10))))
  (encoded [_]
    (if (StringUtils/isEmpty pwdStr)
      ""
      (str PWD_PFX (.encrypt (jasypt-cryptor) pkey pwdStr))))
  (text [_] (SU/nsb pwdStr)))

(defn pwdify "Create a password object."
  ([pwdStr] (pwdify pwdStr C_KEY))
  ([pwdStr pkey]
    (cond
      (StringUtils/isEmpty pwdStr)
      (Password. "" pkey)

      (.startsWith pwdStr PWD_PFX)
      (Password. (.decrypt (jasypt-cryptor) pkey (.substring pwdStr PWD_PFXLEN)) pkey)

      :else
      (Password. pwdStr pkey)) ))

(defn create-random-string ""
  [len]
  (createXXX len s_asciiChars))

(defn create-strong-pwd ""
  [len]
  (pwdify (createXXX len s_pwdChars)))




(def ^:private codec-eof nil)



