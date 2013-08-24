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

(ns testcljc.crypto.cryptostuff)

(use '[clojure.test])
(import '(java.security Policy KeyStore SecureRandom MessageDigest
  KeyStore$PrivateKeyEntry KeyStore$TrustedCertificateEntry))
(import '(java.util Date GregorianCalendar))
(import '(java.io File))
(require '[comzotohcljc.crypto.codec :as RT])
(require '[comzotohcljc.crypto.stores :as ST])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.crypto.core :as RU])


(def ^:private ROOTPFX (CU/rc-bytes "com/zotoh/frwk/crypto/test.pfx"))
(def ^:private ROOTJKS (CU/rc-bytes "com/zotoh/frwk/crypto/test.jks"))
(def ^:private ENDDT (.getTime (GregorianCalendar. 2050 1 1)))
(def ^:private TESTPWD (RT/pwdify "secretsecretsecretsecretsecret"))
(def ^:private HELPME (RT/pwdify "helpme"))
(def ^:private SECRET (RT/pwdify "secret"))

(def ^:private ROOTCS
  (ST/make-crypto-store (RU/init-store! (RU/get-pkcsStore) ROOTPFX HELPME) HELPME))

(def ^:private ROOTKS
  (ST/make-crypto-store (RU/init-store! (RU/get-jksStore) ROOTJKS HELPME) HELPME))

(deftest testcrypto-cryptostuff

(is (not (= "heeloo, how are you?" (RT/caesar-decrypt (RT/caesar-encrypt "heeloo, how are you?" 709394) 666))))
(is (= "heeloo, how are you?" (RT/caesar-decrypt (RT/caesar-encrypt "heeloo, how are you?" 709394) 709394)))

(is (= "heeloo" (let [ c (RT/jasypt-cryptor) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (RT/jasypt-cryptor) ]
                      (.decrypt c SECRET (.encrypt c SECRET "heeloo")))))

(is (= "heeloo" (let [ c (RT/java-cryptor) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (RT/java-cryptor) ]
                      (.decrypt c TESTPWD (.encrypt c TESTPWD "heeloo")))))

(is (= "heeloo" (let [ c (RT/bouncy-cryptor) ]
                      (.decrypt c (.encrypt c "heeloo")))))

(is (= "heeloo" (let [ c (RT/bouncy-cryptor) ]
                      (.decrypt c TESTPWD (.encrypt c TESTPWD "heeloo")))))

(is (= (.length ^String (.text (RT/create-strong-pwd 16))) 16))
(is (= (.length (RT/create-random-string 64)) 64))

(is (satisfies? comzotohcljc.crypto.codec.PasswordAPI (RT/pwdify "secret-text")))
(is (.startsWith ^String (.encoded ^comzotohcljc.crypto.codec.PasswordAPI (RT/pwdify "secret-text")) "CRYPT:"))


(is (= "SHA-512" (.getAlgorithm (RU/make-MsgDigest RU/*SHA_512*))))
(is (= "MD5" (.getAlgorithm (RU/make-MsgDigest RU/*MD_5*))))

(is (> (RU/next-serial) 0))

(is (instance? SecureRandom (RU/get-srand)))

(is (> (.length (RU/new-alias)) 0))

(is (= "PKCS12" (.getType (RU/get-pkcsStore))))
(is (= "JKS" (.getType (RU/get-jksStore))))

(is (instance? Policy (RU/make-easyPolicy)))

(is (> (.length (RU/gen-mac (CU/bytesify "secret") "heeloo world")) 0))
(is (> (.length (RU/gen-hash "heeloo world")) 0))

(is (not (nil? (RU/make-keypair "RSA" 1024))))

(is (let [ v (RU/make-csrreq 1024 "C=AU,ST=NSW,L=Sydney,O=Google,OU=HQ,CN=www.google.com" "PEM") ]
          (and (= (count v) 2) (> (alength ^bytes (first v)) 0) (> (alength ^bytes (nth v 1)) 0))) )

(is (let [ fout (IO/make-tmpfile "kenl" ".p12")]
      (RU/make-ssv1PKCS12 (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" HELPME 1024 fout)
      (> (.length fout) 0)))

(is (let [ fout (IO/make-tmpfile "" ".jks") ]
      (RU/make-ssv1JKS (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" SECRET 1024 fout)
            (> (.length fout) 0)))

(is (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity ^comzotohcljc.crypto.stores.CryptoStore ROOTCS 
                           ^String (first (.keyAliases ^comzotohcljc.crypto.stores.CryptoStore ROOTCS)) 
                           HELPME)
       fout (IO/make-tmpfile "" ".p12")
       pk (.getPrivateKey pke)
       cs (.getCertificateChain pke) ]
            (RU/make-ssv3PKCS12 (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" SECRET 1024  cs pk fout)
              (> (.length fout) 0)))

(is (let [ ^KeyStore$PrivateKeyEntry pke (.keyEntity  ^comzotohcljc.crypto.stores.CryptoStore ROOTKS 
                           ^String (first (.keyAliases  ^comzotohcljc.crypto.stores.CryptoStore ROOTKS)) 
                           HELPME)
       fout (IO/make-tmpfile "" ".jks")
       pk (.getPrivateKey pke)
       cs (.getCertificateChain pke) ]
            (RU/make-ssv3JKS (Date.) ENDDT "C=AU,ST=NSW,L=Sydney,O=Google" SECRET 1024  cs pk fout)
              (> (.length fout) 0)))

(is (let [ ^File fout (IO/make-tmpfile "" ".p7b") ]
        (RU/export-pkcs7 (CU/rc-url "com/zotoh/frwk/crypto/test.pfx") HELPME fout)
          (> (.length fout) 0)))


)

(def ^:private cryptostuff-eof nil)

;;(clojure.test/run-tests 'testcljc.crypto.cryptostuff)

