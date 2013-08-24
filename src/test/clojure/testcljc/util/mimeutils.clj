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

(ns testcljc.util.mimeutils)

(use '[clojure.test])
(import '(java.io File InputStream))
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.io :as IO])
(require '[comzotohcljc.util.mime :as MU])


(eval '(MU/setup-cache (CU/rc-url "com/zotoh/frwk/mime/mime.properties")))

(deftest testutil-mimeutils

(is (= "utf-16" (MU/get-charset "text/plain; charset=utf-16")))

(is (true? (MU/is-signed? "saljas application/x-pkcs7-mime laslasdf lksalfkla multipart/signed signed-data ")))
(is (true? (MU/is-encrypted? "saljas laslasdf lksalfkla application/x-pkcs7-mime  enveloped-data ")))
(is (true? (MU/is-compressed? "saljas laslasdf lksalfkla application/pkcs7-mime compressed-data")))
(is (true? (MU/is-mdn? "saljas laslasdf lksalfkla multipart/report   disposition-notification    ")))

(is (instance? InputStream (MU/maybe-stream (IO/streamify (CU/bytesify "hello")))))
(is (instance? InputStream (MU/maybe-stream (CU/bytesify "hello"))))
(is (instance? InputStream (MU/maybe-stream "hello")))
(is (not (instance? InputStream (MU/maybe-stream 3))))

(is (= "a b" (MU/url-decode (MU/url-encode "a b"))))

(is (>= (.indexOf (MU/guess-mimetype (File. "/tmp/abc.jpeg")) "image/") 0))
(is (> (.indexOf (MU/guess-contenttype (File. "/tmp/abc.pdf")) "/pdf") 0))





)

(def ^:private mimeutils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.mimeutils)

