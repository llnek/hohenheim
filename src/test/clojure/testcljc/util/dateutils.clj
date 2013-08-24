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

(ns testcljc.util.dateutils)

(import '(java.util Date))
(use '[clojure.test])
(require '[comzotohcljc.util.dates :as DU])

(deftest testutil-dateutils

(is (false? (DU/leap-year? 1999)))
(is (true? (DU/leap-year? 2000)))
(is (true? (DU/leap-year? 2020)))
(is (instance? Date (DU/parse-date "1999/12/12 13:13:13" "yyyy/MM/dd HH:mm:ss")))
(is (instance? String (DU/fmt-date (Date.) "yyyy/MM/dd HH:mm:ss Z")))


)

(def ^:private dateutils-eof nil)

;;(clojure.test/run-tests 'testcljc.util.dateutils)

