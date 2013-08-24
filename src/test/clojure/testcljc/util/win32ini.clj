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

(ns testcljc.util.win32ini)
(use '[clojure.test])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.ini :as WI])

(def ^:private INIFILE (WI/parse-inifile (CU/rc-url "com/zotoh/frwk/util/sample.ini")))

(deftest testutil-wi32ini

(is (= (count (.sectionKeys ^comzotohcljc.util.ini.IWin32Conf INIFILE)) 2))

(is (map? (.getSection ^comzotohcljc.util.ini.IWin32Conf  INIFILE "operating systems")))
(is (map? (.getSection ^comzotohcljc.util.ini.IWin32Conf  INIFILE "boot loader")))

(is (true? (.endsWith ^String (.getString ^comzotohcljc.util.ini.IWin32Conf  INIFILE "boot loader" "default") "WINDOWS")))

(is (true? (= (.getLong ^comzotohcljc.util.ini.IWin32Conf  INIFILE "boot loader" "timeout") 30)))



)

(def ^:private win32ini-eof nil)

;;(clojure.test/run-tests 'testcljc.util.win32ini)

