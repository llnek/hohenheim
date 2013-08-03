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

(ns ^{ :doc "Functions to load and query a .ini file."
       :author "kenl" }

  comzotohcljc.util.ini)

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.net URL))
(import '(java.io File IOException InputStreamReader
  LineNumberReader PrintStream))
(import '(com.zotoh.frwk.util NCMap))
(import '(java.util Map LinkedHashMap))

(require '[ comzotohcljc.util.files :as FU])
(require '[ comzotohcljc.util.core :as CU])
(require '[ comzotohcljc.util.str :as SU])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(defmulti parse-inifile "Parse a INI config file." class)

(defprotocol IWin32Conf "A Windows INI file object."

  (getSection [_ sectionName] )
  (sectionKeys [_ ] )
  (dbgShow [_])
  (getString [_ sectionName property] )
  (getLong [_ sectionName property] )
  (getBool [_ sectionName property] )
  (getDouble [_ sectionName property] )
  (optString [_ sectionName property dft] )
  (optLong [_ sectionName property dft] )
  (optBool [_ sectionName property dft] )
  (optDouble [_ sectionName property dft] ) )


(defn- throwBadIni [^LineNumberReader rdr]
  (throw (IOException. (str "Bad ini line: " (.getLineNumber rdr)))))

(defn- throwBadKey [k] (throw (Exception. (str "No such property " k "."))))
(defn- throwBadMap [s] (throw (Exception. (str "No such section " s "."))))

(defn- maybeSection [^LineNumberReader rdr ^Map ncmap ^String line]
  (let [ s (StringUtils/trim (StringUtils/strip line "[]")) ]
    (when (StringUtils/isEmpty s) (throwBadIni rdr))
    (if-not (.containsKey ncmap s) (.put ncmap s (NCMap.)))
    s))

(defn- maybeLine [^LineNumberReader rdr ^Map ncmap ^Map section ^String line]
  (let [ ^Map kvs (.get ncmap section) ]
    (when (nil? kvs) (throwBadIni rdr))
    (let [ pos (.indexOf line (int \=))
           nm (if (> pos 0) (.trim (.substring line 0 pos)) "" ) ]
        (when (StringUtils/isEmpty nm) (throwBadIni rdr))
        (.put kvs nm (.trim (.substring line (+ pos 1)))) )) )

(defn- evalOneLine
  ^String [^LineNumberReader rdr ^Map ncmap ^String line ^String curSec]
  (let [ ln (.trim line) ]
    (cond
      (or (StringUtils/isEmpty ln) (.startsWith ln "#"))
      curSec

      (.matches ln "^\\[.*\\]$")
      (maybeSection rdr ncmap ln)

      :else
      (do (maybeLine rdr ncmap curSec ln) curSec)

      )) )


(defn- hasKV [^Map m k]
  (let [ kn (name k) ]
    (if (or (nil? kn) (nil? m)) nil (.containsKey m kn)) ))

(defn- getKV
  ^String [^comzotohcljc.util.ini.IWin32Conf cf s k err]

  (let [ kn (name k)
         sn (name s)
         ^Map mp (.getSection cf sn) ]
    (cond
      (nil? mp) (if err (throwBadMap sn) nil)
      (nil? k) (if err (throwBadKey "") nil)
      (not (hasKV mp k)) (if err (throwBadKey kn) nil)
      :else (SU/nsb (.get mp kn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-winini [^Map mapOfSections]
  (reify IWin32Conf

    (getSection [_ sectionName]
      (if (nil? sectionName)
        nil
        (let [ m (.get mapOfSections (name sectionName)) ]
          (if (nil? m) nil (into {} m)))))

    (sectionKeys [_] (.keySet mapOfSections))

    (getString [this section property]
      (SU/nsb (getKV this section property true)))

    (optString [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc) dft rc)))

    (getLong [this section property]
      (CU/conv-long (getKV this section property true) 0))

    (optLong [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc)
          dft
          (CU/conv-long rc 0))))

    (getDouble [this section property]
      (CU/conv-double (getKV this section property true) 0.0))

    (optDouble [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc)
          dft
          (CU/conv-double rc 0.0))))

    (getBool [this section property]
      (CU/conv-bool (getKV this section property true) false))

    (optBool [this section property dft]
      (let [ rc (getKV this section property false) ]
        (if (nil? rc)
          dft
          (CU/conv-bool rc false))))

    (dbgShow [_]
      (let [ buf (StringBuilder.) ]
        (doseq [ [k v] (seq mapOfSections) ]
          (do
            (.append buf (str "[" (name k) "]\n"))
            (doseq [ [x y] (seq v) ]
              (.append buf (str (name x) "=" y)))
            (.append buf "\n")))
        (println buf)))
  ))

(defmethod parse-inifile String
  ^comzotohcljc.util.ini.IWin32Conf [^String fpath]
  (if (nil? fpath)
    nil
    (parse-inifile (File. fpath))))

(defmethod parse-inifile File
  ^comzotohcljc.util.ini.IWin32Conf [^File file]
  (if (or (nil? file) (not (FU/file-read? file)))
    nil
    (parse-inifile (.toURL (.toURI file)))))

(defn- parseIniFile
  ^comzotohcljc.util.ini.IWin32Conf [^URL fUrl]
  (with-open [ inp (.openStream fUrl) ]
    (let [ rdr (LineNumberReader. (InputStreamReader. inp "utf-8"))
           total (NCMap.) ]
    (loop [ curSec "" line (.readLine rdr)  ]
      (if (nil? line)
        (make-winini total)
        (recur (evalOneLine rdr total line curSec) (.readLine rdr) )))) ))


(defmethod parse-inifile URL
  ^comzotohcljc.util.ini.IWin32Conf [^URL fileUrl]
  (if (nil? fileUrl)
    nil
    (parseIniFile fileUrl)))





(def ^:private ini-eof nil)

