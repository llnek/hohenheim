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

(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.mvc.ios )


(import '(org.apache.commons.lang3 StringUtils))

(import '(com.zotoh.frwk.util CoreUtils))
(import '(java.net URLDecoder))
(import '(com.zotoh.hohenheim.io HTTPEvent IOSession Emitter))
(import '(com.zotoh.hohenheim.core Container))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.util.core :only (MuObj) ])

(require '[comzotohcljc.crypto.core :as CE])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.guids :as GU])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)


(def ^:private SESSION_COOKIE "hhh_ssid" )
(def ^:private SSID_FLAG "f_01ec")
(def ^:private TS_FLAG "f_684f" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol WebSession
  ""
  (setAttribute [_ k v] )
  (getAttribute [_ k] )
  (removeAttribute [_ k] )
  (clear [_] )
  (setMaxInactiveInterval [_ idleSecs] )
  (isNew [_] )
  (invalidate [_] )
  (getCreationTime [_]  )
  (getId [_] )
  (getLastAccessedTime [_] )
  (getMaxInactiveInterval [_] ))


(defn- resurrect [^comzotohcljc.hhh.mvc.ios.WebSession mvs
                  ^HTTPEvent evt]
  (let [ ctr (.container ^Emitter (.emitter evt))
         pkey (-> ctr (.getAppKey)(CU/bytesify))
         ck (.getCookie evt SESSION_COOKIE)
         cookie (SU/nsb (if-not (nil? ck) (.getValue ck)))
         pos (.indexOf cookie (int \-))
         [rc1 rc2] (if (< pos 0)
                     ["" ""]
                     [(.substring cookie 0 pos)
                      (.substring cookie (+ pos 1) )] ) ]
    (when (and (SU/hgl? rc1)
             (SU/hgl? rc2)
             (= (CE/gen-mac pkey rc2) rc1))
      (let [ ss (CoreUtils/splitNull (URLDecoder/decode rc2 "utf-8")) ]
        (doseq [ s (seq ss) ]
          (let [ [n v] (StringUtils/split ^String s ":") ]
            (.setAttribute mvs n v)))))
    (let [ ts (SU/nsb (.getAttribute mvs TS_FLAG))
           ^comzotohcljc.hhh.core.sys.Thingy
           netty (.emitter evt)
           idleSecs (.getAttr netty :cacheMaxAgeSecs)
           expired (if (SU/hgl? ts)
                     (< (CU/conv-long ts 0) (System/currentTimeMillis))
                     (and (number? idleSecs) (> idleSecs 0))) ]
      (if (and expired (number? idleSecs) (> idleSecs 0))
        (.setAttribute mvs TS_FLAG (+ (System/currentTimeMillis)
                                      (* idleSecs 1000)))))

    (.bindSession evt mvs)
    mvs))

(defn make-session []
  (let [ now (System/currentTimeMillis)
         impl (CU/make-mmap) ]
    (.mm-s impl SSID_FLAG (GU/new-uuid))
    (.mm-s impl :createTS now)
    (.mm-s impl :lastTS now)
    (.mm-s impl :valid false)
    (.mm-s impl :maxIdleSecs 3600)
    (.mm-s impl :newOne true)
    (with-meta
      (reify

        WebSession
          (setAttribute [_ k v] (.mm-s impl k v) )
          (getAttribute [_ k] (.mm-g impl k) )
          (removeAttribute [_ k] (.mm-r impl k) )
          (clear [_] (.mm-c impl))
          (setMaxInactiveInterval [_ idleSecs]
            (.mm-s impl
                   :maxInactiveInterval
                   (if (and (number? idleSecs)(> idleSecs 0)) idleSecs -1)))
          (isNew [_] (.mm-g impl :newOne))
          (invalidate [_]
            (.mm-s impl :createTS 0)
            (.mm-s impl :valid false)
            (.mm-s impl :newOne true)
            (.mm-c impl) )
          (getCreationTime [_]  (.mm-g impl :createTS))
          (getId [_] (.mm-g impl SSID_FLAG))
          (getLastAccessedTime [_] (.mm-g impl :lastTS))
          (getMaxInactiveInterval [_] (* 1000 (.mm-g impl :maxIdleSecs)))

        IOSession
          (handleResult [_ evt res] nil)
          (handleEvent [this evt]
            (resurrect this evt)))

      { :typeid :czc.hhh.io/Session } )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ios-eof nil)

