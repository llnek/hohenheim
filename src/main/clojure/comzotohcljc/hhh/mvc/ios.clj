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

(def ^:private SESSION_COOKIE "hhh_ssref" )
(def ^:private SSID_FLAG "f_01ecf22f")
(def ^:private TS_FLAG "f_684f11a0" )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HttpSession
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


(defn- resurrect [^comzotohcljc.hhh.mvc.ios.HttpSession mvs ^HTTPEvent evt]

  (let [ ^Emitter em (.emitter evt) ^Container ctr (.container em)
         pkey (-> ctr (.getAppKey)(CU/bytesify))
         ck (.getCookie evt SESSION_COOKIE)
         cookie (SU/nsb (if-not (nil? ck) (.getValue ck)))
         ^comzotohcljc.hhh.core.sys.Thingy
         netty (.emitter evt)
         idleSecs (.getAttr netty :cacheMaxAgeSecs)
         pos (.indexOf cookie (int \-))
         [rc1 rc2] (if (< pos 0)
                     ["" ""]
                     [ (.substring cookie 0 pos) (.substring cookie (+ pos 1) )] ) ]
    (when (and (SU/hgl? rc1)
             (SU/hgl? rc2)
             (= (CE/gen-mac pkey rc2) rc1))
      (let [ ss (CoreUtils/splitNull (URLDecoder/decode rc2 "utf-8")) ]
        (doseq [ s (seq ss) ]
          (let [ [n v] (StringUtils/split ^String s ":") ]
            (.setAttribute mvs n v)))))
    (let [ ts (SU/nsb (.getAttribute mvs TS_FLAG))
           expired (if (SU/hgl? ts)
                     (< (CU/conv-long ts 0) (System/currentTimeMillis))
                     (and (number? idleSecs) (> idleSecs 0))) ]
      (if (and expired (number? idleSecs) (> idleSecs 0))
        (.setAttribute mvs TS_FLAG (+ (System/currentTimeMillis)  (* idleSecs 1000)))))

    (.bindSession evt mvs)
    mvs))


(defn make-session []
  (let [ impl (CU/make-mmap) now (System/currentTimeMillis) ]
    (.mm-s impl SSID_FLAG (GU/new-uuid))
    (.mm-s impl :createTS now)
    (.mm-s impl :lastTS now)
    (.mm-s impl :valid false)
    (.mm-s impl :maxIdleSecs 3600)
    (.mm-s impl :newOne true)
    (with-meta
      (reify

        HttpSession
          (setAttribute [_ k v] (.mm-s impl k v) )
          (getAttribute [_ k] (.mm-g impl k) )
          (removeAttribute [_ k] (.mm-r impl k) )
          (clear [_] (.mm-c impl))
          (setMaxInactiveInterval [_ idleSecs]
            (.mm-s impl :maxInactiveInterval (if (and (number? idleSecs)(> idleSecs 0)) idleSecs -1)))
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
          (handleResult [_ evt res] )
          (handleEvent [this evt]
            (resurrect this evt)))

      { :typeid :czc.hhh.io/Session } )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private ios-eof nil)

