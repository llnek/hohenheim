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
  comzotohcljc.hhh.mvc.routes )

(import '(org.apache.commons.lang3 StringUtils))
(import '(java.io File))
(import '(jregex Matcher Pattern))
(import '(java.util StringTokenizer))

(use '[clojure.tools.logging :only (info warn error debug)])
(use '[comzotohcljc.util.core :only (MuObj) ])
(require '[comzotohcljc.util.core :as CU])
(require '[comzotohcljc.util.str :as SU])
(require '[comzotohcljc.util.ini :as WI])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol RouteInfo
  ""
  (getHandler [_] )
  (getPath [_] )
  (getVerbs [_] )
  (resemble? [_ mtd path] )
  (collect [_ matcher] ))


(defn- make-route-info [path ^String verb handler]
  (let [ verbList (.toLowerCase verb)
         impl (CU/make-mmap) ]
    (with-meta
      (reify

        MuObj

        (setf! [_ k v] (.mm-s impl k v) )
        (seq* [_] (seq (.mm-m* impl)))
        (getf [_ k] (.mm-g impl k) )
        (clrf! [_ k] (.mm-r impl k) )
        (clear! [_] (.mm-c impl))

        RouteInfo

        (getHandler [_] handler)
        (getPath [_] path)
        (getVerbs [_] verb)

        (resemble? [_ mtd path]
          (let [ ^Pattern rg (.mm-g impl :regex)
                 m (.matcher rg path) ]
            (if (and (.matches m)
                     (or (= "*" verbList)
                         (>= (.indexOf verbList 
                                       (.toLowerCase ^String mtd)) 0)))
              m
              nil)))

        (collect [_ mc]
          (let [ ph (.mm-g impl :placeHolders)
                 ^Matcher mmc mc
                 gc (.groupCount mmc) ]
            (with-local-vars [ rc (transient {}) r2 "" ]
              (doseq [ h (seq ph) ]
                (var-set r2 (last h))
                (var-set rc
                         (assoc! rc
                                 @r2
                                 (SU/nsb (.group mmc ^String @r2)))))
              (persistent! rc)))) )
      { :typeid :czc.hhh.mvc/RouteInfo } )) )


(defn- initRoute

  [^comzotohcljc.util.core.MuObj rc
   ^String path]

  (let [ tknz (StringTokenizer. path "/" true)
         buff (StringBuilder.)
         phs (transient []) ]
    (with-local-vars [ cg 0 gn "" ts "" ]
      (while (.hasMoreTokens tknz)
        (var-set ts (.nextToken tknz))
        (if (= @ts "/")
          (.append buff "/")
          (do
            (if (.startsWith ^String @ts ":")
              (do
                (var-set gn (.substring ^String @ts 1))
                (var-set cg (inc @cg))
                (var-set phs (conj! @phs [ @cg @gn ] ))
                (var-set ts  (str "({" @gn "}[^/]+)")))
              (let [ c (StringUtils/countMatches @ts "(") ]
                (if (> c 0)
                  (var-set cg (+ @cg c)))))
            (.append buff @ts)))))
    (let [ pp (.toString buff) ]
      (debug "route added: " path " \ncanonicalized to: " pp)
      (.setf! rc :path pp)
      (.setf! rc :regex (Pattern. pp)))
    (.setf! rc :placeHolders (persistent! phs))
    rc))

(defn- mkRoute [stat path flds]
  (let [ tpl (:template flds)
         verb (:verb flds)
         mpt (:mount flds)
         pipe (:pipe flds)
         ^comzotohcljc.util.core.MuObj 
         rc (make-route-info path verb pipe) ]
    (if stat
      (do
        (.setf! rc :mountPoint mpt)
        (.setf! rc :static true)
        (CU/test-nestr "static-route mount point" mpt))
      (do
        (CU/test-nestr "http method for route" verb)
        (CU/test-nestr "pipeline for route" pipe)))
    (when (SU/hgl? tpl)
      (.setf! rc :template tpl))
    (initRoute rc path)
    rc))

;;
;; path can be /host.com/abc/:id1/gg/:id2
;;
(defn load-routes [^File file]
  (let [ stat  (-> file (.getName)(.startsWith "static-"))
         cf (WI/parse-inifile file) ]
    (doseq [ s (seq (.sectionKeys cf)) ]
      (mkRoute stat s (.getSection cf s)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private routes-eof nil)



