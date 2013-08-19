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

(defn- initRoute [rc path]
  (let [ tknz (StringTokenizer path "/" true) ]
    (while (.hasMoreTokens tknz)
      (let [ t (.nextToken tknz) ]
        (if (= t "/") (.append buff "/")
          (do
        if (t.startsWith(":")) {
          cg += 1
          gn= t.substring(1)
          _placeholders.add( ( cg , gn ) )
          t = "({" + gn + "}[^/]+)"
        } else {
          val c= STU.countMatches(t, "(")
          if (c > 0) {
            cg += c
          }
        }
        buff.append(t)
      }
    }
    tlog.debug("Route added: {}\ncanonicalized to: {}{}", _path, buff,"")
    _path=buff.toString
    _regex= new Pattern(_path)

(defn- mkRoute [stat path flds]
  (let [ tpl (:template flds)
         verb (:verb flds)
         mpt (:mount flds)
         pipe (:pipe flds)
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

(defn load-routes [^File file]
  (let [ stat  (-> file (.getName)(.startsWith "static-"))
         cf (WI/parse-inifile file) ]
    (doseq [ s (seq (.sectionKeys cf)) ]
      (mkRoute stat s (.getSection cf s)))
    ))

(defn make-route-info [path verb handler]
  (let [ verbList (.toLowerCase verb)
         impl (CU/make-mmap) ]
    (with-meta
      (reify
        ZZZ


        (resemble [mtd path]
          (let [ ^Pattern rg (.mm-g impl :regex)
                 m (.matcher rg path) ]
            (if (and (.matches m)
                     (or (= "*" verbList) (>= (.indexOf verbList (.toLowerCase mtd)) 0)))
              m
              nil)))


  private def initialize() {
    val tknz = new StringTokenizer(_path, DELIM, true)
    val buff= new StringBuilder(512)
    var t=""
    var gn= ""
    var cg=0
    while (tknz.hasMoreTokens ) {
      t=tknz.nextToken()
      if (t == DELIM) { buff.append(DELIM) } else {
        if (t.startsWith(":")) {
          cg += 1
          gn= t.substring(1)
          _placeholders.add( ( cg , gn ) )
          t = "({" + gn + "}[^/]+)"
        } else {
          val c= STU.countMatches(t, "(")
          if (c > 0) {
            cg += c
          }
        }
        buff.append(t)
      }
    }
    tlog.debug("Route added: {}\ncanonicalized to: {}{}", _path, buff,"")
    _path=buff.toString
    _regex= new Pattern(_path)
  }

  def setStatic(b:Boolean): this.type = {
    _staticFile=b
    this
  }
  def isStatic() = _staticFile

  def resemble(mtd:String, path:String): Option[Matcher] = {
    val m=_regex.matcher(path)
    if (m.matches() &&
      _verbArr.find { (s) =>s=="*" || s == mtd.uc }.isDefined ) {
      Some(m)
    } else {
      None
    }
  }

  def mountPoint_=(s:String) {   _mountPt = nsb(s) }
  def mountPoint = _mountPt

  def template_=(s:String) {   _tpl = nsb(s) }
  def template = _tpl

  def pattern() = _regex
  def pipeline() = _pipe
  def path() = _path
  def verb() = _verb

  def resolveMatched(mc:Matcher) = {
    val rc= mutable.HashMap[String,String]()
    val gc = mc.groupCount()
    _placeholders.foreach { (t) =>
      rc.put( t._2, nsb ( mc.group(t._2) ) )
//      if (t._1 <= gc) {
//        rc.put( t._2, mc.group(t._1) )
//      }
    }
    rc.toMap
  }

  /*
   *  path can be /host.com/abc/:id1/gg/:id2
   */
}





