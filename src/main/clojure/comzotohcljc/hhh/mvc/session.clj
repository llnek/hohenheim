(ns ^{ :doc ""
       :author "kenl" }
  comzotohcljc.hhh.mvc.io )

(defn- resurrect [^HTTPEvent evt]
  (let [ ck (.getCookie evt SESSION_COOKIE)
         cookie (SU/nsb (if-not (nil? ck) (.getValue ck)))
         ^comzotohcljc.hhh.core.sys.Thingy
         netty (.emitter evt)
         idleSecs (.getAttr netty :cacheMaxAgeSecs)
         pos (.indexOf cookie "-")
         ss (MVCSession.)
    val rc = if (pos < 0) ("","") else {
      ( cookie.substring(0,pos),  cookie.substring(pos+1) )
    }
    
    if (STU.isNotEmpty( rc._1) && STU.isNotEmpty( rc._2 )) {
      val k= evt.emitter.container.getAppKey.getBytes("utf-8")
      if ( same( genMAC(k, rc._2) , rc._1) ) {
        STU.split( decode( rc._2, "utf-8") , NV_SEP).foreach { (x) =>
          STU.split(x, ":") match {
            case Array(n,v) =>ss.setAttribute(n, v)
            case _ =>
          }
        }
      }
    }
    
    val expired= ss.getAttribute(TS_FLAG) match {
      case Some(s:String) =>s.toLong < System.currentTimeMillis
      case _ =>if (idleSecs > 0) true else false
    }
    if (expired && idleSecs > 0) {
      ss.setAttribute(TS_FLAG, System.currentTimeMillis  + idleSecs*1000L )
    }    
    
    evt.bindSession(ss)
    evt
  }



(def ^:private io-eof nil)

