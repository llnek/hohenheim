/*??
 * COPYRIGHT (C) 2012-2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
 *
 * THIS IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
 * MODIFY IT UNDER THE TERMS OF THE APACHE LICENSE,
 * VERSION 2.0 (THE "LICENSE").
 *
 * THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
 * BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * SEE THE LICENSE FOR THE SPECIFIC LANGUAGE GOVERNING PERMISSIONS
 * AND LIMITATIONS UNDER THE LICENSE.
 *
 * You should have received a copy of the Apache License
 * along with this distribution; if not, you may obtain a copy of the
 * License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 ??*/

package com.zotoh.hohenheim.loaders

import scala.collection.JavaConversions._
import scala.collection.mutable
import java.net.URLClassLoader
import java.io.File
import java.net.URL
import java.io.FilenameFilter

/**
 * @author kenl
 */
abstract class AbstractClassLoader(par:ClassLoader) extends URLClassLoader( Array[URL]() ,par) {

  protected var _loaded=false

  def findUrls(dir:File): this.type = {
    val seq= dir.listFiles( new FilenameFilter() {
      def accept(f:File,n:String) = n.endsWith(".jar")
    })
    seq.foreach( addUrl _ )
    this
  }

  def addUrl(f:File): this.type = {
    addURL( f.toURI.toURL)
    this
  }

}

