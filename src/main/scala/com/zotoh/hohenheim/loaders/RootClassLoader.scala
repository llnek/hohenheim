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


/**
 * @author kenl
 */
class RootClassLoader(par:ClassLoader) extends AbstractClassLoader( par) {

  val base=System.getProperty("hohenheim.home","")
  if (base.length > 0) { load(base) }

  def configure(baseDir:String) {
    load( baseDir)
  }

  private def load(baseDir:String) {
    val p= new File(baseDir, "patch")
    val d= new File(baseDir, "dist")
    val b= new File(baseDir, "lib")

    if (!_loaded) {
      findUrls(p).findUrls(d).findUrls(b)
    }

    _loaded=true
  }

}

/**
 * @author kenl
 */
class ExecClassLoader(par:ClassLoader) extends AbstractClassLoader( new RootClassLoader( par)) {

  val base=System.getProperty("hohenheim.home","")
  if (base.length > 0) { load(base) }

  private def load(base:String) {
    val p= new File(base, "exec")

    if (!_loaded) {
      findUrls(p)
    }

    _loaded=true
  }

  def configure(baseDir:String) {
    load(baseDir)
  }

}

