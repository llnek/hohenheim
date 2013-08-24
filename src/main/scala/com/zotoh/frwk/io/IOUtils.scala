/*??
 * COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

package com.zotoh.frwk.io

import org.apache.commons.lang3.StringUtils
import java.io._
import org.slf4j._
import org.apache.commons.io.FileUtils

object IOUtils {
  private val _DFT= 1L * 1024 * 1024 * 10 // 10M
  private var _streamLimit= _DFT

  def streamLimit() = _streamLimit
  def setStreamLimit(n:Long) {
    _streamLimit= if (n <= 0) _DFT else n
  }
  
  private var _wd = new File( System.getProperty("java.io.tmpdir"))
  def workDir() = _wd
  def setWorkDir(fpDir:File) { 
    _wd= fpDir
    _wd.mkdirs() 
  }
  
  def newTempFile(open:Boolean=false): (File,OutputStream) = {
    val f= mkTempFile()
    (f, if (open) new FileOutputStream(f) else null)
  }
  
  def mkTempFile(pfx:String="", sux:String=""): File = {
    File.createTempFile(
      if ( StringUtils.isEmpty(pfx)) "temp-" else pfx,
      if ( StringUtils.isEmpty(sux)) ".dat" else sux,
      workDir)
  }

  def listFiles(dir:File, ext:String, recurse:Boolean) = {
    FileUtils.listFiles(dir, Array(ext),recurse)
  }

}


sealed class IOUtils {}


