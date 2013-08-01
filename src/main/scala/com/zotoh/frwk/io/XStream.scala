/*??
 * COPYRIGHT (C) 2012 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

import java.io.{File,FileInputStream,IOException,InputStream}
import org.apache.commons.io.{FileUtils=>FUT}
import org.apache.commons.io.{IOUtils=>IOU}
import java.nio.charset.Charset


/**
 * Wrapper on top of a File input stream such that it can
 * delete itself from the file system when necessary.
 *
 * @author kenl
 *
 */
class XStream(protected var _fn:File, protected var _deleteFile:Boolean) extends InputStream {

  @transient private var _inp:InputStream = null
  protected var _closed = true
  private var pos = 0L

  def this(f:File) {
    this(f,false)
  }
  
  override def available() = {
    pre()
    _inp.available()
  }

  override def read() = {
    pre()
    val r = _inp.read()
    pos += 1
    r
  }

  override def read(b:Array[Byte], offset:Int, len:Int) = if (b==null) -1 else {
    pre()
    val r = _inp.read(b, offset, len)
    pos = if (r== -1 ) -1 else { pos + r }
    r
  }

  override def read(b:Array[Byte]) = if (b==null) -1 else read(b, 0, b.length)

  override def skip(n:Long) = if (n < 0L) -1L else {
    pre()
    val r= _inp.skip(n)
    if (r > 0L) { pos +=  r }
    r
  }

  override def close() {
    IOU.closeQuietly(_inp)
    _inp= null
    _closed= true
  }

  override def mark(readLimit:Int) {
    if (_inp != null) {
      _inp.mark(readLimit)
    }
  }

  override def reset() {
    close()
    _inp= new FileInputStream(_fn)
    _closed=false
    pos=0L
  }

  override def markSupported() = true

  def setDelete(dfile:Boolean): this.type = { _deleteFile = dfile ; this }

  def delete() {
    close()
    if (_deleteFile && _fn != null) {
      FUT.deleteQuietly(_fn)
    }
  }

  def filename() = if (_fn != null) _fn.getCanonicalPath else ""

  override def toString() = filename()

  def getPosition() =  pos

  override def finalize() { delete() }

  private def pre() {
    if (_closed) { ready() }
  }

  private def ready() {
    reset()
  }

}

