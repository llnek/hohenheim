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

import java.io.{ IOException, CharArrayWriter, ByteArrayOutputStream=>ByteArrayOS,File,InputStream,ByteArrayInputStream=>ByteArrayIS}
import org.apache.commons.lang3.{StringUtils=>STU}
import org.apache.commons.io.{FileUtils=>FUT,IOUtils=>IOU}
import org.slf4j._


/**
 * Wrapper structure to abstract a piece of data which can be a file
 * or a memory byte[].  If the data is byte[], it will also be
 * compressed if a certain threshold is exceeded.
 *
 * @author kenl
 *
 */

object XData {
  private val _log= LoggerFactory.getLogger(classOf[XData])
}

@SerialVersionUID(-8637175588593032279L) class XData() extends Serializable {

  private var _encoding ="utf-8"
  private var _data:Any = null
  private var _cls=true

  def tlog() = XData._log

  def this(p:Any) {
    this()
    resetContent(p)
  }

  def encoding_=(enc:String) { _encoding=enc }
  def encoding = _encoding

  /**
   * Control the internal file.
   *
   * @param del true to delete, false ignore.
   */
  def setDeleteFile(del:Boolean): this.type = { _cls= del; this }
  def isDeleteFile() = _cls

  def destroy() {
    _data match {
      case x:File if isDeleteFile => FUT.deleteQuietly( x)
      case _ =>
    }
    reset()
  }

  def isDiskFile() = _data match {
    case x:File => true
    case _ => false
  }

  def resetContent(obj:Any, delIfFile:Boolean): this.type = {
    destroy()
    obj match {
      case wtr: CharArrayWriter => _data= new String( wtr.toCharArray)
      case baos: ByteArrayOS => _data = baos.toByteArray
      case bits: Array[Byte] => _data= bits
      case fa:Array[File] => if (fa.length > 0) _data= fa(0)
      case f:File => _data = f
      case s:String => _data = s
      case _ => if (obj != null) {
        throw new IOException("Unsupported object: " + obj.getClass().getName() )
      }
    }
    setDeleteFile(delIfFile)
  }

  def resetContent(obj:Any): XData = resetContent(obj, true)

  def content() : Option[Any] = Option(_data)

  def hasContent() = _data != null

  def javaBytes() : Array[Byte] = _data match {
    case x:File => IOU.toByteArray(x.toURI.toURL)
    case x:String => x.getBytes(_encoding)
    case x:Array[Byte] => x
    case _ => Array()
  }

  def fileRef() =  _data match {
    case x:File => x
    case _ => null
  }

  def filePath() =  _data match {
    case x:File => x.getCanonicalPath
    case _ => ""
  }

  def size() : Long = _data match {
    case x:File => x.length()
    case _ => javaBytes().length
  }

  override def finalize() {
    destroy()
  }

  def stringify() = {
    new String ( javaBytes(), _encoding )
  }

  def stream(): InputStream = _data match {
    case x:File => new XStream(x)
    case x if x != null => new ByteArrayIS( javaBytes() )
    case _ => null
  }

  private def reset() {
    _encoding= "utf-8"
    _cls=true
    _data=null
  }

}

