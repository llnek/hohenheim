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

package com.zotoh.frwk.net

import org.apache.commons.io.{IOUtils=>IOU}
import com.zotoh.frwk.util.CoreUtils._
import com.zotoh.frwk.io.IOUtils._
import com.zotoh.frwk.io.XData

import java.io.{OutputStream, InputStream, ByteArrayOutputStream=>ByteArrayOS}
import java.io.{File,FileOutputStream,IOException}

import org.apache.commons.fileupload._
import org.slf4j._

object ULFileItem {
  private val _log= LoggerFactory.getLogger(classOf[ULFileItem])
}

/**
 * @author kenl
 *
 */
@SerialVersionUID( 2214937997601489203L)
class ULFileItem extends FileItem with Serializable {

  def tlog() = ULFileItem._log

  @transient private var _os:OutputStream = null
  private var _fieldBits:Array[Byte] = null
  private var _filename = ""
  private var _field=""
  private var _ctype=""
  private var _ds:XData= null
  private var _ff = false

  /**
   * @param field
   * @param contentType
   * @param isFormField
   * @param fileName
   */
  def this(field:String, contentType:String, isFormField:Boolean, fileName:String)   {
    this()
    _ctype= nsb(contentType)
    _field= field
    _ff= isFormField
    _filename= fileName
  }

  override def delete()  {
    IOU.closeQuietly(_os)
    if (_ds!=null) {
      _ds.setDeleteFile(true)
      _ds.destroy()
    }
    _ds=null
  }

  override def getHeaders()  = throw new IOException("not implemented")
  override def setHeaders(h:FileItemHeaders) {
    throw new IOException("not implemented")
  }
    
  override def getContentType() = _ctype

  override def get() = null

  override def getFieldName()  = _field

  override def getInputStream() = throw new IOException("not implemented")

  override def getName() = _filename

  override def getOutputStream() = {
    if (_os==null) iniz() else _os
  }

  override def getSize() = 0L

  def fileData() = _ds

  override def getString() = getString("UTF-8")

  override def getString(charset:String) = {
    if (maybeGetBits() == null) null else new String(_fieldBits, charset)
  }

  override def isFormField() = _ff

  override def isInMemory() = false

  override def setFieldName(s:String) {
    _field=s
  }

  override def setFormField(b:Boolean)  {
    _ff= b
  }

  override def write(fp:File) { }

  def cleanup()  {
    if (_fieldBits == null) {  maybeGetBits() }
    IOU.closeQuietly(_os)
    _os=null
  }

  override def finalize() {
    IOU.closeQuietly(_os)
    super.finalize()
  }

  private def maybeGetBits() = {
    _os match {
      case baos:ByteArrayOS => _fieldBits= baos.toByteArray()
      case _ =>
    }
    _fieldBits
  }

  private def iniz() = {

    if (_ff) {
      _os= new ByteArrayOS(1024)
    } else {
      _ds= new XData()
      try {
        val t= newTempFile(true)
        _ds.resetContent( t._1)
        _os = t._2
      } catch {
        case e:Throwable => tlog.error("", e)
      }
    }

    _os
  }

}
