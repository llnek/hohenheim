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

package com.zotoh.frwk.crypto

import java.io.{File,IOException,InputStream,OutputStream,ByteArrayInputStream}
import javax.activation.DataSource
import com.zotoh.frwk.util.{CoreUtils=>CU}
import com.zotoh.frwk.io.XStream

/**
 * @author kenl
 *
 */
class SDataSource extends DataSource {

  private var _bits:Array[Byte]= null
  private var _ctype:String=""
  private var _fn:File=null

  /**
   * @param content
   * @param contentType
   */
  def this(content:File, contentType:String) {
    this()
    _ctype= CU.nsb(contentType)
    _fn= content
  }

  /**
   * @param content
   * @param contentType
   */
  def this(content:Array[Byte], contentType:String) {
    this()
    _ctype= CU.nsb(contentType)
    _bits= content
  }


  /**
   * @param content
   */
  def this(content:File) {
    this(content, "")
  }


  /**
   * @param content
   */
  def this(content:Array[Byte]) {
    this(content, "")
  }


  override def getContentType() = _ctype

  override def getInputStream() = {
    if (_fn==null) new ByteArrayInputStream(_bits) else new XStream(_fn)
  }

  override def getName() = "Unknown"

  override def getOutputStream() = throw new IOException("Not implemented")

}

