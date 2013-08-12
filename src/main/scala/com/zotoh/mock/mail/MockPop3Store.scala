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

package com.zotoh.mock.mail

import javax.mail.Folder
import javax.mail.MessagingException
import javax.mail.Session
import javax.mail.Store
import javax.mail.URLName


/**
 * @author kenl
 *
 */
class MockPop3Store( s:Session,url:URLName) extends Store(s,url) {

  private val _name:String="pop3"
  protected var _dftPort = 110
  protected var _portNum = -1
  protected var _isSSL=false
  protected var _host =""
  protected var _user = ""
  protected var _pwd = ""

    /*
    if (url != null)
      name = url.getProtocol()
      */

  override def protocolConnect(host:String, portNum:Int,
          user:String, pwd:String) = synchronized  {

    if ((host == null) || (pwd == null) || (user == null)) false else {

      _portNum = if (portNum == -1) _dftPort else portNum
      _host = host
      _user = user
      _pwd = pwd

      true
    }
  }

  override def isConnected() = synchronized {
    if ( super.isConnected()) true else false
  }

  override def close() = synchronized  {
    super.close()
  }

  def getDefaultFolder() = {
    checkConnected()
    new DefaultFolder(this)
  }

  def getFolder(name:String) = {
    checkConnected()
    new MockPop3Folder(name,this)
  }

  def getFolder(url:URLName ) = {
    checkConnected()
    new MockPop3Folder( url.getFile(), this)
  }

  override def finalize() {
    super.finalize()
  }

  private def checkConnected() {
    if (!super.isConnected())
      throw new MessagingException("Not connected")
  }

}

