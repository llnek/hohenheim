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

import javax.mail.Flags
import javax.mail.Folder
import javax.mail.Message
import javax.mail.MessagingException
import javax.mail.Store


/**
 * @author kenl
 *
 */
class MockPop3Folder(private val _name:String, s:Store) extends Folder(s) {

  private var _open = false
  private var _count = 1

  override def appendMessages(a:Array[Message] ) {}

  override def close(arg:Boolean) {
    _open=false
  }

  override def create(arg:Int) = false

  override def delete(arg:Boolean) = false

  override def exists() = true

  override def expunge() = null

  override def getFolder(arg:String) = null

  override def getFullName() = _name

  override def getMessage(pos:Int) = {
    if (pos < 1) throw new MessagingException("wrong message num: " + pos)
    new MockPop3Msg(this, pos).newMimeMsg()
  }

  override def getMessageCount() = _count

  override def getName() = _name

  override def getParent() = null

  override def getPermanentFlags() = null

  override def getSeparator() = 0

  override def getType() = 0

  override def hasNewMessages() = false

  override def isOpen() = _open

  override def list(arg:String) = null

  override def open(arg:Int) {
    _open=true
  }

  override def renameTo(arg:Folder) = false


}

