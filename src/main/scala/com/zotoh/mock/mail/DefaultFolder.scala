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
import javax.mail.MethodNotSupportedException
import javax.mail.Store


/**
 * @author kenl
 *
 */
class DefaultFolder protected[mail] (s:Store) extends Folder(s) {

  def getName() = ""

  def getFullName() = ""

  def getParent() = null

  def exists() = true

  def list(pn:String) = Array( getInbox() )

  def getSeparator() = '/'

  def getType()  = 2

  def create(t:Int) = false

  def hasNewMessages() = false

  def getFolder(name:String) = {
    if (!name.equalsIgnoreCase("INBOX")) {
      throw new MessagingException("Only INBOX is supported")
    }
    getInbox()
  }

  def getInbox() = getStore().getFolder("INBOX")

  def delete(recurse:Boolean) = {
    throw new MethodNotSupportedException("delete")
  }

  def renameTo(f:Folder) = {
    throw new MethodNotSupportedException("renameTo")
  }

  def open(mode:Int) {
    throw new MethodNotSupportedException("open")
  }

  def close(expunge:Boolean) {
    throw new MethodNotSupportedException("close")
  }

  def isOpen() = false

  def getPermanentFlags() = new Flags()

  def getMessageCount() = 0

  def getMessage(msgno:Int) = {
    throw new MethodNotSupportedException("getMessage")
  }

  def appendMessages(msgs:Array[Message] ) {
    throw new MethodNotSupportedException("Append not supported")
  }

  def expunge() = {
    throw new MethodNotSupportedException("expunge")
  }

}

