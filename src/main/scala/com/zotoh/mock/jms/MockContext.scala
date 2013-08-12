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

package com.zotoh.mock.jms

import javax.naming.Binding
import javax.naming.Context
import javax.naming.Name
import javax.naming.NameClassPair
import javax.naming.NameParser
import javax.naming.NamingEnumeration
import javax.naming.NamingException


/**
 * @author kenl
 *
 */
class MockContext extends Context {

  def lookup(n:Name) = null

  def lookup(name:String) = {
    name match {
      case "qcf" => new MockQueueConnFactory()
      case "tcf" => new MockTopicConnFactory()
      case "cf" => new MockConnFactory()
      case s:String if s.startsWith("queue.") => new MockQueue(name)
      case s:String if s.startsWith("topic.") => new MockTopic(name)
      case _ => null
    }
  }

  def bind(n:Name, o:Object) {}

  def bind(n:String , o:Object ) {}

  def rebind(n:Name , o:Object ) {}

  def rebind(n:String , o:Object ) {}

  def unbind(n:Name ) {}

  def unbind(n:String ) {}

  def rename(n:Name , nn:Name ) {}

  def rename(n:String , nn:String ) {}

  def list(n:Name ) = null

  def list(n:String ) = null

  def listBindings(n:Name ) = null

  def listBindings(n:String ) = null

  def destroySubcontext(n:Name ) {}

  def destroySubcontext(n:String ) {}

  def createSubcontext(n:Name ) = null

  def createSubcontext(n:String ) = null

  def lookupLink(n:Name ) = null

  def lookupLink(n:String ) = null

  def getNameParser(n:Name ) = null

  def getNameParser(n:String) = null

  def composeName(n:Name , pfx:Name ) = null

  def composeName(n:String , pfx:String) = ""

  def addToEnvironment(pn:String , pv:Object) = null

  def removeFromEnvironment(pn:String) = null

  def getEnvironment() = null

  def close() {}

  def getNameInNamespace() = ""

}
