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

import javax.jms.Connection
import javax.jms.ConnectionConsumer
import javax.jms.ConnectionMetaData
import javax.jms.Destination
import javax.jms.ExceptionListener
import javax.jms.JMSException
import javax.jms.ServerSessionPool
import javax.jms.Session
import javax.jms.Topic

/**
 * @author kenl
 *
 */
class MockConnection(private val _uid:String,private val _pwd:String) extends Connection {

  def this() {
    this("","")
  }

  def close() {}

  def createConnectionConsumer(dest:Destination, a1:String, pool:ServerSessionPool, a3:Int) = null

  def createDurableConnectionConsumer(topic:Topic, a1:String, a2:String, pool:ServerSessionPool, a4:Int) = null

  def createSession(tx:Boolean, ack:Int) = new MockSession(tx, ack)

  def getClientID() = ""

  def getExceptionListener() = null

  def getMetaData() = null

  def setClientID(a:String) {}

  def setExceptionListener(e:ExceptionListener) {}

  def start() {}

  def stop() {}

}

