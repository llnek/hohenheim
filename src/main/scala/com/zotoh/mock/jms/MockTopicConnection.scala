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

import javax.jms.ConnectionConsumer
import javax.jms.ConnectionMetaData
import javax.jms.Destination
import javax.jms.ExceptionListener
import javax.jms.JMSException
import javax.jms.ServerSessionPool
import javax.jms.Session
import javax.jms.Topic
import javax.jms.TopicConnection
import javax.jms.TopicSession


/**
 * @author kenl
 *
 */
class MockTopicConnection(private val _user:String,private val _pwd:String) extends TopicConnection {

  @volatile private var _active = true

  def close() { stop() }

  def createConnectionConsumer(d:Destination , a1:String , p:ServerSessionPool , arg3:Int) = null

  def createSession(b:Boolean , arg1:Int) = null

  def getClientID() = ""

  def getExceptionListener() = null

  def getMetaData() = null

  def setClientID(a:String ) {}

  def setExceptionListener(e:ExceptionListener) {}

  def start() { _active=true }

  def stop() { _active=false }

  def createConnectionConsumer(t:Topic , a1:String , p:ServerSessionPool , arg3:Int) = null

  def createDurableConnectionConsumer(t:Topic ,
      a1:String , a2:String , p:ServerSessionPool , arg4:Int) = null

  def createTopicSession(tx:Boolean, ack:Int) = {

    val s= new MockTopicSession(this, tx, ack)
    s.run()
    s
  }

  /**
   * @return
   */
  def isActive() = _active

}
