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

import javax.jms.BytesMessage
import javax.jms.Destination
import javax.jms.JMSException
import javax.jms.MapMessage
import javax.jms.Message
import javax.jms.MessageConsumer
import javax.jms.MessageListener
import javax.jms.MessageProducer
import javax.jms.ObjectMessage
import javax.jms.Queue
import javax.jms.QueueBrowser
import javax.jms.Session
import javax.jms.StreamMessage
import javax.jms.TemporaryQueue
import javax.jms.TemporaryTopic
import javax.jms.TextMessage
import javax.jms.Topic
import javax.jms.TopicSubscriber


/**
 * @author kenl
 *
 */
class MockSession(private val _tx:Boolean,private val _ack:Int) extends Session {

  def close() {}

  def commit() {}

  def createBrowser(q:Queue ) = null

  def createBrowser(q:Queue , a:String ) = null

  def createBytesMessage() = null

  def createConsumer(d:Destination ) = new MockMsgConsumer(d)

  def createConsumer(d:Destination , a:String ) = null

  def createConsumer(d:Destination , a:String ,
      a2:Boolean ) = null

  def createDurableSubscriber(t:Topic , a:String ) = null

  def createDurableSubscriber(t:Topic , a1:String , a2:String , a3:Boolean ) = null

  def createMapMessage() = null

  def createMessage() = null

  def createObjectMessage() = null

  def createObjectMessage(s:java.io.Serializable) = null

  def createProducer(d:Destination ) = null

  def createQueue(a:String ) = null

  def createStreamMessage() = null

  def createTemporaryQueue() = null

  def createTemporaryTopic() = null

  def createTextMessage() = null

  def createTextMessage(a:String) = null

  def createTopic(a:String ) = null

  def getAcknowledgeMode() = 0

  def getMessageListener() = null

  def getTransacted() = false

  def recover() {}

  def rollback() {}

  def run() {}

  def setMessageListener(ml:MessageListener ) {}

  def unsubscribe(a:String ) {}

}
