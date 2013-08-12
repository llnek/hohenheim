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

import javax.jms.Destination
import javax.jms.JMSException
import javax.jms.Message
import javax.jms.MessageConsumer
import javax.jms.MessageListener


/**
 * @author kenl
 *
 */
class MockMsgConsumer(private val _dest:Destination) extends MessageConsumer {

  private var _sub:MessageListener = null

  def close() {
    _sub=null
  }

  def getMessageListener() = _sub

  def getMessageSelector() = ""

  def receive() = null

  def receive(a:Long) = null

  def receiveNoWait() = null

  def setMessageListener(ml:MessageListener ) {
    _sub=ml
  }


}
