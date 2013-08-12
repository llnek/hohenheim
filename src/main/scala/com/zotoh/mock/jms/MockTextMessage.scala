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

import java.util.Random

import javax.jms.Destination
import javax.jms.JMSException
import javax.jms.TextMessage

import com.zotoh.frwk.util.CoreUtils._

/**
 * @author kenl
 *
 */
class MockTextMessage(private var _text:String) extends TextMessage {

  private var _type="Mock-Text-Message"

  def acknowledge() {}

  def clearBody() {}

  def clearProperties() {}

  def getBooleanProperty(a:String) = false

  def getByteProperty(a:String) = 0

  def getDoubleProperty(a:String) = 0.toDouble

  def getFloatProperty(a:String ) = 0.toFloat

  def getIntProperty(a:String) = 0

  def getJMSCorrelationID() = {
    new Random().nextInt(Int.MaxValue).toString
  }

  def getJMSCorrelationIDAsBytes() = getJMSCorrelationID().getBytes()

  def getJMSDeliveryMode() = 0

  def getJMSDestination() = null

  def getJMSExpiration() = 0L

  def getJMSMessageID() =
    "msg-" + new Random().nextInt(Int.MaxValue)

  def getJMSPriority() = 0

  def getJMSRedelivered() = false

  def getJMSReplyTo() = null

  def getJMSTimestamp() = 0L

  def getJMSType() = _type

  def getLongProperty(a:String) = 0L

  def getObjectProperty(a:String ) = null

  def getPropertyNames() = null

  def getShortProperty(a:String ) = 0.toShort

  def getStringProperty(a:String ) = null

  def propertyExists(a:String ) = false

  def setBooleanProperty(a:String , b:Boolean) {}

  def setByteProperty(a:String , b:Byte ) {}

  def setDoubleProperty(a:String , d:Double ) {}

  def setFloatProperty(a:String , f:Float ) {}

  def setIntProperty(a:String , arg1:Int) {}

  def setJMSCorrelationID(a:String ) {}

  def setJMSCorrelationIDAsBytes(b:Array[Byte] ) {}

  def setJMSDeliveryMode(arg:Int) {}

  def setJMSDestination(d:Destination ) {}

  def setJMSExpiration(a:Long) {}

  def setJMSMessageID(a:String ) {}

  def setJMSPriority(a:Int) {}

  def setJMSRedelivered(a:Boolean) {}

  def setJMSReplyTo(d:Destination ) {}

  def setJMSTimestamp(arg:Long) {}

  def setJMSType(a:String ) {
    _type=a
  }

  def setLongProperty(a:String , arg:Long) {}

  def setObjectProperty(a:String , o:Object ) {}

  def setShortProperty(a:String , arg:Short) {}

  def setStringProperty(a:String , a1:String ) {}

  def getText() = _text

  def setText(t:String ) {
    _text=t
  }

}
