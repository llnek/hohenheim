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

import scala.collection.JavaConversions._
import java.io.IOException
import java.io.{InputStream,ByteArrayInputStream=>ByteArrayIS}
import java.util.{Date=>JDate}
import java.util.Enumeration
import java.util.Random
import java.util.Vector
import javax.mail.Folder
import javax.mail.MessagingException
import javax.mail.internet.MimeMessage
import javax.mail.Session
import javax.mail.Message
import javax.mail.Multipart
import org.apache.commons.lang3.{StringUtils=>STU}

/**
 * @author kenl
 */
object MockPop3Msg {
  
  private val _mime=
"From: Some One <someone@example.com>\r\n"+
"To: Some Body <somebody@ex.com>\r\n"+
"Subject: Hello Jack\r\n"+
"MIME-Version: 1.0\r\n"+
"Content-Type: multipart/mixed;boundary=\"XXXXboundary text\"\r\n"+
"This is a multipart message in MIME format.\r\n"+
"\r\n"+
"--XXXXboundary text\r\n"+
"Content-Type: text/plain\r\n"+
"\r\n"+
"this is the time ${TS}\r\n"+
"\r\n"+
"--XXXXboundary text\r\n"+
"Content-Type: text/plain\r\n"+
"Content-Disposition: attachment; filename=\"test.txt\"\r\n"+
"\r\n"+
"this is the attachment text\r\n"+
"\r\n"+
"--XXXXboundary text--\r\n"    
    
  def main(args:Array[String]) {
    val m=new MimeMessage( Session.getInstance(System.getProperties()) , 
        new ByteArrayIS(_mime.getBytes("utf-8")))
    m.saveChanges()
    val h=m.getAllHeaderLines()
    val ct=m.getContentType()
    val x=m.getContent() match {
      case p:Multipart =>
        var c=p.getCount()
        val pp=p.getBodyPart(0)
        c=0
      case _ =>
    }
    val s= m.getFrom()(0)
    val r= m.getRecipients(Message.RecipientType.TO)(0)
    val n=m.getMessageNumber()
    return
  }
  
  
}

/**
 * @author kenl
 *
 */
class MockPop3Msg (f:Folder, m:Int)  {

  import MockPop3Msg._

  def newMimeMsg() = {
    val s=STU.replace(_mime, "${TS}",  new JDate().toString )
      val m= new MimeMessage( Session.getInstance(System.getProperties()) , 
        new ByteArrayIS( s.getBytes("utf-8")))
      m.saveChanges()
      m
  }
  
  
  
}


