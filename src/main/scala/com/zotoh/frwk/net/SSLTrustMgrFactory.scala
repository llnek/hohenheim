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

package com.zotoh.frwk.net

import java.security.InvalidAlgorithmParameterException
import java.security.KeyStore
import java.security.KeyStoreException
import java.security.cert.CertificateException
import java.security.cert.{X509Certificate=>XCert}

import javax.net.ssl.ManagerFactoryParameters
import javax.net.ssl.TrustManager
import javax.net.ssl.TrustManagerFactorySpi
import javax.net.ssl.X509TrustManager

import org.slf4j._


/**
 * @author kenl
 *
 */
object SSLTrustMgrFactory {

  private val _log=LoggerFactory.getLogger(classOf[SSLTrustMgrFactory])
  def tlog() = _log

  def getTrustManagers() = Array[TrustManager]( new X509TrustManager() {
    def checkClientTrusted( chain:Array[XCert], authType:String) {
      tlog.warn("SkipCheck: CLIENT CERTIFICATE: {}" , chain(0).getSubjectDN)
    }
    def checkServerTrusted( chain:Array[XCert], authType:String) {
      tlog.warn("SkipCheck: SERVER CERTIFICATE: {}" , chain(0).getSubjectDN)
    }
    def getAcceptedIssuers() = Array[XCert]()
  })

}

/**
 * @author kenl
 *
 */
class SSLTrustMgrFactory extends TrustManagerFactorySpi {

  override def engineGetTrustManagers() = SSLTrustMgrFactory.getTrustManagers
  override def engineInit(ks:KeyStore) {}
  override def engineInit(p:ManagerFactoryParameters) {}
}

