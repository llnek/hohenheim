/*??
 * COPYRIGHT (C) 2012-2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

package com.zotoh.hohenheim.io;


import java.util.List;
import java.util.Map;
import java.net.HttpCookie;


public interface HTTPEvent  extends IOEvent {

  public List<HttpCookie> getCookies();

  public HttpCookie getCookie(String name);

  public boolean isKeepAlive();

  public Object data();

  public boolean hasData();

  public long contentLength();

  public String contentType();

  public String encoding();

  public String contextPath();

  public List<String> getHeaderValues(String nm);
  public List<String> getHeaders();
  public String getHeaderValue(String nm);

  public List<String> getParameterValues(String nm);
  public List<String> getParameters();
  public String getParameterValue(String nm);

  public String localAddr();

  public String localHost();

  public int localPort();

  public String method();

  public String protocol();
  public String host();

  public String queryString();

  public String remoteAddr();

  public String remoteHost();

  public int remotePort();

  public String scheme();

  public String serverName();

  public int serverPort();


  public boolean isSSL();

  public String getUri();

  public String getRequestURL();

  //------------

  public HTTPResult getResultObject();
  public void replyResult();
}



