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


//import com.zotoh.frwk.io.XData;

import java.net.HttpCookie;
import java.net.URL;

public interface HTTPResult extends IOResult {

  public void setRedirect(URL location);

  public void setProtocolVersion(String ver);
  public void setStatus(int code);
  public void addCookie(HttpCookie c);

  public void containsHeader(String name);
  public void removeHeader(String name);
  public void clearHeaders();

  public void addHeader(String name, String value);
  public void setHeader(String name, String value);

  public void setChunked(boolean c);
  public void setContent(Object data);

}


