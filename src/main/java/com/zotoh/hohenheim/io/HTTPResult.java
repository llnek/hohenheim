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


import com.zotoh.frwk.io.XData;

import java.net.HttpCookie;

public interface HTTPResult {

  public HTTPResult setProtocolVersion(String ver);
  public HTTPResult setStatus(int code);
  public HTTPResult addCookie(HttpCookie c);

  public HTTPResult containsHeader(String name);
  public HTTPResult removeHeader(String name);
  public HTTPResult clearHeaders();

  public HTTPResult addHeader(String name, String value);
  public HTTPResult setHeader(String name, String value);

  public HTTPResult setChunked(boolean c);
  public HTTPResult setContent(XData data);

}



