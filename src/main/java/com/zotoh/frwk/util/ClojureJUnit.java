/*??
 * COPYRIGHT (C) 2013 CHERIMOIA LLC. ALL RIGHTS RESERVED.
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

package com.zotoh.frwk.util;



import mikera.cljunit.ClojureTest;

import java.util.Arrays;
import java.util.List;

public class ClojureJUnit extends ClojureTest {
    @Override
    public List<String> namespaces() {
        return Arrays.asList(new String[]{
                "testcljc.util.byteutils",
                "testcljc.util.codes",
                "testcljc.util.coreutils",
                "testcljc.util.dateutils",
                "testcljc.util.fileutils",
                "testcljc.util.guids",
                "testcljc.util.ioutils",
                "testcljc.util.metautils",
                "testcljc.util.mimeutils",
                "testcljc.util.procutils",
                "testcljc.util.seqnumgen",
                "testcljc.util.strutils",
                "testcljc.util.win32ini",
                "testcljc.net.netstuff",
                "testcljc.i18n.i18nstuff",
                "testcljc.crypto.cryptostuff",
                "testcljc.crypto.mimestuff"

        });
    }
}
