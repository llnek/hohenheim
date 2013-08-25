(defproject comzotohcljc/hohenheim "0.1.0-SNAPSHOT"

  :description "A full stack server-side application framework."

  :url "http://zotoh.com/hohenheim"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [
                  [org.clojure/clojure "1.5.1"]
                  [bouncycastle/bcmail-jdk15on "149"]
                  [bouncycastle/bcpkix-jdk15on "149"]
                  [org.jasypt/jasypt "1.9.0"]
                  [org.mindrot/jbcrypt "0.3m"]

                  [org.slf4j/slf4j-api "1.7.5"]
                  [ch.qos.logback/logback-classic "1.0.13"]
                  [ch.qos.logback/logback-core "1.0.13"]

                  [net.sourceforge.jregex/jregex "1.2_01"]
                  [com.beust/jcommander "1.30"]
                  [com.google.guava/guava "14.0.1"]
                  [com.google.code.findbugs/jsr305 "2.0.1"]
                  [joda-time/joda-time "2.2"]

                  [org.jdom/jdom2 "2.0.4"]
                  [org.apache.commons/commons-lang3 "3.1"]
                  [commons-net/commons-net "3.3"]
                  [commons-io/commons-io "2.4"]

                  [commons-collections/commons-collections "3.2.1"]
                  [commons-pool/commons-pool "1.6"]
                  [commons-dbcp/commons-dbcp "1.4"]

                  [commons-logging/commons-logging "1.1.1"]
                  [commons-email/commons-email "1.3.1"]
                  [commons-codec/commons-codec "1.8"]
                  [commons-fileupload/commons-fileupload "1.3"]
                  [commons-dbutils/commons-dbutils "1.5"]
                  [com.sun.mail/javamail "1.5.0"]

                  [org.apache.ivy/ivy "2.3.0"]
                  [org.apache.ant/ant "1.9.2"]
                  [org.apache.ant/ant-launcher "1.9.2"]
                  [org.apache.ant/ant-junit4 "1.9.2"]
                  [org.apache.ant/ant-junit "1.9.2"]
                  [ant-contrib/ant-contrib "1.0b3"]

                  [com.j256.ormlite/ormlite-core "4.42"]
                  [com.j256.ormlite/ormlite-jdbc "4.42"]
                  [com.mchange/c3p0 "0.9.2-pre6"]
                  [org.squeryl/squeryl_2.10 "0.9.5-6"]
                  [org.apache.empire-db/empire-db "2.4.1"]
                  [com.jolbox/bonecp "0.7.1.RELEASE"]

                  [org.apache.httpcomponents/httpcore-nio "4.2.4"]
                  [org.apache.httpcomponents/httpcore "4.2.4"]
                  [org.apache.httpcomponents/httpclient "4.2.5"]
                  [io.netty/netty "3.6.6.Final"]
                  [org.eclipse.jetty.orbit/javax.servlet "3.0.0.v201112011016"]
                  [org.eclipse.jetty/jetty-xml "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-server "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-continuation "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-servlet "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-server "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-util "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-security "9.0.4.v20130625"]
                  [org.eclipse.jetty/jetty-webapp "9.0.4.v20130625"]
                  [org.eclipse.jetty.websocket/websocket-api "9.0.4.v20130625"]
                  [org.eclipse.jetty.websocket/websocket-common "9.0.4.v20130625"]
                  [org.eclipse.jetty.websocket/websocket-servlet "9.0.4.v20130625"]
                  [org.eclipse.jetty.websocket/websocket-client "9.0.4.v20130625"]
                  [org.eclipse.jetty.websocket/websocket-server "9.0.4.v20130625"]

                  [org.codehaus.groovy/groovy-all "2.1.6"]
                  [org.scala-lang/scala-library "2.10.2" ]
                  [org.scala-lang/scala-compiler "2.10.2"]
                  [com.sun.tools/tools "1.7.0"]
                  [org.scalatest/scalatest_2.10 "1.9.1"]

                  [com.github.spullara.mustache.java/compiler "0.8.9"]
                  [org.fusesource.scalate/scalate-core_2.10 "1.6.1"]
                  [org.freemarker/freemarker "2.3.19"]
                  [com.yahoo.platform.yui/yuicompressor "2.4.7"]
                  [junit/junit "4.11"]

                  [javax/geronimo-jms_1.1_spec "1.1.1"]
                  [com.h2database/h2 "1.3.172"]
                  [org.postgresql/postgresql "9.2-1002.jdbc4"]
                  [com.mysql/mysql-connector-java "5.1.23-bin"]
                  [net.sf.ehcache/ehcache "2.7.0"]

                  [javax/javax-transaction "6.0.0"]
                  [javax/javax-jms "6.0.0"]
                  [javax/javax-management "6.0.0"]
                  [org.clojure/math.numeric-tower "0.0.2"]
                  [org.clojure/math.combinatorics "0.0.4"]
                  [org.clojure/tools.logging "0.2.6"]
                  [org.clojure/data.codec "0.1.0"]
                  [org.clojure/java.jdbc "0.3.0-alpha4"]
                  [org.clojure/java.jmx "0.2.0"]
                  [org.clojure/data.json "0.2.2"]
                  [org.clojure/data.xml "0.0.7"]
                  [org.clojure/clojure "1.5.1"]
                  [net.mikera/cljunit "0.3.0"]
                 ]

  :main hohenheim.core)
