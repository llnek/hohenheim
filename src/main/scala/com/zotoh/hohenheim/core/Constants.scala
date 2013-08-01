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

package com.zotoh.hohenheim.core

object Constants {

  val SYS_DEVID_PFX= "system.####"
  val SYS_DEVID_SFX= "####"

  val SYS_DEVID_REGEX= SYS_DEVID_PFX+"[0-9A-Za-z_\\-\\.]+"+SYS_DEVID_SFX
  val SHUTDOWN_DEVID= SYS_DEVID_PFX+"kill_9"+SYS_DEVID_SFX
  val SHUTDOWN_URI="/kill9"

  val POD_PROTOCOL = "pod:"
  val META_INF = "META-INF"
  val POD_INF = "POD-INF"
  val WEB_INF = "WEB-INF"
//  val PATCH="patch"
//  val LIB="lib"
//  val CFG="conf"
//  val CLASSES="classes"
  val DN_BLOCKS = "blocks"
  val DN_CORE="exec"
  val DN_CONF="conf"
  val DN_CLASSES="classes"
  val DN_LIB="lib"
  val DN_CFG="etc"
  val DN_BOXX="apps"    
  val DN_PODS = "pods"
  val DN_LOGS="logs"
  val DN_TMP="tmp"
  val DN_DBS="dbs"
  val DN_DIST="dist"
  val DN_TEMPLATES = "templates"
  val DN_VIEWS = "views"
  val DN_PAGES = "pages"
  val DN_PATCH="patch"
  val DN_IMAGES="images"
  val DN_SCRIPTS="scripts"
  val DN_STYLES="styles"
  val DN_PUBLIC="public"


  val MN_FILE= META_INF + "/" + "MANIFEST.MF"
  val POD_CLASSES = POD_INF + "/"+ DN_CLASSES
  val POD_PATCH = POD_INF + "/"+ DN_PATCH
  val POD_LIB = POD_INF + "/"+ DN_LIB

  val WEB_CLASSES = WEB_INF + "/"+ DN_CLASSES
  val WEB_LIB = WEB_INF + "/"+ DN_LIB
  val WEB_LOG = WEB_INF + "/logs"
  val WEB_XML = WEB_INF + "/web.xml"

  val MN_RNOTES= META_INF + "/" + "RELEASE-NOTES.txt"
  val MN_README= META_INF + "/" + "README.md"
  val MN_NOTES= META_INF + "/" + "NOTES.txt"
  val MN_LIC= META_INF + "/" + "LICENSE.txt"

  val CFG_ENV_CF = DN_CONF + "/" + "env.conf"
  val CFG_APP_CF = DN_CONF + "/" + "app.conf"


  val PF_HOHENHEIM_APPDOMAIN="hohenheim.app.domain"
  val PF_HOHENHEIM_APPID="hohenheim.appid"
  val PF_HOHENHEIM_APPTASK="hohenheim.app.task"

  val PF_JMXMGM="jmx-management"
  val PF_HOMEDIR="hohenheim.home"
  val PF_PROPS="hohenheim.conf"
  val PF_ROUTE_INFO="route.info"
  val PF_CLISH="cli-shell"
  val PF_COMPS="components"
  val PF_REGS="registries"
  val PF_KERNEL="kernel"
  val PF_EXECV="execvisor"
  val PF_DEPLOYER="deployer"
  val PF_BLOCKS="blocks"
  val PF_APPS="apps"
  val PF_SVCS="services"
  val PF_LOCALE="locale"
  val PF_L10N="l10n"
  val PF_PIDFILE="pidfile"

  val K_ROOT_CZLR="root.loader"
  val K_EXEC_CZLR="exec.loader"

  val K_BASEDIR="base.dir"
  val K_PODSDIR="pods.dir"
  val K_CFGDIR="cfg.dir"
  val K_APPDIR="app.dir"
  val K_PLAYDIR="play.dir"
  val K_LOGDIR="log.dir"
  val K_TMPDIR="tmp.dir"
  val K_DBSDIR="dbs.dir"
  val K_BKSDIR="blocks.dir"

  val K_COUNTRY="country"
  val K_LOCALE="locale"
  val K_LANG="lang"

  val K_META="meta"

}
