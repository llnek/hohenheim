/*??
*
* Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
*
* This library is distributed in the hope that it will be useful
* but without any warranty; without even the implied warranty of
* merchantability or fitness for a particular purpose.
*
* The use and distribution terms for this software are covered by the
* Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
* which can be found in the file epl-v10.html at the root of this distribution.
*
* By using this software in any fashion, you are agreeing to be bound by
* the terms of this license.
* You must not remove this notice, or any other, from this software.
*
 ??*/

package com.zotoh.frwk.dbio

import java.sql.Connection

trait JDBCInfo {
  def getDriver() : String
  def getUrl() :String
  def getUser() : String
  def getPwd() : String
  //
  def getId() : String
}


trait JDBCPool {

  def nextFree() : Connection
  def shutdown() : Unit
  def vendor() : Any
}

trait MetaCache {
  def getMetas() : Any
}

trait Schema {
  def getModels() : Any
}

trait Transactable {
  def execWith(fn:Any) : Any
  def begin() : Connection
  def commit(c:Connection) : Unit
  def rollback(c:Connection) : Unit
}

trait SQLr {
  def findSome(model:Any, filters:Map[String,Any], ordering:String) : Seq[Any]
  def findSome(model:Any, filters:Map[String,Any]) : Seq[Any]
  def findAll(model:Any, ordering:String) : Seq[Any]
  def findAll(model:Any) : Seq[Any]
  def findOne(model:Any,filters:Map[String,Any]) : Any
  def update(obj:Any) : Int
  def delete(obj:Any) : Int
  def insert(obj:Any) : Int
  def select(sql:String, params:Seq[Any] ) : Any
  def executeWithOutput(sql:String,params:Seq[Any]) : Any
  def execute(sql:String, params:Seq[Any])
  def countAll(model:Any): Int
  def purge(model:Any): Unit
}

trait DBAPI {

  def supportsOptimisticLock() : Boolean
  def vendor() : Any
  def finz() : Unit
  def open() : Connection
  def newCompositeSQLr() : Transactable
  def newSimpleSQLr() : SQLr

}

