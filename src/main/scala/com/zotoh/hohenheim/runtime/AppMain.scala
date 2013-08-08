package com.zotoh.hohenheim.runtime

import com.zotoh.hohenheim.core.{Container, Initializable, Disposable}
import org.json.JSONObject

trait AppMain extends Disposable with Initializable {

  def contextualize(c:Container) : Unit
  def configure(options:JSONObject ) : Unit

}
