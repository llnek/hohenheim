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

package com.zotoh.hohenheim.runtime

import com.zotoh.hohenheim.core.Container
import com.zotoh.frwk.core.{Startable, Initializable, Disposable}
import org.json.JSONObject

trait AppMain extends Disposable with Initializable with Startable {

  def contextualize(c:Container) : Unit
  def configure(options:JSONObject ) : Unit

}
