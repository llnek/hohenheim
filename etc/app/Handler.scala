package @@APPDOMAIN@@

import com.zotoh.blason.kernel.Job
import com.zotoh.blason.wflow._

class Handler(job:Job) extends Pipeline(job) {

  override def onStart() = new PTask withWork new Work {
    def eval(job:Job,arg:Any*) {
      println("***********************************************")
      println("               Handled one job.")
      println("***********************************************")
    }
  }

}


