package @@APPDOMAIN@@

import com.zotoh.blason.kernel.Container


class AppMain(c:Container) {
  def start() {
    println("Application started.")
  }
  def stop() {
    println("Application stopped.")
  }
  def dispose() {
    println("Application disposed.")
  }
}
