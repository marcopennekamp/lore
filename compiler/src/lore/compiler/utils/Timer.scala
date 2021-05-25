package lore.compiler.utils

object Timer {

  def timed[R](name: String, n: Int = 1)(block: => R): R = {
    val start = System.nanoTime()
    var result: R = block
    for (_ <- 1 until n) {
      result = block
    }
    val end = System.nanoTime()
    println(s"$name took: ${(end - start) / 1000 / n}µs")
    result
  }

}
