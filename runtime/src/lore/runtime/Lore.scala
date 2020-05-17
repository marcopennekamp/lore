package lore.runtime

import scala.scalajs.js

object Lore {
  val testCode: String =
    """function test() {
      |  console.log(Types.typeof('Hello'));
      |}""".stripMargin

  def main(args: Array[String]): Unit = {
    js.eval(testCode)
    js.Dynamic.global.applyDynamic("test")()
  }
}