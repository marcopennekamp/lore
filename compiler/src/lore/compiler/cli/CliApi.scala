package lore.compiler.cli

import lore.compiler.build.BuildApi

/**
  * The CliApi takes care of parsing CLI options and invoking an appropriate build.
  */
object CliApi {

  def main(args: Array[String]): Unit = {
    CliBuildOptionsParser.parse(args) match {
      case Some(options) =>
        val isSuccess = BuildApi.build(options)
        if (!isSuccess) {
          sys.exit(1)
        }

      case None =>
        // scopt will already have written an error message to the console.
        sys.exit(1)
    }
  }

}
