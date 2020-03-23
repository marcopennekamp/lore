package lore

import lore.execution.Context

object Lore {
  def main(args: Array[String]): Unit = {
    val maybeContext = Context.fromExample(args(0))
    if (maybeContext.isEmpty) {
      println("The context couldn't be created due to an error (likely mentioned above). Aborting Lore execution...")
      return
    }

    implicit val context = maybeContext.get

    // Print types for debugging.
    println("Types:")
    context.types.values.foreach { t =>
      println(s"  ${t.verbose}")
    }

    // Print functions for debugging.
    context.multiFunctions.values.foreach { mf =>
      println()
      println(s"${mf.name}:")
      mf.functions.foreach(f => println(s"  $f"))
    }

    println()
    val verificationResult = context.verify()
    verificationResult.print()
  }
}
