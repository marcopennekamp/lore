package lore.definitions

import lore.compiler.{Compilation, Error, Verification}

case class MultiFunctionDefinition(name: String, functions: List[FunctionDefinition]) {
  /**
    * Verifies that all functions declared in the multi-function have a different signature.
    */
  def verify: Verification = {
    // Of course, all functions added to the multi-function must have the same name. If that is not the case,
    // there is something very wrong with the compiler.
    functions.foreach(function => assert(function.name == name))

    // Also verify that all functions have different signatures.
    functions.map { function =>
      if (functions.filterNot(_ == function).map(_.signature).contains(function.signature)) {
        // We have found a function with a duplicated signature!
        Compilation.fail(Error.FunctionAlreadyExists(function))
      } else {
        Compilation.succeed(())
      }
    }.simultaneous.map(_ => ())
  }
}
