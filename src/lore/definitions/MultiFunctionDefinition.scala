package lore.definitions

import lore.compiler.Compilation.Verification
import lore.compiler.{Compilation, Error, Registry}
import lore.functions.{InputAbstractnessConstraint, TotalityConstraint}
import lore.types.{Subtyping, Type}

case class MultiFunctionDefinition(name: String, functions: List[FunctionDefinition]) {
  /**
    * Returns the multi-function fit as defined in the specification.
    */
  def fit(t: Type): Set[FunctionDefinition] = {
    //functions.foreach(f => println(s"$t <= ${f.inputType}? ${Subtyping.isSubtype(t, f.inputType)}"))
    functions.filter(f => Subtyping.isSubtype(t.toTuple, f.inputType)).toSet
  }

  /**
    * Returns the function with the exact given input type.
    */
  def exact(inputType: Type): Option[FunctionDefinition] = {
    functions.find(f => f.inputType == inputType.toTuple)
  }

  /**
    * Verifies that all functions declared in the multi-function have a unique signature.
    */
  def verifyUnique: Verification = {
    // Of course, all functions added to the multi-function must have the same name. If that is not the case,
    // there is something very wrong with the compiler.
    functions.foreach(function => assert(function.name == name))

    // Then verify that all functions have different signatures.
    functions.map { function =>
      if (functions.filterNot(_ == function).map(_.signature).contains(function.signature)) {
        // We have found a function with a duplicated signature!
        Compilation.fail(Error.FunctionAlreadyExists(function))
      } else {
        Compilation.succeed(())
      }
    }.simultaneous.map(_ => ())
  }

  /**
    * Verifies that the multi-function adheres to the input abstractness and totality constraints. Also verifies
    * that at no position in any function body may there be a continuation node.
    */
  def verifyConstraints(implicit registry: Registry): Verification = {
    // TODO: Actually use this.
    (
      InputAbstractnessConstraint.verify(this),
      TotalityConstraint.verify(this),
      // None of the functions may contain a continuation node in their bodies.
      functions.map { function =>
        function.body match {
          case None => Compilation.succeed(())
          case Some(expression) =>
            // TODO: Verify.
            Compilation.succeed(())
        }
      }.simultaneous,
    ).simultaneous.map(_ => ())
  }
}
