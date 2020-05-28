package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.Verification
import lore.compiler.definitions.ClassDefinition

object DeferredTypingResolver {
  /**
    * Resolves the deferred typing for a given class definition. Any errors are reported through the returned
    * Verification monad.
    */
  def resolveDeferredTypings(definition: ClassDefinition): Verification = {
    (
      definition.tpe.ownedByDeferred.map(_.verifyDeferredTyping).toCompiledOption,
      definition.localMembers.map(_.verifyDeferredTyping).simultaneous,
      definition.constructors.flatMap(_.parameters).map(_.verifyDeferredTyping).simultaneous,
    ).simultaneous.map(_ => ())
  }
}
