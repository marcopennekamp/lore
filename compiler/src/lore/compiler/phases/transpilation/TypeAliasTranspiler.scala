package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.semantics.Registry
import lore.compiler.types.Type

object TypeAliasTranspiler {

  // TODO (alias): Note the accompanying TODO in RuntimeTypeTranspiler.

  /**
    * Transpiles the given type alias to a global constant that can be referenced by other types at run-time.
    */
  def transpile(name: String, tpe: Type)(implicit registry: Registry): Compilation[String] = {
    // TODO: Implement.
    "".compiled
  }

}