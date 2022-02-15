package lore.compiler.semantics.core

import lore.compiler.semantics.NamePath

/**
  * Contains all core definitions of Pyramid's `lore.core` module. The compiler expects these definitions to exist
  * somewhere in the source code with compatible parameters and output types.
  */
class CoreDefinitions(
  // Traits.
  val Type: CoreTrait,

  // Multi-functions.
  val equal: CoreMultiFunction,
  val less_than: CoreMultiFunction,
  val less_than_equal: CoreMultiFunction,
  val hash: CoreMultiFunction,
  val to_string: CoreMultiFunction,
  //val type_of: CoreMultiFunction,
  //val subtype: CoreMultiFunction,
)

object CoreDefinitions {
  val modulePath: NamePath = NamePath("lore", "core")
}
