package lore.types

import lore.compiler.Compilation.C

/**
  * OwnedBy has its typing deferred since, similar to class constructors and members, OwnedBy can be a complex
  * type, which might easily lead to cycles if we attempted to include it in the dependency graph. Since it doesn't
  * declare a type itself and has no bearing on the class type directly (for purposes of declaration resolution), it
  * can easily be deferred.
  */
class OwnedBy(override val resolveType: () => C[Type]) extends TypingDeferred[Type]
