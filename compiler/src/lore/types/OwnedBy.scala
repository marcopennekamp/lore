package lore.types

import lore.compiler.Compilation.C

/**
  * OwnedBy has its typing deferred since, similar to class constructors and members, OwnedBy can be a complex
  * type, which might easily lead to cycles if we attempted to include it in the dependency graph. Since it doesn't
  * declare a type itself and has no bearing on the class type directly (for purposes of declaration resolution), it
  * can easily be deferred.
  */
class OwnedBy(override val typeResolver: () => C[Type]) extends TypingDeferred[Type] {
  // TODO: It would be cleaner if we could keep TypingDeferred out of the types package, as the JS runtime
  //       shouldn't have to deal with deferred typings.
  override def equals(obj: Any): Boolean = obj match {
    case rhs: OwnedBy => this.eq(rhs) || this.tpe == rhs.tpe
    case _ => false
  }
  override def hashCode(): Int = tpe.hashCode()
}
