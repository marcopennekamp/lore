package lore.compiler.semantics.bindings

import lore.compiler.core.{UniqueIdentifiable, UniqueKey}

/**
  * An untyped [[LocalVariable]]. Untyped local variables are converted to typed local variables during typing, with
  * the [[uniqueKey]] being the crucial identifier.
  *
  * One [[UntypedLocalVariable]] instance should exist per local variable declaration.
  */
class UntypedLocalVariable(
  val uniqueKey: UniqueKey,
  val name: String,
  override val isMutable: Boolean,
) extends TermBinding with UniqueIdentifiable {
  override def toString: String = name
}

object UntypedLocalVariable {
  def apply(name: String, isMutable: Boolean): UntypedLocalVariable = {
    new UntypedLocalVariable(UniqueKey.fresh(), name, isMutable)
  }

  /**
    * Creates an <b>immutable</b> local variable.
    */
  def apply(name: String): UntypedLocalVariable = UntypedLocalVariable(name, isMutable = false)
}
