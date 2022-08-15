package lore.compiler.semantics.bindings

import lore.compiler.core.{UniqueIdentifiable, UniqueKey}
import lore.compiler.types.Type

/**
  * A LocalVariable is a variable declared in a local scope.
  *
  * The [[UniqueKey]] is used to differentiate between local variables defined in nested local scopes.
  *
  * Example:
  *
  * {{{
  * func foo(): Int = do
  *   let x = 7
  *   let mut y = 0
  *   if x > 5
  *     let x = 2
  *     y = x
  *   end
  * end
  * }}}
  *
  * Both `x` variables have the same type `Int` and are immutable. Hence, as LocalVariables without a unique key they
  * would be equal. The unique key allows the assembly phase to differentiate between these two variables. For example,
  * the first `x` would receive key 0, `y` key 1, and the second `x` key 2.
  *
  * Exactly one [[LocalVariable]] instance should exist per local variable declaration. The conversion from
  * [[UntypedLocalVariable]] must be one-to-one.
  */
case class LocalVariable(
  uniqueKey: UniqueKey,
  name: String,
  tpe: Type,
  override val isMutable: Boolean,
) extends TypedTermBinding with UniqueIdentifiable {
  override def toString: String = name
}

object LocalVariable {
  def apply(name: String, tpe: Type, isMutable: Boolean): LocalVariable = {
    LocalVariable(UniqueKey.fresh(), name, tpe, isMutable)
  }

  def apply(untypedVariable: UntypedLocalVariable, tpe: Type): LocalVariable = {
    LocalVariable(untypedVariable.uniqueKey, untypedVariable.name, tpe, untypedVariable.isMutable)
  }

  /**
    * Creates an <b>immutable</b> local variable.
    *
    * TODO (multi-import): Needed?
    */
  def apply(name: String, tpe: Type): LocalVariable = LocalVariable(name, tpe, isMutable = false)
}
