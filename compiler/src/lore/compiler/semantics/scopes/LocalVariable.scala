package lore.compiler.semantics.scopes

import lore.compiler.core.UniqueKey
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
  */
case class LocalVariable(
  uniqueKey: UniqueKey,
  name: String,
  tpe: Type,
  override val isMutable: Boolean,
) extends TypedBinding {
  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVariable => this.uniqueKey == other.uniqueKey
    case _ => false
  }

  override def hashCode(): Int = uniqueKey.hashCode()

  override def toString: String = name
}

object LocalVariable {
  def apply(name: String, tpe: Type, isMutable: Boolean): LocalVariable = {
    LocalVariable(UniqueKey.fresh(), name, tpe, isMutable)
  }

  /**
    * Creates an <b>immutable</b> local variable.
    */
  def apply(name: String, tpe: Type): LocalVariable = LocalVariable(name, tpe, isMutable = false)
}
