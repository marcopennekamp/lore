package lore.compiler.semantics.scopes

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Position
import lore.compiler.types.{NamedType, Type, TypeVariable}
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * A scope that provides access to types.
  */
trait TypeScope extends Scope[Type] {
  def register(entry: NamedType)(implicit position: Position): Verification = super.register(entry.name, entry)

  override def entryLabel: String = "type"
}

/**
  * A scope that provides access to locally declared types, such as type variables. The parent MUST be defined, either
  * as another local type scope or the registry.
  *
  * This scope is currently used to make type variables available to a function's scope.
  */
class LocalTypeScope(parent: TypeScope) extends BasicScope[Type](Some(parent)) with TypeScope {

  /**
    * All type variables declared in the local scope.
    */
  def localTypeVariables: Vector[TypeVariable] = entries.values.toVector.filterType[TypeVariable]

}
