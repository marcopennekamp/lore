package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{OptionExtension, VectorExtension}

/**
  * A scope that provides access to types.
  */
trait TypeScope extends Scope[Type] {
  def register(entry: NamedType, position: Position)(implicit reporter: Reporter): Unit = super.register(entry.name, entry, position)

  /**
    * Fetches a struct type with the given name from the closest scope.
    */
  def getStructType(name: String): Option[StructType] = get(name).filterType[StructType]

  /**
    * Fetches a trait type with the given name from the closest scope.
    */
  def getTraitType(name: String): Option[TraitType] = get(name).filterType[TraitType]

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

/**
  * A type scope that is backed by an existing type map. New types cannot be registered.
  */
case class ImmutableTypeScope(types: Map[String, Type], override val parent: Option[TypeScope]) extends TypeScope {
  override protected def local(name: String): Option[Type] = types.get(name)
  override protected def add(name: String, entry: Type): Unit = throw new UnsupportedOperationException
}
