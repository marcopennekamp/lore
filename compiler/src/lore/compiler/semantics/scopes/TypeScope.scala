package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{OptionExtension, VectorExtension}

/**
  * A scope that provides access to type schemas.
  */
trait TypeScope extends Scope[NamedSchema] {
  def register(entry: NamedSchema, position: Position)(implicit reporter: Reporter): Unit = super.register(entry.name, entry, position)

  /**
    * Fetches a struct schema with the given name from the closest scope.
    */
  def getStructSchema(name: String): Option[StructSchema] = get(name).filterType[StructSchema]

  /**
    * Fetches a trait schema with the given name from the closest scope.
    */
  def getTraitSchema(name: String): Option[TraitSchema] = get(name).filterType[TraitSchema]

  override def entryLabel: String = "type"
}

/**
  * A scope that provides access to locally declared type schemas, such as type variables. The parent MUST be defined,
  * either as another local type scope or the registry.
  *
  * This scope is currently used to make type variables available to a function's scope.
  */
class LocalTypeScope(parent: TypeScope) extends BasicScope[NamedSchema](Some(parent)) with TypeScope {

  /**
    * All type variables declared in the local scope.
    */
  def localTypeVariables: Vector[TypeVariable] = entries.values.toVector.filterType[TypeVariable]

}

/**
  * A type scope that is backed by an existing schema map. New schemas cannot be registered.
  */
case class ImmutableTypeScope(schemas: Registry.Types, override val parent: Option[TypeScope]) extends TypeScope {
  override protected def local(name: String): Option[NamedSchema] = schemas.get(name)
  override protected def add(name: String, entry: NamedSchema): Unit = throw new UnsupportedOperationException
}
