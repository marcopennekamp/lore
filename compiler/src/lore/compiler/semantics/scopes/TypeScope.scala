package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.OptionExtension

/**
  * A scope that provides access to type schemas.
  */
trait TypeScope extends Scope[NamedSchema] {
  def register(entry: NamedSchema, position: Position)(implicit reporter: Reporter): Unit = super.register(entry.name, entry, position)

  /**
    * Fetches an alias schema with the given name from the closest scope.
    */
  def getAliasSchema(name: String): Option[AliasSchema] = get(name).filterType[AliasSchema]

  /**
    * Fetches a trait schema with the given name from the closest scope.
    */
  def getTraitSchema(name: String): Option[TraitSchema] = get(name).filterType[TraitSchema]

  /**
    * Fetches a struct schema with the given name from the closest scope.
    */
  def getStructSchema(name: String): Option[StructSchema] = get(name).filterType[StructSchema]

  override def entryLabel: String = "type"
}

/**
  * A type scope that is backed by an existing schema map. New schemas cannot be registered.
  */
case class ImmutableTypeScope(schemas: Registry.Schemas, override val parent: Option[TypeScope]) extends TypeScope {
  override protected def local(name: String): Option[NamedSchema] = schemas.get(name)
  override protected def add(name: String, entry: NamedSchema): Unit = throw new UnsupportedOperationException
}

object ImmutableTypeScope {
  def from(schemas: Vector[NamedSchema], parent: TypeScope): ImmutableTypeScope = {
    ImmutableTypeScope(schemas.map(tpe => (tpe.name, tpe)).toMap, Some(parent))
  }
}
