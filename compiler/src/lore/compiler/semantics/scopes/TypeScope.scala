package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.types._

/**
  * A scope that provides access to type schemas.
  */
trait TypeScope extends Scope[NamedSchema] {
  def resolveStatic(
    namePath: NamePath,
    position: Position,
  )(implicit termScope: TermScope, reporter: Reporter): Option[NamedSchema] = {
    resolveStatic(namePath, termScope, position)
  }

  override def entryLabel: String = "type"
}

case class ImmutableTypeScope(
  entries: Map[String, NamedSchema],
  override val optionalParent: Option[TypeScope],
) extends ImmutableScope[NamedSchema] with TypeScope

object ImmutableTypeScope {
  def from(schemas: Vector[NamedSchema], parent: TypeScope): ImmutableTypeScope = {
    ImmutableTypeScope(schemas.map(tpe => (tpe.name.simpleName, tpe)).toMap, Some(parent))
  }
}
