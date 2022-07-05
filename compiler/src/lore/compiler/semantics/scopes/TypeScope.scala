package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.TypeBinding
import lore.compiler.types._

/**
  * A scope that provides access to type schemas.
  */
trait TypeScope extends Scope[TypeBinding] {
  def resolveStatic(
    namePath: NamePath,
    position: Position,
  )(implicit termScope: TermScope, reporter: Reporter): Option[TypeBinding] = {
    resolveStatic(namePath, termScope, position)
  }

  override def entryLabel: String = "type"
}

case class ImmutableTypeScope(
  entries: Map[String, TypeBinding],
  override val optionalParent: Option[TypeScope],
) extends ImmutableScope[TypeBinding] with TypeScope

object ImmutableTypeScope {
  def from(schemas: Vector[NamedSchema], parent: TypeScope): ImmutableTypeScope = {
    ImmutableTypeScope(schemas.map(tpe => (tpe.name.simpleName, tpe)).toMap, Some(parent))
  }
}
