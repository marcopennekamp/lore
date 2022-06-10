package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned, UniqueKey}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.types.Type

/**
  * A function parameter definition. The unique key identifies local variables.
  *
  * The position is restricted to the parameter's name for better error highlighting and index building, as long as the
  * name exists.
  */
case class ParameterDefinition(
  uniqueKey: UniqueKey,
  name: Option[String],
  tpe: Type,
  override val position: Position,
) extends Positioned {
  override def toString: String = name match {
    case Some(name) => s"$name: $tpe"
    case None => tpe.toString
  }
}

object ParameterDefinition {
  /**
    * A view on a parameter definition that is definitely named.
    */
  case class NamedParameterView(definition: ParameterDefinition) {
    val name: String = definition.name.get
    val tpe: Type = definition.tpe
    lazy val asVariable: LocalVariable = LocalVariable(definition.uniqueKey, name, tpe, isMutable = false)
  }
}
