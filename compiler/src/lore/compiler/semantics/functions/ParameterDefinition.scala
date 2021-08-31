package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.scopes.Variable
import lore.compiler.target.Target
import lore.compiler.transpilation.RuntimeNames
import lore.compiler.types.Type

/**
  * A function parameter definition.
  *
  * The position is restricted to the parameter's name for better error highlighting and index building, as long as the
  * name exists.
  */
case class ParameterDefinition(
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
    * A parameter definition that is definitely named.
    */
  case class NamedParameterDefinition(underlying: ParameterDefinition) {
    val name: String = underlying.name.get
    val tpe: Type = underlying.tpe
    def asVariable: Variable = Variable(name, tpe, isMutable = false)
  }
}
