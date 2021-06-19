package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.scopes.TypedVariable
import lore.compiler.target.Target
import lore.compiler.types.FunctionType

/**
  * Represents the call-style constructor of the given struct.
  */
case class StructConstructorDefinition(struct: StructDefinition) extends Positioned with TypedVariable {
  override val position: Position = struct.position
  override val name: String = struct.name
  override lazy val tpe: FunctionType = signature.functionType
  override lazy val targetVariable: Target.Variable = RuntimeNames.constructor(struct.tpe)

  lazy val signature: FunctionSignature = FunctionSignature(struct.name, struct.properties.map(_.asParameter), struct.tpe, position)
}
