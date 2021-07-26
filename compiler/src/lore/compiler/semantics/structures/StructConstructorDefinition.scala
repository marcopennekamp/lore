package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.scopes.TypedBinding
import lore.compiler.target.Target
import lore.compiler.types.FunctionType

/**
  * Represents the call-style constructor of the given struct.
  *
  * TODO (schemas): Overhaul this completely. This needs to be parameterized.
  */
case class StructConstructorDefinition(struct: StructDefinition) extends Positioned with TypedBinding {
  override val position: Position = struct.position
  override val name: String = struct.name
  override lazy val tpe: FunctionType = signature.functionType
  override lazy val targetVariable: Target.Variable = RuntimeNames.constructor(struct.schema)

  lazy val signature: FunctionSignature = {
    // TODO (schemas): This is only a temporary fix. We will have to refactor struct constructors completely.
    val tpe = struct.schema.instantiateConstant()
    FunctionSignature(struct.name, tpe.properties.map(_.asParameter), tpe, position)
  }
}
