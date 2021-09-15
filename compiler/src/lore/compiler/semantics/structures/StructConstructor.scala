package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.scopes.TypedBinding
import lore.compiler.types.{FunctionType, StructType}

/**
  * Represents the call-style constructor of the given struct type.
  */
case class StructConstructor(structType: StructType) extends Positioned with TypedBinding {
  val name: NamePath = structType.name
  override val position: Position = structType.schema.definition.position

  val signature: FunctionSignature = FunctionSignature(
    structType.name,
    structType.properties.map(_.asParameter),
    structType,
    structType.schema.definition.position
  )

  override val tpe: FunctionType = signature.functionType
}
