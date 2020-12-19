package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.StructType

class StructDefinition(
  override val name: String,
  override val tpe: StructType,
  val properties: Vector[PropertyDefinition],
  override val position: Position,
) extends DeclaredTypeDefinition {

  /**
    * The signature of the call-style constructor.
    */
  lazy val constructorSignature: FunctionSignature = FunctionSignature(name, properties.map(_.asParameter), tpe, position)

}
