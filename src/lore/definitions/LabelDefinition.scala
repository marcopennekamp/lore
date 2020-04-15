package lore.definitions
import lore.compiler.{C, Compilation, Position}
import lore.types.LabelType

class LabelDefinition(
  override val name: String,
  override val tpe: LabelType,
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[LabelDefinition] = tpe.supertype.map(_.definition)
  override def verifyDeferredTypings: C[Unit] = Compilation.succeed(())
}
