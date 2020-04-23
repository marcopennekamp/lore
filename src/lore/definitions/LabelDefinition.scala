package lore.definitions
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Position
import lore.compiler.{Compilation, Registry}
import lore.types.LabelType

class LabelDefinition(
  override val name: String,
  override val tpe: LabelType,
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[LabelDefinition] = tpe.supertype.map(_.definition)
  override def verifyDeferredTypings: Verification = Compilation.succeed(())
  override def verifyConstraints(implicit registry: Registry): Verification = {
    // TODO: Do we need to check any constraints here at all?
    Compilation.succeed(())
  }
}
