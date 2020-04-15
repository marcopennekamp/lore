package lore.definitions
import lore.compiler.{C, Compilation}
import lore.types.LabelType

class LabelDefinition(override val name: String, override val tpe: LabelType) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[LabelDefinition] = tpe.supertype.map(_.definition)
  override def verifyDeferredTypings: C[Unit] = Compilation.succeed(())
}
