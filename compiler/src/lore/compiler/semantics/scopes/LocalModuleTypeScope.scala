package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.{Reporter, ScopeFeedback}
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NameKind, NamePath, Registry}
import lore.compiler.types.NamedSchema
import lore.compiler.utils.CollectionExtensions.OptionExtension

/**
  * A type scope backed by the registry and a local module for name resolution.
  */
case class LocalModuleTypeScope(
  localModule: LocalModule,
  types: Registry.Types,
) extends TypeScope {

  override protected def local(name: String): Option[NamedSchema] = {
    localModule.getPath(name, NameKind.Type).flatMap {
      namePath => types.schemas.get(namePath)
    }
  }

  override protected def resolveAbsolute(absolutePath: NamePath, position: Position)(implicit reporter: Reporter): Option[NamedSchema] = {
    types.schemas
      .get(absolutePath)
      .ifEmpty(reporter.error(ScopeFeedback.UnknownEntry(entryLabel, absolutePath.toString, position)))
  }

}
