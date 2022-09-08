package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{ModuleFeedback, Reporter}
import lore.compiler.resolution.ImportResolver.{AccessibleSource, AccessibleSources}
import lore.compiler.semantics.definitions.{BindingDefinition, BindingDefinitionKind, TermDefinition, TypeDefinition}
import lore.compiler.semantics.modules._
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.DeclNode
import lore.compiler.syntax.DeclNode.ImportNode

/**
  * Due to temporary data that [[ImportResolver]] gathers, it should be instantiated exactly once per local module.
  * Resolving imports with multiple different instances of the import resolver is illegal.
  */
class ImportResolver(localModule: LocalModule)(implicit registry: Registry, reporter: Reporter) {

  private val typeSources: AccessibleSources = new AccessibleSources(localModule, localModule.types)
  private val termSources: AccessibleSources = new AccessibleSources(localModule, localModule.terms)

  /**
    * Resolves `importNodes` for `localModule`, updating the local module's accessibles according to the import rules
    * in the specification. Illegal imports are reported as errors and the local module's accessibles remain unchanged
    * for that specific import.
    *
    * Roughly, imports have the following precedence rules:
    *   - Local declarations have precedence over all imports.
    *   - Wildcard imports are overridden by subsequent wildcard imports and direct imports of the same simple name.
    *   - Wildcard imports following a direct import of the same simple name are ignored.
    *   - Multi-referable bindings of the same kind will be merged until overridden.
    *   - A direct import conflicting with another direct import or local declaration results in an error.
    *
    * For a more complete definition, consult the specification.
    *
    * Imports are first resolved such that an import `use a.b.c` is mapped to its absolute path, which may be more
    * complex if `a` is a relative binding. For example, the absolute path may be `x.y.a.b.c`. In this first step, we
    * only have to resolve the head segment of the import path.
    *
    * To check the rest of the path, in a second step, we take the complete absolute path and check it against the
    * registry. If the registry has a member with the exact path name, the import is valid. If not, the compiler
    * reports an error.
    *
    * An import can start from the current module, any parent of the current module, or a previously imported module.
    */
  def resolve(importNodes: Vector[ImportNode]): Unit = {
    importNodes.foreach(resolveImport)
  }

  /**
    * Resolves a single import according to the rules laid out in [[resolve]].
    */
  private def resolveImport(importNode: DeclNode.ImportNode): Unit = {
    val importPath = resolveAbsoluteImportPath(importNode).getOrElse(return)
    val (typeMembers, termMembers) = resolveImportedBindings(importNode, importPath).getOrElse(return)

    typeMembers.foreach(addAccessibleImport(_, importNode, localModule.types, typeSources))
    termMembers.foreach(addAccessibleImport(_, importNode, localModule.terms, termSources))
  }

  /**
    * Resolves the absolute import path for the given import, or reports an error if the import path cannot be
    * resolved.
    */
  private def resolveAbsoluteImportPath(importNode: ImportNode): Option[NamePath] = {
    val relativeImportPath = importNode.namePathNode.namePath

    // All import paths should have at least two segments. Wildcard imports always fulfill this constraint due to the
    // way they are parsed.
    if (!importNode.isWildcard && relativeImportPath.length < 2) {
      reporter.error(ModuleFeedback.Import.TooShort(importNode))
      return None
    }

    // To get the absolute import path, we must resolve the head segment, which must be a module.
    val headSegment = relativeImportPath.headName
    localModule.terms.getAccessibleMembers(headSegment) match {
      case Some(multiReference) if multiReference.definitionKind == BindingDefinitionKind.Module =>
        Some(multiReference.singleBinding.name ++ relativeImportPath.tail)

      case Some(_) =>
        reporter.error(ModuleFeedback.Import.ModuleExpected(importNode, headSegment))
        None

      case None =>
        reporter.error(ModuleFeedback.Import.NotFound(importNode, headSegment))
        None
    }
  }

  /**
    * Resolve imported types and terms of the definition `importPath`. This depends on the kind of import:
    *   - For wildcard imports, the function resolves all members of a global module.
    *   - For direct imports, we have to verify that the type or term definition at the import path exists.
    */
  private def resolveImportedBindings(
    importNode: ImportNode,
    importPath: NamePath,
  ): Option[(Iterable[TypeDefinition], Iterable[TermDefinition])] = {
    val parentPath = if (importNode.isWildcard) importPath else importPath.parentOrEmpty
    val parentModule = registry.getModule(parentPath).getOrElse {
      reporter.error(ModuleFeedback.Import.NotFound(importNode, parentPath.toString))
      return None
    }

    if (importNode.isWildcard) {
      Some(parentModule.types.all, parentModule.terms.all)
    } else {
      val memberName = importPath.simpleName
      val types = parentModule.types.get(memberName)
      val terms = parentModule.terms.get(memberName)

      if (types.nonEmpty || terms.nonEmpty) {
        Some(types, terms)
      } else {
        reporter.error(ModuleFeedback.Import.NotFound(importNode, importPath.toString))
        None
      }
    }
  }

  /**
    * Attempts to add an imported name path referring to `moduleMember` to the local module's accessibles. Refer to the
    * rules described in [[resolve]] or the specification for details on precedences and failure states.
    *
    * If the import is illegal, an error is reported and the local module's accessibles are not modified.
    */
  private def addAccessibleImport[A <: BindingDefinition](
    moduleMember: A,
    importNode: ImportNode,
    localModuleMembers: LocalModuleMembers[A],
    accessibleSources: AccessibleSources,
  ): Unit = {
    val memberName = moduleMember.name.simpleName
    val importSource = if (importNode.isWildcard) AccessibleSource.WildcardImport else AccessibleSource.DirectImport
    lazy val singleReference = MultiReference(moduleMember.definitionKind, Vector(moduleMember), Vector.empty)

    localModuleMembers.accessibles.get(memberName) match {
      case Some(existingMembers) =>
        if (existingMembers.definitionKind.isMultiReferable && existingMembers.definitionKind == moduleMember.definitionKind) {
          // We can merge the multi references. The new accessible source has to be the source with the highest
          // precedence. For example, if we wildcard-import a member and add it alongside a local definition, the
          // resulting accessible source must still be local. Otherwise, an accessible with higher precedence may be
          // accidentally overridden.
          localModuleMembers.accessibles += memberName -> existingMembers.addLocal(moduleMember)
          accessibleSources.add(memberName, importSource)
        } else {
          val existingSource = accessibleSources.get(memberName)
          (importSource, existingSource) match {
            case (AccessibleSource.DirectImport, AccessibleSource.Local) =>
              // A direct import conflicting with a local declaration results in an error.
              reporter.error(ModuleFeedback.DirectImport.CannotOverrideLocalDeclaration(importNode))

            case (AccessibleSource.DirectImport, AccessibleSource.DirectImport) =>
              // A direct import conflicting with another direct import results in an error, except when the exact same
              // module member has been imported again.
              if (!existingMembers.bindings.contains(moduleMember)) {
                reporter.error(
                  ModuleFeedback.DirectImport.CannotOverrideDirectImport(importNode, existingMembers.bindings)
                )
              }

            case (AccessibleSource.DirectImport, AccessibleSource.WildcardImport) =>
              // Direct imports override wildcard imports.
              localModuleMembers.accessibles += memberName -> singleReference
              accessibleSources.replace(memberName, AccessibleSource.DirectImport)

            case (AccessibleSource.WildcardImport, AccessibleSource.WildcardImport) =>
              // Later wildcard imports override earlier wildcard imports. There is no need to update AccessibleSources
              // because the accessible source is already WildcardImport.
              localModuleMembers.accessibles += memberName -> singleReference

            case _ =>
          }
        }

      case None =>
        localModuleMembers.accessibles += memberName -> singleReference
        accessibleSources.add(memberName, importSource)
    }
  }

}

object ImportResolver {
  /**
    * During import resolution, we have to remember the sources that local module accessibles have come from to apply
    * precedence rules correctly.
    */
  private class AccessibleSources(
    localModule: LocalModule,
    localModuleMembers: LocalModuleMembers[_],
  ) {
    private var sources: Map[String, AccessibleSource] = {
      localModuleMembers.accessibles.keys.map(_ -> AccessibleSource.Local).toMap
    }

    def get(memberName: String): AccessibleSource = {
      sources.getOrElse(
        memberName,
        throw CompilationException(s"Accessible sources must exist if the accessible exists. Local module position:" +
          s" ${localModule.position}."),
      )
    }

    /**
      * If a module member has been added to existing accessibles, the new source has to be merged with the existing
      * source. If no accessible source is recorded, the new source is simply added.
      */
    def add(memberName: String, source: AccessibleSource): Unit = {
      val combinedSource = sources.get(memberName) match {
        case Some(existingSource) => AccessibleSource.max(existingSource, source)
        case None => source
      }
      sources += memberName -> combinedSource
    }

    /**
      * If a module member should replace accessible, the new source has to replace any existing source.
      */
    def replace(memberName: String, source: AccessibleSource): Unit = {
      sources += memberName -> source
    }
  }

  private sealed trait AccessibleSource {
    def precedence: Int
  }

  private object AccessibleSource {
    case object Local extends AccessibleSource {
      override val precedence: Int = 2
    }

    case object DirectImport extends AccessibleSource {
      override val precedence: Int = 1
    }

    case object WildcardImport extends AccessibleSource {
      override val precedence: Int = 0
    }

    /**
      * Returns `s1` or `s2`, whichever source has the highest precedence.
      */
    def max(s1: AccessibleSource, s2: AccessibleSource): AccessibleSource = if (s1.precedence > s2.precedence) s1 else s2
  }
}
