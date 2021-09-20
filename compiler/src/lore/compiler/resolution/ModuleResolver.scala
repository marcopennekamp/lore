package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{ModuleFeedback, Reporter}
import lore.compiler.semantics.modules.LocalModule.ImportMap
import lore.compiler.semantics.modules.{GlobalModuleIndex, LocalModule}
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.syntax.{BindingDeclNode, DeclNode, TypeDeclNode}
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

object ModuleResolver {

  /**
    * Resolves a list of module nodes and produces LocalModules in the process. The function will also build a
    * GlobalModuleIndex. The imports of each local module are processed fully and can be accessed through each local
    * module's import map.
    */
  def resolve(moduleNodes: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter): (Vector[LocalModule], GlobalModuleIndex) = {
    // Step 1: Build the ModuleNodeIndex, which will be used to resolve name paths for imports and scopes.
    implicit val globalModuleIndex: GlobalModuleIndex = new GlobalModuleIndex
    Type.predefinedTypes.values.foreach(tpe => globalModuleIndex.add(tpe.name, NameKind.Type))
    moduleNodes.foreach(globalModuleIndex.add(_, NamePath.empty))

    // Step 2: Flatten module nodes and resolve imports.
    val localModules = moduleNodes.flatMap(resolve(_, None))

    // Step 3: Annotate each DeclNode with the LocalModule it's declared in. This allows type resolution and later
    //         phases to easily build LocalModule*Scopes.
    localModules.foreach(annotateNodes)

    (localModules, globalModuleIndex)
  }

  /**
    * Resolves the given module node, producing a list of LocalModules. Also processes the imports contained in
    * `moduleNode` and adds them to the import map of the local module.
    *
    * We first build a LocalModule instance, then resolve its imports, and finally we resolve nested module nodes
    * (passing the LocalModule instance as a parent).
    */
  private def resolve(
    moduleNode: DeclNode.ModuleNode,
    parent: Option[LocalModule],
  )(implicit globalModuleIndex: GlobalModuleIndex, reporter: Reporter): Vector[LocalModule] = {
    val modulePath = parent.map(_.modulePath).getOrElse(NamePath.empty) ++ moduleNode.namePath

    val localTypeNames: Set[String] = {
      moduleNode.members.filterType[TypeDeclNode].map(_.simpleName).toSet
    }

    val localBindingNames: Set[String] = {
      moduleNode.members.flatMap {
        case node: DeclNode.ModuleNode => Some(node.namePath.headName)
        case node: BindingDeclNode => Some(node.simpleName)
        // Structs always have a constructor or an object and thus also define binding names.
        case node: DeclNode.StructNode => Some(node.simpleName)
        case _ => None
      }.toSet
    }

    val starterModule = LocalModule(
      modulePath,
      parent,
      moduleNode.members,
      localTypeNames,
      localBindingNames,
      Map.empty,
      Map.empty,
      moduleNode.namePathNode.position
    )

    val localModule = moduleNode.imports.foldLeft(starterModule) {
      case (localModule, importNode) => resolve(importNode, localModule)
    }

    val nestedLocalModules = moduleNode.members.flatMap {
      case nestedModuleNode: DeclNode.ModuleNode => resolve(nestedModuleNode, Some(localModule))
      case _ => Vector.empty
    }
    Vector(localModule) ++ nestedLocalModules
  }

  /**
    * Resolves the given import node for a given local module, building a new local module with its import map updated,
    * as long as the import is valid. If the import is invalid in any way, an unchanged local module is returned
    * instead.
    *
    * Imports are first resolved such that an import `use a.b.c` is mapped to its absolute path, which may be more
    * complex if `a` is a relative binding. For example, the absolute path may be `x.y.a.b.c`. In this first step, we
    * only have to resolve the head segment of the import path.
    *
    * To check the rest of the path, in a second step, we take the complete absolute path and check it against the
    * GlobalModuleIndex. If the index has a member with the exact path name, the import is valid. If not, the compiler
    * reports an error.
    *
    * An import can start from the current module, any parent of the current module, or a previously imported module.
    *
    * <h3>Example</h3>
    *
    * TODO (modules): Turn this into a test case instead.
    *
    * <pre>
    * module abc
    *
    * use A.fuz
    * use B.bar
    *
    * module A do
    *   use B.buz
    *   func foo(): Int = 5
    *   func fuz(): Int = buz()
    * end
    *
    * module B do
    *   use A.foo
    *   func bar(): Int = foo()
    *   func buz(): Int = 7
    * end
    *
    * func c(): Int = fuz() + bar()
    * </pre>
    *
    * This example demonstrates how nested modules and imports should interact. Module `A` should know module `B` even
    * if `B` is declared after `A`. And the outer module `abc` should be able to import from `A` and `B` before either
    * are declared.
    */
  private def resolve(
    importNode: DeclNode.ImportNode,
    localModule: LocalModule,
  )(implicit globalModuleIndex: GlobalModuleIndex, reporter: Reporter): LocalModule = {
    val importPath = importNode.namePathNode.namePath
    if (importNode.isWildcard && importPath.length < 1) {
      reporter.error(ModuleFeedback.Import.Wildcard.TooShort(importNode))
      return localModule
    } else if (!importNode.isWildcard && importPath.length < 2) {
      reporter.error(ModuleFeedback.Import.TooShort(importNode))
      return localModule
    }

    // The head segment is the segment that we have to resolve to get the absolute import path. Because the head
    // segment must be a module to contain any meaningful members to import, we can avoid calling `getPath` with
    // `NameKind.Type`.
    val absolutePath = localModule.getPath(importPath.headName, NameKind.Binding).map(prefix => prefix ++ importPath.tail) match {
      case Some(absolutePath) => absolutePath
      case None =>
        reporter.error(ModuleFeedback.Import.UnresolvedHeadSegment(importNode, importPath.headName))
        return localModule
    }

    // The second step depends on whether the import is a wildcard import. If it is, we can get the global module for
    // the absolute path, as a wildcard import path describes a module. If the import is single, we must verify that
    // the absolute path is valid.
    val (importedTypes, importedBindings) = if (importNode.isWildcard) {
      val globalModule = globalModuleIndex.getModule(absolutePath).getOrElse {
        reporter.error(ModuleFeedback.Import.Wildcard.NotFound(importNode, absolutePath))
        return localModule
      }

      def pathsFor(names: Set[String]) = names.toVector.map(absolutePath + _)
      (pathsFor(globalModule.typeNames), pathsFor(globalModule.bindingNames))
    } else {
      if (!globalModuleIndex.has(absolutePath)) {
        reporter.error(ModuleFeedback.Import.NotFound(importNode, absolutePath))
        return localModule
      }

      def pathsFor(nameKind: NameKind) = if (globalModuleIndex.has(absolutePath, nameKind)) Vector(absolutePath) else Vector.empty
      (pathsFor(NameKind.Type), pathsFor(NameKind.Binding))
    }

    def updateImportMap(importMap: ImportMap, namePaths: Vector[NamePath]): ImportMap = {
      namePaths.foldLeft(importMap) {
        case (importMap, path) => importMap + (path.simpleName -> path)
      }
    }

    localModule.copy(
      typeImportMap = updateImportMap(localModule.typeImportMap, importedTypes),
      bindingImportMap = updateImportMap(localModule.bindingImportMap, importedBindings),
    )
  }

  /**
    * Annotates all DeclNodes contained in the local module with the instance of the local module.
    */
  private def annotateNodes(localModule: LocalModule): Unit = {
    localModule.members.foreach {
      node =>
        if (node.localModule != null) {
          throw CompilationException(s"`localModule` for DeclNode $node was already set!")
        }
        node.localModule = localModule
    }
  }

}
