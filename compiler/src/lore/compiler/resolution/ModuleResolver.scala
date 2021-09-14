package lore.compiler.resolution

import lore.compiler.feedback.{ModuleFeedback, Reporter}
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.semantics.modules.{GlobalModuleIndex, LocalModule}
import lore.compiler.syntax.{BindingDeclNode, DeclNode, TypeDeclNode}

object ModuleResolver {

  /**
    * TODO (modules): Synthesize documentation from the step list.
    */
  def resolve(moduleNodes: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter) = {
    // Step 1: Build the ModuleNodeIndex, which will be used to resolve name paths for imports and scopes.
    implicit val index: GlobalModuleIndex = new GlobalModuleIndex
    moduleNodes.foreach(index.add(_, NamePath.empty))

    // Step 2: Flatten module nodes and resolve imports.
    val localModules = moduleNodes.flatMap(resolve(_, None))

    // TODO (modules): Go through the local modules and annotate each DeclNode with it. This will allow type resolution
    //                 and later phases to easily build LocalModule*Scopes.

    ???
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

    val localTypeNames = moduleNode.members.flatMap {
      case node: TypeDeclNode => Some(node.name)
      case _ => None
    }.toSet

    val localBindingNames = moduleNode.members.flatMap {
      case DeclNode.ModuleNode(namePathNode, _, _, _) => Some(namePathNode.segments.head.value)
      case node: BindingDeclNode => Some(node.name)
      case _ => None
    }.toSet

    val localModule = moduleNode.imports.foldLeft(LocalModule(modulePath, parent, localTypeNames, localBindingNames, Map.empty)) {
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
    * Imports
    * are first resolved such that an import `use a.b.c` is mapped to its absolute path, which may be more complex if
    * `a` is a relative binding. For example, the absolute path may be `x.y.a.b.c`. In this first step, we only have to
    * resolve the first segment of the import path.
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
    if (importPath.length < 2) {
      reporter.error(ModuleFeedback.Import.TooShort(importNode))
      return localModule
    }

    // The head segment is the segment that we have to resolve to get the absolute import path. Because the head
    // segment must be a module to contain any meaningful members to import, we can avoid calling `getPath` with
    // `NameKind.Type`.
    val headSegment = importPath.segments.head.name
    val absolutePath = localModule.getPath(headSegment, NameKind.Binding).map(prefix => prefix ++ importPath.tail) match {
      case Some(absolutePath) => absolutePath
      case None =>
        reporter.error(ModuleFeedback.Import.UnresolvedHeadSegment(importNode, headSegment))
        return localModule
    }

    // In this second step, we have to verify that the absolute path is valid as checked against the global index.
    if (!globalModuleIndex.has(absolutePath)) {
      reporter.error(ModuleFeedback.Import.NotFound(importNode, absolutePath))
      return localModule
    }

    localModule.copy(
      importMap = localModule.importMap.updated(absolutePath.simpleName, absolutePath)
    )
  }

}
