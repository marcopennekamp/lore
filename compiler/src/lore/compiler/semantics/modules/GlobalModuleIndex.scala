package lore.compiler.semantics.modules

import lore.compiler.semantics.NamePath
import lore.compiler.syntax.DeclNode
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.NamePathNode

/**
  * The GlobalModuleIndex contains, for each flat module name path, all of the module's members including nested
  * modules as DeclNodes. The members come from all equally named ModuleNodes from across all fragments.
  *
  * The GlobalModuleIndex also provides quick access to all binding and type names declared within the module, for the
  * purposes of resolving simple names and imports.
  */
class GlobalModuleIndex {

  private var index: Map[NamePath, IndexedModule] = Map.empty

  /**
    * Adds the given DeclNode to the indexed module identified by `modulePath`, while also adding all of its members if
    * it's a module.
    */
  def add(node: DeclNode, modulePath: NamePath): Unit = this.synchronized {
    val indexedModule = index.get(modulePath) match {
      case Some(indexedModule) => indexedModule
      case None =>
        val indexedModule = new IndexedModule
        index = index.updated(modulePath, indexedModule)
        indexedModule
    }

    node match {
      case node@DeclNode.ModuleNode(pathNode, imports, members, position) =>
        // A nested module node `module Foo.Bar` has to be added to the global index such that both Foo and Bar are
        // added as separate modules. Foo might not actually be defined anywhere else, so it's important that we
        // de-nest here. The inner module (the right-most name) receives all the imports and declarations, while the
        // outer modules simply get the nested module as the single member.
        // For example, a declaration `module Foo.Bar.Baz` gives all imports and declarations to `Baz`, makes `Baz` a
        // member of `Bar`, and `Bar` a member of `Foo`.
        val denestedModuleNode = if (pathNode.segments.length > 1) {
          val innerModuleNode = DeclNode.ModuleNode(
            NamePathNode(Vector(pathNode.segments.last)),
            imports,
            members,
            position
          )

          pathNode.segments.init.foldRight(innerModuleNode) {
            case (segment, innerModuleNode) => ModuleNode(
              NamePathNode(Vector(segment)),
              Vector.empty,
              Vector(innerModuleNode),
              position
            )
          }
        } else node

        indexedModule.add(denestedModuleNode)
        denestedModuleNode.members.foreach(add(_, modulePath.concat(denestedModuleNode.namePath)))

      case _ => indexedModule.add(node)
    }
  }

  /**
    * This function is the type-centric version of [[findPath]].
    */
  def getTypePath(startPath: NamePath, memberName: String): Option[NamePath] = {
    findPath(_.typeNames)(startPath, memberName)
  }

  /**
    * This function is the binding-centric version of [[findPath]]
    */
  def getBindingPath(startPath: NamePath, memberName: String): Option[NamePath] = {
    findPath(_.bindingNames)(startPath, memberName)
  }

  /**
    * Finds the closest module member with the given name, starting with the module at `startPath` and successively
    * working up the parentage, up to and including the root module. Returns None if none of the modules contain a
    * member called `name`.
    *
    * For example, if we are requesting a member `baz` by simple name inside the module `Foo.Bar`, but `baz` is
    * actually a member of `Foo`, this function will return `Foo.baz`. If `baz` is instead a member of the root, this
    * function will return simply `baz`.
    */
  def findPath(getNames: IndexedModule => Set[String])(startPath: NamePath, memberName: String): Option[NamePath] = {
    def fallback: Option[NamePath] = {
      startPath.parent.flatMap(
        parentPath => findPath(getNames)(parentPath, memberName)
      )
    }

    index.get(startPath) match {
      case Some(indexedModule) =>
        val names = getNames(indexedModule)
        if (names.contains(memberName)) {
          Some(startPath.append(memberName))
        } else fallback

      case None => fallback
    }
  }

}
