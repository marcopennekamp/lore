package lore.compiler.semantics.modules

import lore.compiler.semantics.{NameKind, NamePath}
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
    val indexedModule = getIndexedModule(modulePath)
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

        if (!node.namePath.isRoot) {
          indexedModule.add(denestedModuleNode)
        }
        denestedModuleNode.members.foreach(add(_, modulePath ++ denestedModuleNode.namePath))

      case _ => indexedModule.add(node)
    }
  }

  /**
    * Adds the type or binding `name` to the proper indexed module. This is used to add predefined types to the root
    * indexed module.
    */
  def add(name: NamePath, kind: NameKind): Unit = {
    getIndexedModule(name.parentOrEmpty).add(name.simpleName, kind)
  }

  private def getIndexedModule(modulePath: NamePath): IndexedModule = {
    index.get(modulePath) match {
      case Some(indexedModule) => indexedModule
      case None =>
        val indexedModule = new IndexedModule
        index = index.updated(modulePath, indexedModule)
        indexedModule
    }
  }

  /**
    * Whether the index has a binding or type with the exact name path.
    */
  def has(name: NamePath, nameKind: NameKind): Boolean = {
    index
      .get(name.parentOrEmpty)
      .exists(m => m.has(name.simpleName, nameKind))
  }

  /**
    * Whether the index has a binding and/or type with the exact name path.
    */
  def has(name: NamePath): Boolean = {
    has(name, NameKind.Type) || has(name, NameKind.Binding)
  }

  def root: Option[IndexedModule] = index.get(NamePath.empty)

  /**
    * Finds a path to the module member with the given name, either in the module identified by `modulePath`, or in the
    * root module. Returns None if the member cannot be found.
    *
    * For example, if we are requesting a member via the module path `foo.bar` and member name `baz`, but `baz` is
    * actually a member of `foo`, `getPath` will return None. If `baz` is instead a member of the root, this function
    * will return simply `baz`.
    */
  def getPath(modulePath: NamePath, memberName: String, nameKind: NameKind): Option[NamePath] = {
    def fallback: Option[NamePath] = {
      root.filter(_.has(memberName, nameKind)).map(_ => NamePath(memberName))
    }

    index.get(modulePath) match {
      case Some(indexedModule) =>
        if (indexedModule.has(memberName, nameKind)) {
          Some(modulePath + memberName)
        } else fallback

      case None => fallback
    }
  }

}
