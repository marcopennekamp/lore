package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.syntax.DeclNode
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.Node.NamePathNode

/**
  * The global module index contains a global module for each module name path. Such a module knows all of its members
  * globally. The members come from all module declarations in all fragments.
  */
class GlobalModuleIndex {

  private var index: Map[NamePath, GlobalModule] = Map.empty

  /**
    * Adds the given DeclNode to the indexed module identified by `modulePath`, while also adding all of its members if
    * it's a module.
    */
  def add(node: DeclNode, modulePath: NamePath): Unit = this.synchronized {
    val parentModule = getOrCreateModule(modulePath)
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

        // The denested module node must be added to its parent module, UNLESS the module node is an implicit root
        // module, which is the case when a fragment doesn't contain a top module declaration: the fragment's outer
        // module node has the root name path then. This exception exists because we don't want to add the root module
        // to the root module itself.
        if (!node.namePath.isRoot) {
          parentModule.add(denestedModuleNode)
        }

        // Although global modules are usually created implicitly when the first module member is added to the index,
        // empty modules must be added to the index manually. This also allows us to add a "declared at" position to
        // the module.
        val denestedModulePath = modulePath ++ denestedModuleNode.namePath
        val denestedModule = getOrCreateModule(denestedModulePath)
        denestedModule.addModulePosition(position)
        denestedModuleNode.members.foreach(add(_, denestedModulePath))

      case _ => parentModule.add(node)
    }
  }
  // A global module index should always have its root module set. This simplifies various parts of resolution.
  getOrCreateModule(NamePath.empty)

  /**
    * The root module contains all members which aren't declared as part of a module themselves.
    */
  val root: GlobalModule = {
    index.getOrElse(NamePath.empty, throw new IllegalStateException("The root module must exist in a global module."))
  }

  /**
    * Returns all global modules.
    */
  def modules: Iterable[GlobalModule] = index.values

  /**
    * Gets the global module with the given name, if it exists.
    */
  def getModule(modulePath: NamePath): Option[GlobalModule] = index.get(modulePath)

  /**
    * Gets the global module with the given name if it exists, or creates a new one.
    */
  private def getOrCreateModule(modulePath: NamePath): GlobalModule = {
    getModule(modulePath) match {
      case Some(globalModule) => globalModule
      case None =>
        val globalModule = new GlobalModule(modulePath)
        index = index.updated(modulePath, globalModule)
        globalModule
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
      case Some(globalModule) =>
        if (globalModule.has(memberName, nameKind)) {
          Some(modulePath + memberName)
        } else fallback

      case None => fallback
    }
  }

  /**
    * Adds the type or binding `name` to the proper global module directly.
    */
  def addMember(name: NamePath, kind: NameKind): Unit = this.synchronized {
    getOrCreateModule(name.parentOrEmpty).add(name.simpleName, Position.unknown, kind)
  }

}
