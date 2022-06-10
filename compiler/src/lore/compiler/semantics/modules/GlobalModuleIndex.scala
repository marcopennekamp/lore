package lore.compiler.semantics.modules

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.{BindingKind, NamePath}
import lore.compiler.syntax.DeclNode.{AliasNode, ModuleNode, SpecNode, StructNode}
import lore.compiler.syntax.{TermDeclNode, DeclNode, TypeDeclNode}

/**
  * The global module index contains a global module for each module name path. Such a module knows all of its members
  * globally. The members come from all module declarations in all fragments.
  */
class GlobalModuleIndex {

  private var index: Map[NamePath, GlobalModule] = Map.empty

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
    * Whether the index has a binding with the exact name path.
    */
  def has(name: NamePath, bindingKind: BindingKind): Boolean = {
    index
      .get(name.parentOrEmpty)
      .exists(m => m.has(name.simpleName, bindingKind))
  }

  /**
    * Whether the index has a binding with the exact name path.
    */
  def has(name: NamePath): Boolean = {
    has(name, BindingKind.Type) || has(name, BindingKind.Term)
  }

  /**
    * Adds `node` to the given global module, while also adding all of `node`'s members to `node`'s global module. If
    * `node` is an at-root module declaration, it will be added to the root module instead.
    */
  def addNode(globalModule: GlobalModule, node: DeclNode): Unit = this.synchronized {
    val (parentModule, nodes) = node match {
      case node: ModuleNode if node.namePath.isEmpty =>
        // A module with an empty name path can only exist in the form of an implicit root module, which is the case
        // when a fragment doesn't contain a top module declaration. In such cases, the given module declaration has no
        // parent module and its declarations must be added to the root module.
        if (globalModule != root) {
          throw CompilationException("A module with an empty name path should not be declared under a parent module" +
            s" `${globalModule.name}`. The module declaration occurs at ${node.position}.")
        }
        root.addModulePosition(node.position)
        (root, node.members)

      case node: ModuleNode =>
        val parentModule = node match {
          case ModuleNode(_, true, _, _, _) => root
          case _ => globalModule
        }
        (parentModule, Vector(node))

      case node => (globalModule, Vector(node))
    }

    nodes.foreach {
      case node: ModuleNode =>
        // A nested module node `module Foo.Bar.Baz` has to be added to the global index such that `Foo`, `Bar`, and
        // `Baz` are added as separate modules. `Foo` and `Bar` might not actually be defined anywhere else, so it's
        // important to have these global modules created in the index. The inner module receives all member names,
        // while the outer modules simply have the nested module as their sole member. Given `module Foo.Bar.Baz`,
        // `Foo` becomes a member of the root module, `Bar` a member of `Foo`, `Baz` a member of `Bar`, and all members
        // are registered to `Baz`.
        val innerModule = node.namePath.segments.foldLeft(parentModule) {
          case (globalModule, simpleModuleName) =>
            globalModule.add(simpleModuleName, node.position, BindingKind.Term)
            val nestedModule = getOrCreateModule(globalModule.name + simpleModuleName)
            nestedModule.addModulePosition(node.position)
            nestedModule
        }
        node.members.foreach(addNode(innerModule, _))

      case node: StructNode => globalModule.add(node.simpleName, node.position)
      case node: AliasNode if node.isStructAlias => globalModule.add(node.simpleName, node.position)
      case node: TermDeclNode => globalModule.add(node.simpleName, node.position, BindingKind.Term)
      case node: TypeDeclNode => globalModule.add(node.simpleName, node.position, BindingKind.Type)
      case _: SpecNode =>
        // Specs do not need to be added to the global module, because they cannot be referenced from Lore code.
    }
  }

  /**
    * Adds `node` to the root global module, while also adding all of `node`'s members to `node`'s global module.
    */
  def addNodeToRoot(node: DeclNode): Unit = addNode(root, node)

  /**
    * Adds the binding `name` to the proper global module directly.
    */
  def addMember(name: NamePath, kind: BindingKind): Unit = this.synchronized {
    getOrCreateModule(name.parentOrEmpty).add(name.simpleName, Position.unknown, kind)
  }

}
