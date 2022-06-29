package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.semantics.modules.{BindingModuleMember, GlobalModule, LocalModule, TermModuleMember, TypeModuleMember}
import lore.compiler.syntax.DeclNode.{FunctionNode, GlobalVariableNode, ModuleNode}
import lore.compiler.syntax.{DeclNode, NamedDeclNode, TypeDeclNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension

import scala.reflect.ClassTag

object LocalModuleResolver {

  /**
    * Resolves [[LocalModule]]s from the given module node (including nested modules), attaching them to their
    * respective [[DeclNode]]s. Also processes the imports contained in `moduleNode`, adding them to the local module.
    */
  def resolve(moduleNode: ModuleNode)(implicit registry: Registry, reporter: Reporter): Unit = {
    resolve(moduleNode, None)
  }

  /**
    * Resolves [[LocalModule]]s from the given module node (including nested modules), attaching them to their
    * respective [[DeclNode]]s. Also processes the imports contained in `moduleNode`, adding them to the local module.
    */
  private def resolve(
    moduleNode: ModuleNode,
    parent: Option[LocalModule],
  )(implicit registry: Registry, reporter: Reporter): Unit = {
    val modulePath = getLocalModulePath(moduleNode, parent)
    val globalModule = registry.getOrCreateModule(modulePath)

    val typeMembers = resolveLocalMembers[TypeModuleMember](
      moduleNode.members.filterType[TypeDeclNode],
      globalModule,
    )
    val termMembers = resolveLocalMembers[TermModuleMember](
      moduleNode.members.filterType[NamedDeclNode].filter(isLocalTerm),
      globalModule,
    )
    val localModule = new LocalModule(
      globalModule,
      parent,
      typeMembers,
      termMembers,
      moduleNode.namePathNode.position,
    )

    val importResolver = new ImportResolver(localModule)
    importResolver.resolve(moduleNode.imports)

    annotateDeclNodes(moduleNode, localModule)
    moduleNode.members.foreach {
      case nestedModuleNode: DeclNode.ModuleNode => resolve(nestedModuleNode, Some(localModule))
      case _ =>
    }
  }

  private def getLocalModulePath(moduleNode: ModuleNode, parent: Option[LocalModule]): NamePath = {
    // At-root module declarations don't carry forward the names of their parents.
    if (moduleNode.atRoot) moduleNode.namePath
    else parent.map(_.globalModule.name).getOrElse(NamePath.empty) ++ moduleNode.namePath
  }

  /**
    * Whether `node` is a local term that should be included as a member in a local module.
    */
  private def isLocalTerm(node: NamedDeclNode): Boolean = node match {
    // At-root module declarations shouldn't be added as local bindings, per the specification.
    case node: DeclNode.ModuleNode if node.atRoot => false

    // Structs always have a constructor or an object and thus also define term names. The same applies to struct
    // aliases.
    case _: DeclNode.StructNode => true
    case node: DeclNode.AliasNode if node.isStructAlias => true

    case _: DeclNode.ModuleNode | _: GlobalVariableNode | _: FunctionNode => true
    case _ => false
  }

  /**
    * Annotate each [[DeclNode]] in `moduleNode` with the [[LocalModule]] it's declared in. This allows type resolution
    * and later phases to easily build LocalModule*Scopes.
    */
  private def annotateDeclNodes(moduleNode: ModuleNode, localModule: LocalModule): Unit = {
    moduleNode.members.foreach(node => node.localModule = localModule)
  }

  /**
    * `memberNodes` should be pre-filtered so that only relevant [[DeclNode]]s are considered for types and terms.
    */
  private def resolveLocalMembers[A <: BindingModuleMember](
    memberNodes: Vector[NamedDeclNode],
    globalModule: GlobalModule,
  )(implicit memberTag: ClassTag[A]): Map[String, A] = {
    // Sometimes, a struct node might be a duplicate and its corresponding constructor/object term might not be
    // declared in the global module. Due to such cases, we cannot assume that member nodes always exist in the global
    // module. If this is the case, errors will already have been reported, so it's just a matter of filtering out
    // these missing members.
    memberNodes
      .flatMap(declNode => globalModule.members[A].get(declNode.simpleName))
      .map(moduleMember => moduleMember.simpleName -> moduleMember)
      .toMap
  }

}
