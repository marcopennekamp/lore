package lore.compiler.resolution

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.feedback.{ModuleFeedback, Reporter}
import lore.compiler.semantics.modules._
import lore.compiler.semantics.{BindingKind, NamePath}
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.{DeclNode, NamedDeclNode, TermDeclNode, TypeDeclNode}
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

object ModuleResolver {

  /**
    * Builds a complete global module index and collects all unique declarations contained in every local module. Also
    * builds local modules (including import resolution) and attaches them to their respective [[DeclNode]]s. Duplicate
    * declarations are reported as errors and not added to global modules or collected.
    *
    * Care should be taken to pass all module nodes to this function at once, as module members, duplicate
    * declarations, and imports can only be resolved correctly if a complete understanding of global modules is built.
    */
  def resolve(moduleNodes: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter): (GlobalModuleIndex, DeclarationCollector) = {
    implicit val globalModuleIndex: GlobalModuleIndex = new GlobalModuleIndex
    implicit val declarationCollector: DeclarationCollector = new DeclarationCollector

    // Step 1: Add all module members to their global modules and collect all declarations (ignoring duplicates).
    Type.predefinedTypes.values.foreach(tpe => globalModuleIndex.addMember(tpe.name, BindingKind.Type))
    moduleNodes.foreach(addNode(globalModuleIndex.root, _))

    // Step 2: Build local modules, fetch module members from global modules, and resolve imports.
    moduleNodes.foreach(resolveLocalModule(_, None))

    (globalModuleIndex, declarationCollector)
  }

  /**
    * Adds `node` as a [[ModuleMember]] to the given global module, while also adding all of `node`'s members to
    * `node`'s global module if `node` represents a module. If `node` is an at-root module declaration, it will be
    * added to the root module instead.
    *
    * If `node`'s simple name has already been added to the module, and the binding is not multi-definable, a
    * [[ModuleFeedback.MemberNameTaken]] is reported and the binding isn't added to the module.
    *
    * Unique nodes are collected by the [[DeclarationCollector]].
    */
  def addNode(
    globalModule: GlobalModule,
    node: DeclNode,
  )(
    implicit globalModuleIndex: GlobalModuleIndex,
    declarationCollector: DeclarationCollector,
    reporter: Reporter,
  ): Unit = {
    // TODO (multi-import): Separate this function into an addNode and an addModuleNode. We don't actually need to
    //                      match on `node` twice, only once. The `parentModule` handling is only needed for module
    //                      nodes.
    val root = globalModuleIndex.root
    val (parentModule, nodes) = node match {
      // TODO (multi-import): I think this case can be removed, as it would be handled in the same way by the second
      //                      case. The only difference would be that the members aren't immediately added as members,
      //                      but rather the ModuleNode is processed by `nodes.foreach`, the inner module becomes just
      //                      `globalModule` because `node`'s name path is empty (hence the foldLeft does nothing), and
      //                      all members are added recursively with `addNode`. This would also allow us to work with
      //                      the node directly, without the need to iterate over a `nodes` vector.
      case node: ModuleNode if node.namePath.isEmpty =>
        // A module with an empty name path can only exist in the form of an implicit root module. The given module
        // declaration has no parent module and its declarations must be added to the root module.
        if (globalModule != root) {
          throw CompilationException("A module with an empty name path should not be declared under a parent module" +
            s" `${globalModule.name}`. The module declaration occurs at ${node.position}.")
        }
        (root, node.members)

      case node: ModuleNode =>
        val parentModule = node match {
          case ModuleNode(_, true, _, _, _) => root
          case _ => globalModule
        }
        (parentModule, Vector(node))

      case node => (globalModule, Vector(node))
    }

    def handleStructNode(node: TypeDeclNode): Unit = {
      addModuleMember(globalModule, node.simpleName, BindingKind.Type, node.position).foreach { _ =>
        addModuleMember(globalModule, node.simpleName, BindingKind.Struct, node.position).foreach {
          moduleMember => declarationCollector.collect(node, moduleMember.namePath)
        }
      }
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
            // Do NOT use `node.simpleName`, because it refers to the head segment of a composite module name. Instead,
            // the name path's segments must be folded as is done here.
            addModuleMember(globalModule, simpleModuleName, BindingKind.Module, node.position)
            globalModuleIndex.getOrCreateModule(globalModule.name + simpleModuleName)
        }
        node.members.foreach(addNode(innerModule, _))

      case node: StructNode => handleStructNode(node)
      case node: AliasNode if node.isStructAlias => handleStructNode(node)

      case node: TermDeclNode =>
        val bindingKind = node match {
          case _: ModuleNode => BindingKind.Module
          case _: GlobalVariableNode => BindingKind.GlobalVariable
          case _: FunctionNode => BindingKind.MultiFunction
        }
        addModuleMember(globalModule, node.simpleName, bindingKind, node.position).foreach {
          memberNode => declarationCollector.collect(node, memberNode.namePath)
        }

      case node: TypeDeclNode =>
        addModuleMember(globalModule, node.simpleName, BindingKind.Type, node.position).foreach {
          memberNode => declarationCollector.collect(node, memberNode.namePath)
        }

      case node: SpecNode =>
        // Specs do not need to be added to the global module, because they cannot be referenced from Lore code.
        // Still, spec declarations need to be collected.
        declarationCollector.collect(node)
    }
  }

  /**
    * Adds a module member `simpleName` to `globalModule`. A duplicate declaration is reported as an error and not
    * added to the global module.
    *
    * @return The added module member if the node is not a duplicate, and `None` otherwise.
    */
  private def addModuleMember(
    globalModule: GlobalModule,
    simpleName: String,
    bindingKind: BindingKind,
    position: Position,
  )(implicit reporter: Reporter): Option[ModuleMember] = {
    val globalModuleMembers = globalModule.members(bindingKind.toModuleMemberKind)
    globalModuleMembers.get(simpleName) match {
      case Some(existingMember) =>
        if (
          bindingKind.isMultiDefinable && bindingKind == existingMember.bindingKind
            || bindingKind == BindingKind.Module && existingMember.bindingKind == BindingKind.Struct
            || bindingKind == BindingKind.Struct && existingMember.bindingKind == BindingKind.Module
        ) {
          // Apart from stacking multi-definable members, we have to add companion modules or their respective structs
          // to the global module even if the other half is already registered.
          Some(globalModuleMembers.add(simpleName, bindingKind, Some(position)))
        } else {
          reporter.error(ModuleFeedback.MemberNameTaken(simpleName, existingMember, position))
          None
        }

      case None => Some(globalModuleMembers.add(simpleName, bindingKind, Some(position)))
    }
  }

  /**
    * Resolves [[LocalModule]]s from the given module node (including nested modules), attaching them to their
    * respective [[DeclNode]]s. Also processes the imports contained in `moduleNode` and adds them to the local module.
    */
  private def resolveLocalModule(
    moduleNode: DeclNode.ModuleNode,
    parent: Option[LocalModule],
  )(implicit globalModuleIndex: GlobalModuleIndex, reporter: Reporter): Unit = {
    val modulePath = getLocalModulePath(moduleNode, parent)
    val globalModule = globalModuleIndex.getOrCreateModule(modulePath)

    val typeMembers = resolveLocalMembers(
      moduleNode.members.filterType[TypeDeclNode],
      globalModule,
      ModuleMemberKind.Type,
    )
    val termMembers = resolveLocalMembers(
      moduleNode.members.filterType[NamedDeclNode].filter(isLocalTerm),
      globalModule,
      ModuleMemberKind.Term,
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
      case nestedModuleNode: DeclNode.ModuleNode => resolveLocalModule(nestedModuleNode, Some(localModule))
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
    * `memberNodes` should be pre-filtered so that only relevant [[DeclNode]]s are considered for types and terms. By
    * fetching module members from the global module, global and local module members are represented by the same
    * unique instances.
    */
  private def resolveLocalMembers(
    memberNodes: Vector[NamedDeclNode],
    globalModule: GlobalModule,
    memberKind: ModuleMemberKind,
  ): Map[String, ModuleMember] = {
    // Sometimes, a struct node might be a duplicate and its corresponding constructor/object term might not be
    // declared in the global module. Due to such cases, we cannot assume that member nodes always exist in the global
    // module. If this is the case, errors will already have been reported, so it's just a matter of filtering out
    // these missing members.
    memberNodes
      .flatMap(declNode => globalModule.members(memberKind).get(declNode.simpleName))
      .map(moduleMember => moduleMember.namePath.simpleName -> moduleMember)
      .toMap
  }

}
