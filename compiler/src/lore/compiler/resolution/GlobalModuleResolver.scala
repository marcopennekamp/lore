package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{ModuleFeedback, Reporter}
import lore.compiler.semantics.bindings.StructBinding
import lore.compiler.semantics.definitions.BindingDefinition
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules._
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.{DeclNode, NamedDeclNode}

object GlobalModuleResolver {

  /**
    * Adds all global modules and their module members (found in the given module node) in the form of created
    * [[BindingDefinition]]s to their respective global modules. If a module node is an at-root module declaration, it
    * will be added to the root module instead.
    *
    * If a node's simple name has already been added to a global module, and the new member cannot be merged with the
    * existing member, a [[ModuleFeedback.MemberNameTaken]] is reported and the member isn't created or added to the
    * module.
    */
  def resolve(moduleNode: ModuleNode)(implicit registry: Registry, reporter: Reporter): Unit = {
    addNode(registry.rootModule, moduleNode)
  }

  private def addNode(
    globalModule: GlobalModule,
    node: DeclNode,
  )(implicit registry: Registry, reporter: Reporter): Unit = node match {
    case node: NamedDeclNode => AddMember(node, globalModule).apply()
    case node: SpecNode => globalModule.addSpec(SpecDefinitionResolver.create(node))
  }

  /**
    * [[AddMember]] exists due to the complexity of adding a `node` to a `parentModule`. Sharing the class-wide
    * properties across helper functions simplifies the syntax of these functions and their calls.
    *
    * In particular, [[AddMember]] guarantees that a module member is only instantiated if it'll actually be added to
    * the global module. This avoids an awkward situation where Definitions have to be mindful that they might be
    * created without actually being used.
    *
    * A multi-definable `moduleMember` may be merged with an already existing module member if their binding kinds
    * agree. An edge case is concerned with structs and companion modules: Adding a struct type overrides an already
    * existing module with the same name. Vice versa, a module is ignored if a corresponding struct type already
    * exists, though the module is added to the struct's companion modules.
    */
  private case class AddMember(
    node: NamedDeclNode,
    parentModule: GlobalModule,
  )(implicit registry: Registry, reporter: Reporter) {

    def apply(): Unit = node match {
      case node: ModuleNode =>
        // A nested module node `module Foo.Bar.Baz` has to be added to the registry such that `Foo`, `Bar`, and `Baz`
        // are added as separate modules. `Foo` and `Bar` might not actually be defined anywhere else, so it's important
        // to have these global modules created in the registry. The inner module receives all members, while the outer
        // modules simply have the nested module as their sole member. Given `module Foo.Bar.Baz`, `Foo` becomes a member
        // of the root module, `Bar` a member of `Foo`, `Baz` a member of `Bar`, and all members are registered to `Baz`.
        val actualParentModule = node match {
          case ModuleNode(_, true, _, _, _) => registry.rootModule
          case _ => parentModule
        }
        val innerModule = node.namePath.segments.foldLeft(actualParentModule) {
          case (actualParentModule, simpleModuleName) =>
            // Do NOT use `node.simpleName`, because it refers to the head segment of a composite module name. Instead,
            // the name path's segments must be folded as is done here.
            val globalModule = registry.getOrCreateModule(actualParentModule.name + simpleModuleName)
            globalModule.addPosition(node.position)
            addModule(actualParentModule, globalModule)
            globalModule
        }
        node.members.foreach(addNode(innerModule, _))

      case node: AliasNode =>
        addSimple(parentModule.types, () => AliasSchemaResolver.create(node, parentModule)).foreach {
          case schema if schema.aliasVariant.isStructAlias =>
            addStructBinding(() => AliasSchemaResolver.createStructBinding(schema))
          case _ =>
        }

      case node: StructNode =>
        addSimple(parentModule.types, () => StructSchemaResolver.create(node, parentModule)).foreach {
          schema => addStructBinding(() => StructSchemaResolver.createStructBinding(schema))
        }

      case node: TraitNode =>
        addSimple(parentModule.types, () => TraitSchemaResolver.create(node, parentModule))

      case node: GlobalVariableNode =>
        addSimple(parentModule.terms, () => GlobalVariableDefinitionResolver.create(node, parentModule))

      case node: FunctionNode =>
        addWithMerge(parentModule.terms, () => MultiFunctionDefinition(fullName, node)) {
          case mf: MultiFunctionDefinition =>
            mf.addFunctionNode(node)
            Some(mf)
        }

      case _ => throw CompilationException(s"Unexpected node: $node")
    }

    /**
      * Adds a module member created by `create` to the given global module members. Merge semantics are defined by
      * `handleExisting`.
      *
      * Care should be taken not to assume [[AddMember]]'s parent module or the node's simple name in this function, as
      * it may be called with contracted modules.
      *
      * @return The created module member if it has been added to the global module, an existing module member if the
      *         new member was merged into it, or `None` otherwise.
      */
    private def addWithMerge[A <: BindingDefinition](
      globalModuleMembers: GlobalModuleMembers[A],
      simpleName: String,
      create: () => A,
    )(
      handleExisting: PartialFunction[A, Option[A]],
    )(implicit reporter: Reporter): Option[A] = {
      globalModuleMembers.get(simpleName) match {
        case None =>
          val moduleMember = create()
          globalModuleMembers.add(moduleMember)
          Some(moduleMember)

        case Some(existingMember) if handleExisting.isDefinedAt(existingMember) =>
          handleExisting(existingMember)

        case Some(existingMember) =>
          reporter.error(ModuleFeedback.MemberNameTaken(simpleName, existingMember, node.position))
          None
      }
    }

    private def addWithMerge[A <: BindingDefinition](
      globalModuleMembers: GlobalModuleMembers[A],
      create: () => A,
    )(handleExisting: PartialFunction[A, Option[A]]): Option[A] = {
      addWithMerge(globalModuleMembers, simpleName, create)(handleExisting)
    }

    private def addSimple[A <: BindingDefinition, B <: A](
      globalModuleMembers: GlobalModuleMembers[A],
      create: () => B,
    ): Option[B] = {
      // We can cast the result to `Option[B]` because `add` will at most return the module member created by `create`
      // if `handleExisting` is empty.
      addWithMerge(globalModuleMembers, create)(PartialFunction.empty).asInstanceOf[Option[B]]
    }

    private def addModule(parentModule: GlobalModule, globalModule: GlobalModule): Unit = {
      addWithMerge(parentModule.terms, globalModule.simpleName, () => globalModule) {
        case existingModule: GlobalModule => Some(existingModule)
        case structBinding: StructBinding =>
          // A struct has already been added and we only need to add the companion module to the struct.
          if (structBinding.companionModule.isEmpty) {
            structBinding.companionModule = Some(globalModule)
          }
          Some(structBinding)
      }
    }

    private def addStructBinding(create: () => StructBinding): Unit = {
      addWithMerge(parentModule.terms, create) {
        case globalModule: GlobalModule =>
          // We're adding a struct term to an existing module member, which means that the module is a companion module
          // and needs to be replaced by the struct term.
          val structBinding = create()
          structBinding.companionModule = Some(globalModule)
          parentModule.terms.add(structBinding)
          Some(structBinding)
      }
    }

    private def simpleName: String = node.simpleName
    private def fullName: NamePath = parentModule.name + simpleName

  }

}
