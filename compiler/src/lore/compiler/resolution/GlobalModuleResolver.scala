package lore.compiler.resolution

import lore.compiler.feedback.{ModuleFeedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules._
import lore.compiler.syntax.DeclNode._
import lore.compiler.syntax.{DeclNode, TypeDeclNode}

import scala.reflect.ClassTag

object GlobalModuleResolver {

  /**
    * Adds all global modules and their module members found in the given module node to the registry. Duplicate
    * declarations are reported as errors and not added to global modules.
    */
  def resolve(moduleNode: ModuleNode)(implicit registry: Registry, reporter: Reporter): Unit = {
    addNode(registry.rootModule, moduleNode)
  }

  /**
    * Adds `node` as a [[BindingModuleMember]] to the given global module, while also handling `node`'s members if it
    * represents a module. If `node` is an at-root module declaration, it will be added to the root module instead.
    *
    * If `node`'s simple name has already been added to the module, and the new binding is incompatible with the
    * existing binding, a [[ModuleFeedback.MemberNameTaken]] is reported and the member isn't added to the module.
    */
  private def addNode(globalModule: GlobalModule, node: DeclNode)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = node match {
    case node: ModuleNode =>
      // A nested module node `module Foo.Bar.Baz` has to be added to the global index such that `Foo`, `Bar`, and
      // `Baz` are added as separate modules. `Foo` and `Bar` might not actually be defined anywhere else, so it's
      // important to have these global modules created in the index. The inner module receives all member names,
      // while the outer modules simply have the nested module as their sole member. Given `module Foo.Bar.Baz`,
      // `Foo` becomes a member of the root module, `Bar` a member of `Foo`, `Baz` a member of `Bar`, and all members
      // are registered to `Baz`.
      val parentModule = node match {
        case ModuleNode(_, true, _, _, _) => registry.rootModule
        case _ => globalModule
      }
      val innerModule = node.namePath.segments.foldLeft(parentModule) {
        case (parentModule, simpleModuleName) =>
          // Do NOT use `node.simpleName`, because it refers to the head segment of a composite module name. Instead,
          // the name path's segments must be folded as is done here.
          val globalModule = registry.getOrCreateModule(parentModule.name + simpleModuleName)
          globalModule.addPosition(node.position)
          addModuleMember(parentModule, new ModuleModuleMember(globalModule, parentModule))
          globalModule
      }
      node.members.foreach(addNode(innerModule, _))

    case node: StructNode => addStructNode(globalModule, node)
    case node: AliasNode if node.isStructAlias => addStructNode(globalModule, node)
    case node: TypeDeclNode => addModuleMember(globalModule, new DeclaredTypeModuleMember(node, globalModule))

    case node: GlobalVariableNode => addModuleMember(globalModule, new GlobalVariableModuleMember(node, globalModule))
    case node: FunctionNode => addModuleMember(globalModule, new MultiFunctionModuleMember(node, globalModule))

    case node: SpecNode =>
      // Specs do not need to be added to the global module, because they cannot be referenced from Lore code.
      // Still, spec declarations need to be collected.
      globalModule.specs :+= new SpecModuleMember(node, globalModule)
  }

  private def addStructNode(globalModule: GlobalModule, node: TypeDeclNode)(implicit reporter: Reporter): Unit = {
    val typeModuleMember = new DeclaredTypeModuleMember(node, globalModule)
    addModuleMember(globalModule, typeModuleMember).foreach { _ =>
      addModuleMember(globalModule, new StructModuleMember(typeModuleMember, globalModule))
    }
  }

  /**
    * Adds `moduleMember` to `globalModule`. A duplicate declaration is reported as an error and not added to the
    * global module. A multi-definable `moduleMember` may be merged with an already existing module member if their
    * binding kinds agree. Another edge case is concerned with structs and companion modules: Adding a struct type
    * overrides an already existing module with the same name. Vice versa, a module is ignored if a corresponding
    * struct type already exists.
    *
    * @return The added module member if the node is not a duplicate, and `None` otherwise. The returned member is not
    *         always the same as `moduleMember` if `moduleMember` is merged into an already existing member.
    */
  private def addModuleMember[A <: BindingModuleMember](
    globalModule: GlobalModule,
    moduleMember: A,
  )(implicit memberTag: ClassTag[A], reporter: Reporter): Option[BindingModuleMember] = {
    val globalModuleMembers = globalModule.members[A]
    val simpleName = moduleMember.name.simpleName

    globalModuleMembers.get(simpleName) match {
      case Some(existingMember) =>
        (moduleMember, existingMember) match {
          case (_: ModuleModuleMember, existingMember: ModuleModuleMember) =>
            // A module member representing a module is just a delegate for a global module. There is no need to merge
            // in `moduleMember`.
            Some(existingMember)

          case (structModuleMember: StructModuleMember, existingMember: ModuleModuleMember) =>
            // We're adding a struct to an existing module member, which means that the module is a companion module
            // and needs to be replaced by the struct module member.
            globalModuleMembers.add(moduleMember)
            structModuleMember.companionModule = Some(existingMember.globalModule)
            Some(moduleMember)

          case (moduleMember: ModuleModuleMember, existingMember: StructModuleMember) =>
            // A struct has already been added and we don't need to add the companion module.
            if (existingMember.companionModule.isEmpty) {
              existingMember.companionModule = Some(moduleMember.globalModule)
            }
            Some(existingMember)

          case (moduleMember: MultiFunctionModuleMember, existingMember: MultiFunctionModuleMember) =>
            moduleMember.functionNodes.foreach(existingMember.addFunctionNode)
            Some(existingMember)

          case _ =>
            reporter.error(ModuleFeedback.MemberNameTaken(simpleName, existingMember, moduleMember.position))
            None
        }

      case None =>
        globalModuleMembers.add(moduleMember)
        Some(moduleMember)
    }
  }

}
