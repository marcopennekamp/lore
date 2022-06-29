package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback._
import lore.compiler.resolution.TypeDependencies.SchemaResolutionOrder
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.semantics.bindings.{StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.modules._
import lore.compiler.syntax.DeclNode.{ModuleNode, StructNode}
import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.types._

object RegistryResolver {

  /**
    * Builds the registry from all module nodes.
    */
  def resolve(moduleNodes: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter): Registry = {
    implicit val registry: Registry = new Registry
    resolveModules(moduleNodes)
    val typeModuleMembers = resolveTypes()
    resolveTerms()
    resolveSpecs()
    registry.coreDefinitions.assign(CoreDefinitionsResolver.resolve())
    registry.declaredTypeHierarchy.assign(new DeclaredTypeHierarchy(typeModuleMembers.map(_.schema.value)))
    registry
  }

  /**
    * Resolve global modules and local modules. All global modules need to be resolved first, as local modules rely on
    * module members in global modules via local declarations and imports.
    */
  private def resolveModules(moduleNodes: Vector[ModuleNode])(implicit registry: Registry, reporter: Reporter): Unit = {
    resolvePredefinedTypes()
    moduleNodes.foreach(GlobalModuleResolver.resolve)
    moduleNodes.foreach(LocalModuleResolver.resolve)
  }

  /**
    * Adds all predefined types to the root global module.
    */
  private def resolvePredefinedTypes()(implicit registry: Registry): Unit = {
    Type.predefinedTypes.values.foreach {
      tpe => registry.rootModule.types.add(new BuiltinTypeModuleMember(tpe, registry.rootModule))
    }
  }

  /**
    * Each module member (except for global modules) will have an unresolved NamedSchema, GlobalVariableDefinition, and
    * so on. We have to resolve these in the correct order, with types first.
    *
    * We perform two separate passes over type declarations: (1) Resolve types and definitions and (2) resolve struct
    * properties. This has the distinct advantage that we don't need to defer typings of struct properties with
    * laziness.
    *
    * Struct bindings are resolved with struct schemas so that functions like `Scope.getStatic` can properly access
    * companion modules of structs, which happens sometimes during schema resolution if a type is accessed through a
    * companion module.
    *
    * Not all types are necessarily resolved. For example, a type with cyclic inheritance won't be added to the schema
    * resolution order and thus not resolved. See also [[TypeModuleMember.schema]].
    */
  private def resolveTypes()(implicit registry: Registry, reporter: Reporter): Iterable[DeclaredTypeModuleMember] = {
    val typeModuleMembers = collectTypeModuleMembers()
    val schemaResolutionOrder = TypeDependencies.resolve(typeModuleMembers)
    resolveSchemas(schemaResolutionOrder)
    resolveStructProperties(schemaResolutionOrder)
    typeModuleMembers.values
  }

  /**
    * Collects all [[TypeDeclNode]]s from all global modules in the registry. In contrast to other declarations, type
    * declarations have to be collected centrally so that a schema resolution order and later a declared type hierarchy
    * can be built.
    */
  private def collectTypeModuleMembers()(implicit registry: Registry): Map[NamePath, DeclaredTypeModuleMember] = {
    var typeModuleMembers = Map.empty[NamePath, DeclaredTypeModuleMember]
    registry.modules.foreach { globalModule =>
      globalModule.types.all.foreach {
        case moduleMember: DeclaredTypeModuleMember => typeModuleMembers += moduleMember.name -> moduleMember
        case _ =>
      }
    }
    typeModuleMembers
  }

  /**
    * Resolves schemas, definitions, and struct bindings in their schema resolution order.
    */
  private def resolveSchemas(schemaResolutionOrder: SchemaResolutionOrder)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    schemaResolutionOrder.foreach { moduleMember =>
      val schema = moduleMember.declNode match {
        case aliasNode: DeclNode.AliasNode => AliasSchemaResolver.resolve(aliasNode)
        case traitNode: DeclNode.TraitNode => TraitSchemaResolver.resolve(traitNode)
        case structNode: DeclNode.StructNode => StructSchemaResolver.resolve(structNode)
      }
      moduleMember.schema.assign(schema)

      // Resolve struct bindings immediately so that type resolution can access companion modules through them.
      // TODO (multi-import): Duplicate code!
      schema match {
        case _: StructSchema =>
          moduleMember.owner.terms.get(moduleMember.simpleName) match {
            case Some(bindingModuleMember: StructModuleMember) => resolveStructBinding(bindingModuleMember)
            case None =>
          }
        case aliasSchema: AliasSchema if aliasSchema.definition.isStructAlias =>
          moduleMember.owner.terms.get(moduleMember.simpleName) match {
            case Some(bindingModuleMember: StructModuleMember) => resolveStructBinding(bindingModuleMember)
            case None =>
          }
        case _ =>
      }
    }
  }

  /**
    * Resolves the struct binding for `moduleMember`.
    *
    * In case of type aliases representing a struct type, the struct binding will be able to instantiate the struct
    * with the correct struct type given the type alias's type parameters.
    *
    * TODO (multi-import): Maybe split into `resolveStructBinding` and `resolveStructAliasBinding`.
    */
  private def resolveStructBinding(moduleMember: StructModuleMember)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    val binding = moduleMember.typeModuleMember.schema.value match {
      case schema: StructSchema =>
        if (schema.definition.isObject) {
          StructObjectBinding(moduleMember, schema.constantType)
        } else {
          StructConstructorBinding(moduleMember, schema.parameters, schema.instantiate(schema.identityAssignments))
        }

      case schema: AliasSchema if schema.definition.isStructAlias =>
        val underlyingType = schema.originalType.asInstanceOf[StructType]
        if (underlyingType.schema.definition.isObject) {
          StructObjectBinding(moduleMember, underlyingType)
        } else {
          StructConstructorBinding(moduleMember, schema.parameters, underlyingType)
        }

      case schema => throw CompilationException(s"Unexpected schema for struct binding `${moduleMember.name}`:" +
        s" $schema. The schema is neither a struct schema nor a struct alias schema.")
    }
    moduleMember.structBinding.assign(binding)
  }

  private def resolveStructProperties(schemaResolutionOrder: SchemaResolutionOrder)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    schemaResolutionOrder.foreach { moduleMember =>
      moduleMember.schema.value match {
        case schema: StructSchema => StructPropertyResolver.resolve(
          schema,
          // TODO (multi-import): This cast and other difficulties with matching on type module members tell me that
          //                      we need separate module member types for each kind of type, i.e. StructTypeModuleMember,
          //                      TraitTypeModuleMember, and AliasTypeModuleMember.
          moduleMember.declNode.asInstanceOf[StructNode].properties,
        )
        case _ =>
      }
    }
  }

  /**
    * Terms can be resolved in one go as global variables and multi-functions have no declarative interdependency.
    *
    * Struct bindings have already been resolved during type resolution.
    */
  private def resolveTerms()(implicit registry: Registry, reporter: Reporter): Unit = {
    registry.modules.foreach(_.terms.all.foreach(resolveTerm))
  }

  private def resolveTerm(moduleMember: TermModuleMember)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    moduleMember match {
      //case moduleMember: StructModuleMember => resolveStructBinding(moduleMember)
      case moduleMember: GlobalVariableModuleMember => resolveGlobalVariable(moduleMember)
      case moduleMember: MultiFunctionModuleMember => resolveMultiFunction(moduleMember)
      case _ =>
        // Global modules (ModuleModuleMembers) don't need to be resolved further.
    }
  }

  private def resolveGlobalVariable(moduleMember: GlobalVariableModuleMember)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    val definition = GlobalVariableDefinitionResolver.resolve(moduleMember.declNode)
    moduleMember.globalVariable.assign(definition)
  }

  private def resolveMultiFunction(moduleMember: MultiFunctionModuleMember)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    val definition = MultiFunctionDefinitionResolver.resolve(moduleMember.functionNodes)
    moduleMember.multiFunction.assign(definition)
  }

  private def resolveSpecs()(implicit registry: Registry): Unit = {
    registry.modules.foreach { globalModule =>
      globalModule.specs.foreach {
        moduleMember =>
          val spec = SpecDefinitionResolver.resolve(moduleMember.declNode)
          moduleMember.spec.assign(spec)
      }
    }
  }

}
