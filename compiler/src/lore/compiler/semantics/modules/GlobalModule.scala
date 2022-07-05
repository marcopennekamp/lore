package lore.compiler.semantics.modules

import lore.compiler.semantics.definitions.{BindingDefinition, Definition, TermDefinition, TypeDefinition}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.{BindingKind, NamePath, PositionsProperty}
import lore.compiler.types.DeclaredSchema
import lore.compiler.utils.CollectionExtensions.{IterableExtension, OptionExtension}

/**
  * A global module combines a module's definitions from across all source fragments. It also represents modules as a
  * palpable binding in scopes, so that module accesses can be resolved properly.
  */
class GlobalModule(
  override val name: NamePath,
) extends TermDefinition with PositionsProperty {
  val types: GlobalModuleMembers[TypeDefinition] = new GlobalModuleMembers(this, ModuleMemberKind.Type)
  val terms: GlobalModuleMembers[TermDefinition] = new GlobalModuleMembers(this, ModuleMemberKind.Term)

  def members[A <: BindingDefinition](moduleMemberKind: ModuleMemberKind[A]): GlobalModuleMembers[A] = {
    ModuleMembers.membersOfKind(types, terms, moduleMemberKind)
  }

  /*
  private var _schemas: Map[String, NamedSchema] = Map.empty
  def schemas: Map[String, NamedSchema] = _schemas
  def addSchema(schema: NamedSchema): Unit = _schemas += schema.name -> schema

  private var _globalVariables: Map[String, GlobalVariableDefinition] = Map.empty
  def globalVariables: Map[String, GlobalVariableDefinition] = _globalVariables
  def addGlobalVariable(global: GlobalVariableDefinition): Unit = _globalVariables += global.name -> global

  private var _multiFunctions: Map[String, MultiFunctionDefinition] = Map.empty
  def multiFunctions: Map[String, MultiFunctionDefinition] = _multiFunctions
  def addMultiFunction(mf: MultiFunctionDefinition): Unit = _multiFunctions += mf.name -> mf
  */

  private var _specs: Vector[SpecDefinition] = Vector.empty
  def specs: Vector[SpecDefinition] = _specs
  def addSpec(spec: SpecDefinition): Unit = _specs :+= spec

  def declaredSchemas: Iterable[DeclaredSchema] = types.all.filterType[DeclaredSchema]

  def definitionsIterator: Iterator[Definition] = {
    types.all.iterator ++
      terms.all.iterator ++
      specs.iterator
  }

  /**
    * Returns the multi-function `memberName`. If the member doesn't exist or isn't a multi-function, `None` is
    * returned.
    */
  def getMultiFunction(memberName: String): Option[MultiFunctionDefinition] = {
    terms.get(memberName).filterType[MultiFunctionDefinition]
  }

  override def bindingKind: BindingKind = BindingKind.Module
  override def isInitialized: Boolean = true
  override def toString: String = name.toString
}
