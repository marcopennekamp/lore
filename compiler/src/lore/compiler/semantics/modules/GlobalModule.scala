package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.definitions.{BindingDefinition, Definition, BindingDefinitionKind, TermDefinition, TypeDefinition}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.NamePath
import lore.compiler.types.DeclaredSchema
import lore.compiler.utils.CollectionExtensions.{IterableExtension, OptionExtension}

/**
  * A global module combines a module's definitions from across all source fragments. It also represents modules as a
  * palpable binding in scopes, so that module accesses can be resolved properly.
  */
class GlobalModule(
  override val name: NamePath,
) extends TermDefinition {
  val types: GlobalModuleMembers[TypeDefinition] = new GlobalModuleMembers(this, ModuleMemberKind.Type)
  val terms: GlobalModuleMembers[TermDefinition] = new GlobalModuleMembers(this, ModuleMemberKind.Term)

  def members[A <: BindingDefinition](moduleMemberKind: ModuleMemberKind[A]): GlobalModuleMembers[A] = {
    ModuleMembers.membersOfKind(types, terms, moduleMemberKind)
  }

  private var _specs: Vector[SpecDefinition] = Vector.empty
  def specs: Vector[SpecDefinition] = _specs
  def addSpec(spec: SpecDefinition): Unit = _specs :+= spec

  private var _positions: Vector[Position] = Vector.empty
  def positions: Vector[Position] = _positions
  def addPosition(position: Position): Unit = _positions :+= position

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

  override def definitionKind: BindingDefinitionKind = BindingDefinitionKind.Module
  override def isInitialized: Boolean = true
  override def position: Position = positions.headOption.getOrElse(Position.unknown)
  override def toString: String = name.toString
}
