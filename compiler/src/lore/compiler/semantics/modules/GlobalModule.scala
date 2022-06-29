package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.bindings.TermBinding
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.{Definition, NamePath}
import lore.compiler.types.{DeclaredSchema, NamedSchema}
import lore.compiler.utils.CollectionExtensions.{IterableExtension, OptionExtension}

import scala.reflect.ClassTag

/**
  * A global module combines a module's definitions from across all source fragments. It also represents modules as a
  * palpable binding in scopes, so that module accesses can be resolved properly.
  */
class GlobalModule(val name: NamePath) extends TermBinding {
  val types: GlobalModuleMembers[TypeModuleMember] = new GlobalModuleMembers(this)
  val terms: GlobalModuleMembers[TermModuleMember] = new GlobalModuleMembers(this)

  private var _specs: Vector[SpecModuleMember] = Vector.empty
  private var _positions: Vector[Position] = Vector.empty

  def members[A <: BindingModuleMember](implicit memberTag: ClassTag[A]): GlobalModuleMembers[A] = {
    ModuleMembers.membersOfType(types, terms)
  }

  def addSpec(moduleMember: SpecModuleMember): Unit = _specs :+= moduleMember
  def specs: Vector[SpecModuleMember] = _specs

  def addPosition(position: Position): Unit = _positions :+= position
  def positions: Vector[Position] = _positions

  def schemas: Iterable[NamedSchema] = types.all.map(_.schema.value)
  def declaredSchemas: Iterable[DeclaredSchema] = schemas.filterType[DeclaredSchema]

  def bindings: Iterable[TermBinding] = terms.all.map(_.binding)
  def bindingDefinitions: Iterable[Definition] = bindings.filterType[Definition]

  def definitionsIterator: Iterator[Definition] = {
    declaredSchemas.iterator.map(_.definition) ++
      bindingDefinitions.iterator ++
      specs.iterator.map(_.spec.value)
  }

  /**
    * Returns the schema of the member `memberName`. If the member doesn't exist or if the schema hasn't been
    * initialized, `None` is returned.
    */
  def getSchema(memberName: String): Option[NamedSchema] = types.get(memberName).flatMap(_.schema.toOption)

  /**
    * Returns the multi-function definition of the member `memberName`. If the member doesn't exist or isn't a
    * multi-function, `None` is returned.
    */
  def getMultiFunction(memberName: String): Option[MultiFunctionDefinition] = {
    terms.get(memberName).filterType[MultiFunctionModuleMember].map(_.multiFunction.value)
  }

  override def toString: String = name.toString
}
