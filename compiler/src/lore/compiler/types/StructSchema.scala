package lore.compiler.types

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.syntax.DeclNode.StructNode
import lore.compiler.syntax.Node.NameNode
import lore.compiler.utils.Once

class StructSchema(
  override val name: NamePath,
  val isObject: Boolean,
  override val node: StructNode,
) extends DeclaredSchema {
  private val _properties: Once[Vector[StructProperty]] = new Once

  def properties: Vector[StructProperty] = _properties
  override def kind: Kind = Kind.Struct

  /**
    * Initializes the properties of the struct schema. Because properties don't influence the schema initialization
    * order, they have to be resolved in a second phase when all types have already been initialized.
    */
  def initializeProperties(properties: Vector[StructProperty]): Unit = {
    _properties.assign(properties)
  }

  lazy val openParameters: Vector[TypeVariable] = typeParameters.filter(_.isOpen)

  /**
    * The map contains the properties from which each open type parameter must be derived.
    *
    * If the type parameter is contained in none or multiple property types, there will be no entry in this map. The
    * struct constraints will properly report this before any exceptions are raised.
    */
  lazy val openParameterDerivations: Map[TypeVariable, StructProperty] = {
    openParameters.flatMap { typeParameter =>
      properties.filter(property => Type.contains(property.tpe, typeParameter)) match {
        case Vector(property) => Vector((typeParameter, property))
        case _ => Vector.empty
      }
    }.toMap
  }

  lazy val propertyMap: Map[String, StructProperty] = properties.map(p => (p.name, p)).toMap
  lazy val openProperties: Vector[StructProperty] = properties.filter(_.isOpen)
  def hasOpenProperties: Boolean = openProperties.nonEmpty

  /**
    * The constructor signature of the struct <i>without</i> instantiated type parameters.
    */
  lazy val constructorSignature: FunctionSignature = {
    instantiate(typeParameterIdentityAssignments).constructorSignature.copy(typeParameters = typeParameters)
  }

  override def constantType: StructType = super.constantType.asInstanceOf[StructType]
  override def instantiate(assignments: TypeVariable.Assignments): StructType = StructType(this, assignments)

  override def position: Position = node.position
}

object StructSchema {

  /**
    * Creates a mock struct or object schema which can be used as a stand-in for an unresolved struct schema. The
    * schema is guaranteed to be constant.
    */
  def createMock(name: NamePath, isObject: Boolean): StructSchema = {
    val node = StructNode(
      NameNode(name.simpleName, Position.internal),
      isObject,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Position.internal,
    )
    val schema = new StructSchema(name, isObject, node)
    schema.initialize(Vector.empty, Vector.empty)
    schema.initializeProperties(Vector.empty)
    schema
  }

}
