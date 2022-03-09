package lore.compiler.types

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}

class StructSchema(
  override val name: NamePath,
  override val parameters: Vector[TypeVariable],
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DeclaredSchema.DefinitionProperty[StructDefinition] {
  override val kind: Kind = Kind.Struct

  val openParameters: Vector[TypeVariable] = parameters.filter(_.isOpen)

  /**
    * The map contains the properties from which each open type parameter must be derived.
    *
    * If the type parameter is contained in none or multiple property types, there will be no entry in this map. The
    * struct constraints will properly report this before any exceptions are raised.
    */
  lazy val openParameterDerivations: Map[TypeVariable, StructPropertyDefinition] = {
    openParameters.flatMap { typeParameter =>
      definition.properties.filter(property => Type.contains(property.tpe, typeParameter)) match {
        case Vector(property) => Vector((typeParameter, property))
        case _ => Vector.empty
      }
    }.toMap
  }

  /**
    * The constructor signature of the struct <i>without</i> instantiated type parameters.
    */
  lazy val constructorSignature: FunctionSignature = {
    instantiate(identityAssignments).constructorSignature.copy(typeParameters = parameters)
  }

  override def constantType: StructType = super.constantType.asInstanceOf[StructType]
  override def instantiate(assignments: TypeVariable.Assignments): StructType = StructType(this, assignments)

  def hasOpenProperties: Boolean = definition.openProperties.nonEmpty
}
