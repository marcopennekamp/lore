package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.types.TypeVariable.Assignments
import lore.compiler.types.{StructType, Type, TypeSchema, TypeVariable}

/**
  * A struct binding allows instantiating a specific underlying struct type given a type parameter list potentially
  * different from the struct schema's type parameters. This is used to represent struct types and their associated
  * constructors as binding scope entries.
  */
case class StructBinding(name: String, parameters: Vector[TypeVariable], tpe: StructType) extends TypeSchema with Binding {
  override def representative: StructType = super.representative.asInstanceOf[StructType]
  override def instantiate(arguments: Vector[Type]): Option[StructType] = super.instantiate(arguments).asInstanceOf[Option[StructType]]
  override def instantiate(arguments: Vector[Option[Type]], position: Position)(implicit reporter: Reporter): StructType = {
    super.instantiate(arguments, position).asInstanceOf[StructType]
  }
  override def instantiate(assignments: Assignments): StructType = Type.substitute(tpe, assignments).asInstanceOf[StructType]
  override def toString: String = {
    val parameterString = if (parameters.nonEmpty) s"[${parameters.mkString(", ")}]" else ""
    s"$name$parameterString"
  }
}
