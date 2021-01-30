package lore.compiler.phases.transpilation

import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.target.Target.TargetName
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.{DeclaredType, StructType, Type}

// TODO: Return Target.Variable instead of TargetName.

object RuntimeNames {
  def declaredType(tpe: DeclaredType): TargetName = s"lore_type_${tpe.name}".asName
  def typeSchema(tpe: DeclaredType): TargetName = s"lore_schema_${tpe.name}".asName
  def newType(tpe: DeclaredType): TargetName = s"lore_newtype_${tpe.name}".asName
  def instantiate(struct: StructType): TargetName = s"lore_instantiate_${struct.name}".asName
  def defaultValue(struct: StructType, property: StructPropertyDefinition): TargetName = s"${declaredType(struct)}__default_${property.name}".asName

  def temporaryVariable(name: String): TargetName = s"lore_tmp_$name".asName
  def localVariable(loreName: String): TargetName = s"lore_lv_$loreName".asName

  /**
    * The name of a polymorphic function's type variable assignments map. These assignments contain, for each type
    * variable defined in the signature of the polymorphic function, the run-time type assigned to the type variable
    * for the current call.
    */
  def localTypeVariableAssignments: TargetName = "lore_type_assignments".asName

  /**
    * Returns a unique name for a given function definition. Because multi-functions contain many functions of the
    * same name, we have to incorporate the input type into the individual function's name. This is similar to C++'s
    * name mangling. It is also preferable to just giving the function an index or a UUID, because apart from removing
    * the potential for collisions, we can also reconstruct the function's name, which will be especially useful
    * between compilation passes.
    *
    * The input type's outer tuple is omitted from the identifier, as it solely represents redundant information.
    * Unit functions thus are represented by the simple name function$.
    */
  def function(function: FunctionDefinition): TargetName = {
    val id = Type.uniqueIdentifier(function.signature.inputType.elements)
    s"${function.name}$$$id".asName
  }
}
