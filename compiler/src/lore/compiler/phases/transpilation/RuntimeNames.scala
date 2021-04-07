package lore.compiler.phases.transpilation

import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.target.Target
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.{DeclaredType, StructType, Type}


object RuntimeNames {
  def declaredType(tpe: DeclaredType): Target.Variable = s"lore_type_${tpe.name}".asVariable
  def typeSchema(tpe: DeclaredType): Target.Variable = s"lore_schema_${tpe.name}".asVariable
  def newType(tpe: DeclaredType): Target.Variable = s"lore_newtype_${tpe.name}".asVariable
  def instantiate(struct: StructType): Target.Variable = s"${declaredType(struct).name}__instantiate".asVariable
  def constructor(struct: StructType): Target.Variable = s"${declaredType(struct).name}__constructor".asVariable
  def defaultValue(struct: StructType, property: StructPropertyDefinition): Target.Variable = s"${declaredType(struct).name}__default_${property.name}".asVariable

  def temporaryVariable(name: String): Target.Variable = s"lore_tmp_$name".asVariable
  def localVariable(loreName: String): Target.Variable = s"lore_lv_$loreName".asVariable

  /**
    * The name of a polymorphic function's type variable assignments map. These assignments contain, for each type
    * variable defined in the signature of the polymorphic function, the run-time type assigned to the type variable
    * for the current call.
    */
  def localTypeVariableAssignments: Target.Variable = "lore_type_assignments".asVariable

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
  def functionDefinition(function: FunctionDefinition): Target.Variable = {
    val id = Type.uniqueIdentifier(function.signature.inputType.elements)
    s"${function.name}$$$id".asVariable
  }

  /**
    * Run-time multi-function names are unqualified for now. However, this function should be used so that we can
    * change this later more easily.
    */
  def multiFunction(mf: MultiFunctionDefinition): Target.Variable = mf.name.asVariable
}
