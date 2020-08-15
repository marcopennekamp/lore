package lore.compiler.phases.transpilation

import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.{NamedType, Type}

object TranspiledNames {
  def namedType(tpe: NamedType): String = s"lore_type_${tpe.name}"
  def classDefinition(definition: ClassDefinition): String = definition.name
  def temporaryVariable(name: String): String = s"lore_tmp_$name"
  def localVariable(loreName: String): String = s"lore_lv_$loreName"

  /**
    * The name of a polymorphic function's type variable assignments map. These assignments contain, for each type
    * variable defined in the signature of the polymorphic function, the run-time type assigned to the type variable
    * for the current call.
    */
  def localTypeVariableAssignments: String = "lore_type_assignments"

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
  def function(function: FunctionDefinition): String = {
    val id = Type.uniqueIdentifier(function.signature.inputType.components)
    s"${function.name}$$$id"
  }
}
