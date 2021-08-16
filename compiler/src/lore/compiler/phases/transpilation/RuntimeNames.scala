package lore.compiler.phases.transpilation

import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.target.Target
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.{DeclaredSchema, DeclaredType, StructSchema, Type}

object RuntimeNames {
  object schema {
    def apply(schema: DeclaredSchema): Target.Variable = s"lore_schema_${schema.name}".asVariable
    def typeParameters(schema: DeclaredSchema): Target.Variable = s"${apply(schema).name}__type_parameters".asVariable
    def representative(schema: DeclaredSchema): Target.Variable = s"${apply(schema).name}__representative".asVariable
  }

  object struct {
    def defaultValue(struct: StructSchema, property: StructPropertyDefinition): Target.Variable = s"${schema(struct).name}__default_${property.name}".asVariable
    def instantiate(struct: StructSchema): Target.Variable = s"${schema(struct).name}__instantiate".asVariable
    def construct(struct: StructSchema): Target.Variable = s"${schema(struct).name}__construct".asVariable

    /**
      * The constructor of a <b>constant</b> struct schema. This does not get generated for parameterized structs. Use
      * [[RuntimeApi.structs.getConstructor]] instead.
      */
    def constructor(struct: StructSchema): Target.Variable = s"${schema(struct).name}__constructor".asVariable
  }

  def newType(tpe: DeclaredType): Target.Variable = s"lore_newtype_${tpe.name}".asVariable

  def temporaryVariable(prefix: String, name: String): Target.Variable = s"${prefix}tmp_$name".asVariable
  def localVariable(loreName: String): Target.Variable = s"lore_lv_$loreName".asVariable

  def symbolType(name: String): Target.Variable = s"lore_symbol_type_$name".asVariable
  def symbolValue(name: String): Target.Variable = s"lore_symbol_value_$name".asVariable

  /**
    * The name of a polymorphic function's type variable assignments. These assignments contain, for each type variable
    * defined in the signature of the polymorphic function, the run-time type assigned to the type variable for the
    * current call.
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

  def functionTypeParameters(function: FunctionDefinition): Target.Variable = s"${functionDefinition(function).name}__type_parameters".asVariable

  /**
    * Run-time multi-function names are unqualified for now. However, this function should be used so that we can
    * change this later more easily.
    */
  def multiFunction(mf: MultiFunctionDefinition): Target.Variable = mf.name.asVariable
}
