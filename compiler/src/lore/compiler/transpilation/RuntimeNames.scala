package lore.compiler.transpilation

import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.target.Target
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.{DeclaredSchema, DeclaredType, StructSchema}

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

    /**
      * The instance of an `object` struct. This only gets transpiled when the struct is actually an object.
      */
    def `object`(struct: StructSchema): Target.Variable = s"${schema(struct).name}__object".asVariable
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

  def functionDefinition(function: FunctionDefinition): Target.Variable = function.runtimeName.asVariable
  def functionTypeParameters(function: FunctionDefinition): Target.Variable = s"${function.runtimeName}__type_parameters".asVariable

  /**
    * Run-time multi-function names are unqualified for now. However, this function should be used so that we can
    * change this later more easily.
    */
  def multiFunction(mf: MultiFunctionDefinition): Target.Variable = mf.name.asVariable
}
