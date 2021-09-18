package lore.compiler.transpilation

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.target.Target
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.{DeclaredSchema, DeclaredType, StructSchema}

object RuntimeNames {
  def namePath(namePath: NamePath): Target.Variable = namePath.segments.mkString("$").asVariable

  object schema {
    def apply(schema: DeclaredSchema): Target.Variable = s"lore_schema_${namePath(schema.name)}".asVariable
    def typeParameters(schema: DeclaredSchema): Target.Variable = s"${apply(schema)}__type_parameters".asVariable
    def representative(schema: DeclaredSchema): Target.Variable = s"${apply(schema)}__representative".asVariable
  }

  object struct {
    def defaultValue(struct: StructSchema, property: StructPropertyDefinition): Target.Variable = s"${schema(struct)}__default_${property.name}".asVariable
    def instantiate(struct: StructSchema): Target.Variable = s"${schema(struct)}__instantiate".asVariable
    def construct(struct: StructSchema): Target.Variable = s"${schema(struct)}__construct".asVariable

    /**
      * The constructor of a <b>constant</b> struct schema. This does not get generated for parameterized structs. Use
      * [[RuntimeApi.structs.getConstructor]] instead.
      */
    def constructor(struct: StructSchema): Target.Variable = s"${schema(struct)}__constructor".asVariable

    /**
      * The instance of an `object` struct. This only gets transpiled when the struct is actually an object.
      */
    def `object`(struct: StructSchema): Target.Variable = s"${schema(struct)}__object".asVariable
  }

  object globalVariable {
    def apply(variable: GlobalVariableDefinition): Target.Variable = namePath(variable.name)
    def getValue(variable: GlobalVariableDefinition): Target.Variable = s"${globalVariable(variable)}__get_value".asVariable
  }

  def newType(tpe: DeclaredType): Target.Variable = s"lore_newtype_${namePath(tpe.name)}".asVariable

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

  def multiFunction(name: NamePath): Target.Variable = namePath(name)
  def multiFunction(mf: MultiFunctionDefinition): Target.Variable = multiFunction(mf.name)
}
