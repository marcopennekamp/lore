package lore.compiler.phases.transpilation.functions

import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.TypeTranspiler.TranspiledTypeVariables
import lore.compiler.phases.transpilation.{RuntimeApi, TemporaryVariableProvider, TypeTranspiler}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl._
import lore.compiler.types.Type

/**
  * The dispatch input transpiler covers input and argument types. The [[DispatchBehavior]] relies on the types
  * provided by this transpiler.
  */
class DispatchInput(mf: MultiFunctionDefinition, properties: MultiFunctionProperties)(implicit variableProvider: TemporaryVariableProvider, typeVariables: TranspiledTypeVariables) {

  val varArgumentType: Target.Variable = "argumentType".asVariable

  val (preamble, inputTypes): (Vector[TargetStatement], Map[Type, Target.Variable]) = initialize()

  /**
    * All possible input types are declared as global constants so that the target VM doesn't have to recreate these
    * types every time the multi-function is called.
    */
  private def initialize() = {
    val inputTypes = mf.functions.map(_.signature.inputType).toSet
    var variables = Map.empty[Type, Target.Variable]
    val statements = inputTypes.toVector.map { inputType =>
      val simplifiedInputType = if (properties.mayUnpackArgumentTuple) {
        if (inputType.elements.size != 1) {
          throw CompilationException(s"An unpacked argument tuple requires functions of arity 1. Affected multi-function: ${mf.name}.")
        }
        inputType.elements.head
      } else inputType

      val varType = variableProvider.createVariable()
      val typeExpr = TypeTranspiler.transpile(simplifiedInputType)
      variables = variables + (inputType -> varType)
      varType.declareAs(typeExpr)
    }
    (statements, variables)
  }

  /**
    * The parameters coming into the multi-function. If the multi-function contains functions with differing arities,
    * we must default to rest parameters.
    */
  lazy val parameters: Vector[Target.Parameter] = {
    properties.uniqueArity match {
      case None => Vector(Target.Parameter("args".asName, isRestParameter = true))
      case Some(arity) => (0 until arity).toVector.map(index => s"arg$index".asParameter)
    }
  }

  lazy val requiresRestParameters: Boolean = properties.uniqueArity.isEmpty

  /**
    * Transpiles the code that gathers the argument types into a tuple type.
    */
  def gatherArgumentTypes(): Vector[TargetStatement] = {
    // If we can unpack the argument tuple, the generated code is very simple.
    if (properties.mayUnpackArgumentTuple) {
      return Vector(varArgumentType.declareAs(RuntimeApi.types.typeOf(parameters.head.asVariable)))
    }

    // If we don't use the cache, there is no reason to hash a transient tuple type.
    def constructTupleType(types: TargetExpression) = {
      if (properties.shouldUseDispatchCache) RuntimeApi.tuples.tpe(types)
      else RuntimeApi.tuples.unhashedType(types)
    }

    // If we are forced to use rest parameters, we will have to gather the argument type with a loop.
    if (requiresRestParameters) {
      val varArgumentTypes = "argumentTypes".asVariable
      val varArgs = parameters.head.asVariable
      val argsLength = varArgs.prop("length")
      Vector(
        varArgumentTypes.declareAs("Array".asVariable.`new`(argsLength)),
        varArgs.iterateWithIndex(argsLength) {
          (varArg, varIndex) => varArgumentTypes.element(varIndex).assign(RuntimeApi.types.typeOf(varArg))
        },
        varArgumentType.declareAs(constructTupleType(varArgumentTypes))
      )
    } else {
      // Otherwise, we know the number of arguments exactly.
      val typeOfCalls = parameters.map(parameter => RuntimeApi.types.typeOf(parameter.asVariable))
      Vector(
        varArgumentType.declareAs(constructTupleType(Target.List(typeOfCalls)))
      )
    }
  }

}
