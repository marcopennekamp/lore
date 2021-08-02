package lore.compiler.phases.transpilation.functions

import lore.compiler.core.CompilerOptions
import lore.compiler.phases.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.phases.transpilation.{RuntimeApi, TemporaryVariableProvider, TypeTranspiler}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types._

class MultiFunctionTranspiler(mf: MultiFunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry, symbolHistory: SymbolHistory) {

  private val properties = MultiFunctionProperties(mf)

  private implicit val variableProvider: TemporaryVariableProvider = new TemporaryVariableProvider(s"${mf.targetVariable.name}__")

  def transpile(): Vector[TargetStatement] = {
    if (properties.isSingleFunction) {
      val function = mf.functions.head
      if (function.isMonomorphic && !function.isAbstract) {
        return transpileSingleFunction(function)
      }
    }

    // Phase 1: Transpile type variables.
    val (typeVariableStatements, typeVariables) = transpiledInputTypeParameters
    implicit val runtimeTypeVariables: RuntimeTypeVariables = typeVariables

    // Phase 2: Transpile functions.
    val functionStatements = mf.functions.filterNot(_.isAbstract).flatMap(FunctionTranspiler.transpile)

    // Phase 3: Transpile input types.
    val dispatchInput = new DispatchInput(mf, properties)

    // Phase 4: Transpile multi-function with dispatch logic.
    val dispatchBehavior = new DispatchBehavior(mf, properties, dispatchInput)

    val loggingStatements = if (compilerOptions.enableRuntimeLogging) {
      Vector(RuntimeApi.io.println(s"Called multi-function ${mf.name}.".asLiteral))
    } else Vector.empty

    val body = Target.Block(loggingStatements ++ dispatchInput.gatherArgumentTypes() ++ dispatchBehavior.transpileDispatchCall())
    val multiFunctionDeclaration = Target.Function(
      mf.targetVariable.name,
      dispatchInput.parameters,
      body,
      shouldExport = true
    )

    typeVariableStatements ++ functionStatements ++ dispatchInput.preamble ++ dispatchBehavior.preamble ++ Vector(multiFunctionDeclaration)
  }

  /**
    * If the multi-function consists of a single function that is not polymorphic, we can bypass all dispatch logic.
    * This requires that all function calls are legal at compile-time, but this is already guaranteed by the compiler.
    */
  private def transpileSingleFunction(function: FunctionDefinition): Vector[TargetStatement] = {
    implicit val runtimeTypeVariables: RuntimeTypeVariables = Map.empty
    FunctionTranspiler.transpile(function, mf.targetVariable.name, shouldExport = true)
  }

  /**
    * All type parameters are transpiled in bulk before functions are defined because a function also might need to
    * have access to its own type parameters.
    */
  private lazy val transpiledInputTypeParameters: (Vector[TargetStatement], RuntimeTypeVariables) = {
    def handleFunction(function: FunctionDefinition) = {
      if (function.isPolymorphic) {
        TypeTranspiler.transpileTypeVariables(function.typeParameters)
      } else (Vector.empty, Map.empty: RuntimeTypeVariables)
    }
    mf.functions.map(handleFunction).foldLeft((Vector.empty[TargetStatement], Map.empty: RuntimeTypeVariables)) {
      case ((d1, v1), (d2, v2)) => (d1 ++ d2, v1 ++ v2)
    }
  }

}
