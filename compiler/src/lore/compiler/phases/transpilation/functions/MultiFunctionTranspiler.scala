package lore.compiler.phases.transpilation.functions

import lore.compiler.CompilerOptions
import lore.compiler.phases.transpilation.TypeTranspiler.TranspiledTypeVariables
import lore.compiler.phases.transpilation.{RuntimeApi, TypeTranspiler, TemporaryVariableProvider}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types._

class MultiFunctionTranspiler(mf: MultiFunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry) {

  private val properties = MultiFunctionProperties(mf)

  private implicit val variableProvider: TemporaryVariableProvider = new TemporaryVariableProvider(s"${mf.name}__")

  def transpile(): Vector[TargetStatement] = {
    if (properties.isSingleFunction && mf.functions.forall(_.isMonomorphic)) {
      return transpileSingleFunction()
    }

    // Phase 1: Transpile type variables.
    val (typeVariableStatements, typeVariables) = transpiledInputTypeVariables
    implicit val implicitTypeVariables: TranspiledTypeVariables = typeVariables

    // Phase 2: Transpile functions.
    val functionStatements = mf.functions.filterNot(_.isAbstract).flatMap(FunctionTranspiler.transpile)

    // Phase 3: Transpile input types.
    val dispatchInput = new DispatchInput(mf, properties)

    // Phase 4: Transpile multi-function with dispatch logic.
    val dispatchBehavior = new DispatchBehavior(mf, properties, dispatchInput)

    val loggingStatements = if (compilerOptions.runtimeLogging) {
      Vector(RuntimeApi.io.println(s"Called multi-function ${mf.name}.".asLiteral))
    } else Vector.empty

    val body = Target.Block(loggingStatements ++ dispatchInput.gatherArgumentTypes() ++ dispatchBehavior.transpileDispatchCall())
    val multiFunctionDeclaration = Target.Function(
      mf.name.asName,
      dispatchInput.parameters,
      body,
      shouldExport = true
    )

    typeVariableStatements ++ functionStatements ++ dispatchInput.preamble ++ dispatchBehavior.preamble ++ Vector(multiFunctionDeclaration)
  }

  /**
    * If the multi-function consists of a single function that is not polymorphic, we can bypass all dispatch logic.
    * This requires that all function calls are legal at compile-time, but this is already guaranteed by the compiler.
    *
    * TODO: There are ways to treat polymorphic functions in this way too. We just have to assure that a call that
    *       is valid at compile-time doesn't become invalid at run-time. This means that none of the variables may
    *       have a lower bound, as that might exclude a subtype at run-time only. In addition, type variables may
    *       not occur twice or more times in the input type so that missing type equality cannot rule out the validity
    *       of the call.
    *       Polymorphic functions may still require assigning an argument type to a type variable, but this could then
    *       be done ad-hoc in the generated single function.
    *
    * TODO: This approach lacks correctness when Lore functions are called from Javascript. To improve interfacing
    *       with native Javascript, we should transpile a second "external" function that actually does the type
    *       checking. This would be the function that we export.
    */
  private def transpileSingleFunction(): Vector[TargetStatement] = {
    implicit val typeVariables: TranspiledTypeVariables = Map.empty
    FunctionTranspiler.transpile(mf.functions.head, mf.name.asName, shouldExport = true)
  }

  /**
    * All type variables are transpiled in bulk before functions are defined because a function also might need to have
    * access to its own type variables.
    */
  private lazy val transpiledInputTypeVariables: (Vector[TargetStatement], TranspiledTypeVariables) = {
    def handleFunction(function: FunctionDefinition) = {
      if (function.isPolymorphic) {
        TypeTranspiler.transpileTypeVariables(Type.variables(function.signature.inputType).toVector)
      } else (Vector.empty, Map.empty: TranspiledTypeVariables)
    }
    mf.functions.map(handleFunction).foldLeft((Vector.empty[TargetStatement], Map.empty: TranspiledTypeVariables)) {
      case ((d1, v1), (d2, v2)) => (d1 ++ d2, v1 ++ v2)
    }
  }

}
