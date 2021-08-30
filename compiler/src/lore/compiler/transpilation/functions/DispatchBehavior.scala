package lore.compiler.transpilation.functions

import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.transpilation.{RuntimeApi, RuntimeNames, TargetRepresentableTranspiler, TemporaryVariableProvider}

class DispatchBehavior(
  mf: MultiFunctionDefinition,
  properties: MultiFunctionProperties,
  dispatchInput: DispatchInput,
)(
  implicit variableProvider: TemporaryVariableProvider,
  runtimeTypeVariables: RuntimeTypeVariables,
  symbolHistory: SymbolHistory,
) {

  private val varDispatchCache = s"${RuntimeNames.multiFunction(mf).name}__dispatchCache".asVariable

  lazy val preamble: Vector[TargetStatement] = {
    // The cache is declared as a global constant so that it exists between multi-function calls.
    if (properties.shouldUseDispatchCache) {
      Vector(varDispatchCache.declareAs(RuntimeApi.utils.typeMap.create()))
    } else Vector.empty
  }

  private val varTarget = "target".asVariable
  private val varCachedTarget = "cachedTarget".asVariable

  /**
    * This function turns the dispatch hierarchy of the multi-function into the target representation, following the
    * steps of this algorithm:
    *
    *   1. First, using a "hierarchy" of nested if statements derived from the dispatch graph, find a candidate
    *      function that could be called with the given input type. In addition, throw an error for any ABSTRACT
    *      functions that are selected.
    *   2. Ensure that all functions in this list are unique. This is important because the dispatch hierarchy
    *      is not a tree and so two different paths in the graph may lead to the same node, which effectively
    *      means that such a function would be duplicated in the result list.
    *   3. If there is more than one unique function, we have an ambiguity that couldn't be caught at compile-time.
    *      In this case, throw an ambiguity error. If there are no functions at all, we have an empty fit (against
    *      our best expectations) and also need to report an error. If there is exactly one function, we choose
    *      this function to call.
    *
    * To generate this algorithm for a given multi-function, we just have to follow the edges of the dispatch
    * graph and make sure that every edge is in its essence an if statement. We also have to implement the way
    * a function is chosen: looking at the min function, we can choose a function if that function's input is a
    * subtype of the input type and none of its direct successors' inputs are subtypes of the input type.
    *
    * Performing such a calculation every time a multi-function is called is a costly endeavour. Luckily, we
    * can rely on the invariant that the target function will always stay the same for the same input type.
    * Hence, we can cache the result of this dispatch algorithm at runtime for real values.
    */
  def transpileDispatchCall(): Vector[TargetStatement] = {
    def transpileTargetCall(target: TargetExpression): TargetStatement = {
      Target.Return(
        Target.Call(
          target,
          dispatchInput.parameters.map(_.asVariable),
          isRestCall = dispatchInput.requiresRestParameters
        )
      )
    }

    val cacheTry = if (properties.shouldUseDispatchCache) {
      Vector(
        varCachedTarget.declareAs(varDispatchCache.prop("get").call(dispatchInput.varArgumentType)),
        Target.IfElse(TargetOperator.Truthy(varCachedTarget), transpileTargetCall(varCachedTarget), Target.Empty)
      )
    } else Vector.empty

    val targetDeclaration = varTarget.declareMutableAs(Target.Undefined)
    val indexedRoots = withFitsVariables(mf.hierarchy.roots)
    val dispatchStatements = indexedRoots.map(transpileFitsCall) ++ indexedRoots.map(transpileDispatchNode)

    cacheTry ++ Vector(targetDeclaration) ++ dispatchStatements ++ Vector(
      Target.IfElse(
        TargetOperator.Not(TargetOperator.Truthy(varTarget)),
        Target.block(
          RuntimeApi.utils.error.emptyFit(mf.name, dispatchInput.varArgumentType)
        ),
        Target.Empty
      ),
      if (properties.shouldUseDispatchCache) {
        varDispatchCache.prop("set").call(dispatchInput.varArgumentType, varTarget)
      } else Target.Empty,
      transpileTargetCall(varTarget)
    ).filterNot(_ == Target.Empty)
  }

  private def withFitsVariables(nodes: Vector[mf.hierarchy.graph.NodeT]): Vector[(mf.hierarchy.graph.NodeT, Target.Variable)] = {
    nodes.zipWithIndex.map { case (node, index) => (node, s"fits$index".asVariable) }
  }

  /**
    * Transpiles a singular fits decision and keeps it in a constant identified by the given `fitsX` variable.
    */
  private def transpileFitsCall(function: FunctionDefinition, varFitsX: Target.Variable): TargetStatement = {
    val varRightType = dispatchInput.inputTypes(function.signature.inputType)

    // We can decide at compile-time which version of the fit should be used, because the type on the right side is
    // constant. If the parameter type isn't polymorphic now, it won't ever be, so we can skip all that run-time
    // testing.
    val fitsCall = if (function.isPolymorphic) {
      RuntimeApi.types.fitsPolymorphic(dispatchInput.varArgumentType, varRightType, RuntimeNames.functionTypeParameters(function))
    } else {
      RuntimeApi.types.fitsMonomorphic(dispatchInput.varArgumentType, varRightType)
    }

    // If we invoke the fitsPolymorphic function and the type fits, assignments are returned rather than a boolean.
    varFitsX.declareAs(fitsCall)
  }

  private def transpileFitsCall(value: (mf.hierarchy.graph.NodeT, Target.Variable)): TargetStatement = transpileFitsCall(value._1, value._2)

  /**
    * For the given node/function, looks at varFitsX to determine whether this function could be called with the given
    * argument. If that's the case, the dispatch algorithm has to look at the successors of the node to ultimately
    * find out the dispatch target.
    */
  private def transpileDispatchNode(node: mf.hierarchy.graph.NodeT, varFitsX: Target.Variable): TargetStatement = {
    val successors = withFitsVariables(node.diSuccessors.toVector)
    val function = node.value

    // Sets the function represented by this node as the target of the multi-function call.
    val setAsTarget = if (!function.isAbstract) {
      val actualFunction = TargetRepresentableTranspiler.transpile(function)
      val candidate = if (function.isPolymorphic) {
        // The first parameter of a polymorphic function is the map of type variable assignments passed to it at
        // run-time. Hence, we have to bind that map (saved in the fitsX variable after being returned by
        // fitsPolymorphic) to the candidate function. This turns the n-ary function into an (n-1)-ary function
        // and also plays nice with the dispatch cache. WIN-WIN!
        actualFunction.prop("bind").call(Target.Null, varFitsX) // Null refers to the function's value of 'this'.
      } else actualFunction

      val varCandidate = "candidate".asVariable
      Target.block(
        varCandidate.declareAs(candidate),
        Target.IfElse(
          TargetOperator.And(TargetOperator.Truthy(varTarget), TargetOperator.NotEquals(varTarget, varCandidate)),
          Target.block(
            RuntimeApi.utils.error.ambiguousCall(mf.name, dispatchInput.varArgumentType)
          ),
          Target.Empty
        ),
        varTarget.assign(varCandidate),
      )
    } else {
      Target.block(
        RuntimeApi.utils.error.missingImplementation(mf.name, function.signature.inputType.toString.asLiteral, dispatchInput.varArgumentType)
      )
    }

    Target.IfElse(
      TargetOperator.Truthy(varFitsX),
      if (successors.nonEmpty) {
        val fitsDeclarations = successors.map(transpileFitsCall)
        val anyFits = TargetOperator.Or(successors.map(_._2): _*)

        Target.Block(
          fitsDeclarations :+ Target.IfElse(
            TargetOperator.Not(anyFits),
            setAsTarget,
            // We don't need to test for successors when we've already determined that none of the successors
            // fit. Hence the else.
            Target.Block(successors.map(transpileDispatchNode))
          )
        )
      } else setAsTarget,
      Target.Empty
    )
  }

  private def transpileDispatchNode(value: (mf.hierarchy.graph.NodeT, Target.Variable)): TargetStatement = transpileDispatchNode(value._1, value._2)

}
