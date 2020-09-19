package lore.compiler.phases.transpilation

import java.io.{ByteArrayOutputStream, PrintStream}

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation
import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.TranspiledChunk.JsCode
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{ProductType, Type}

import scala.collection.mutable
import scala.util.Using

class MultiFunctionTranspiler(mf: MultiFunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry) {

  private val varArgumentType = "argumentType"
  private val varDispatchCache = s"${mf.name}__dispatchCache"

  // TODO: This could be passed around as an immutable map instead of being "global" state.
  private val inputTypeJsNames = mutable.HashMap[Type, String]()

  private implicit val nameProvider: TemporaryNameProvider = new TemporaryNameProvider(s"${mf.name}__")

  def transpile(): Compilation[String] = {
    val out = new ByteArrayOutputStream()
    val tryCompilation = Using(new PrintStream(out, true, "utf-8")) { printer =>
      mf.functions.filterNot(_.isAbstract).map { function =>
        FunctionTranspiler.transpile(function, nameProvider)
      }.simultaneous.map { results =>
        results.reduce[(JsCode, RuntimeTypeVariables)] { case ((code1, variables1), (code2, variables2)) =>
          // We can just concatenate the variable maps even if multiple functions have a variable of the same name
          // because type variables are uniquely identified by their object reference and not by their name.
          (code1 + "\n" + code2, variables1 ++ variables2)
        }
      }.map { case (functionCode, runtimeTypeVariables) =>
        printer.print(functionCode)
        prepareFunctionInputTypes(printer)(runtimeTypeVariables)
        prepareDispatchCache(printer)
        val mfName = s"${mf.name}"
        printer.println(s"export function $mfName($jsParameters) {")
        if (compilerOptions.runtimeLogging) {
          printer.println(s"console.info('Called multi-function $mfName.');")
        }

        transpileArgumentTypeGathering(printer)
        transpileDispatchCall(printer)
        printer.println("}")
      }
    }

    tryCompilation.map(c => c.map(_ => out.toString("utf-8"))).get
  }

  /**
    * Whether this multi-function consists of a single function.
    */
  private lazy val isSingleFunction = mf.functions.length == 1

  /**
    * All possible arities of the functions.
    */
  private lazy val arities: Set[Int] = mf.functions.map(_.signature.arity).toSet

  /**
    * If the multi-function only has functions of the same arity, this option contains that arity.
    */
  private lazy val uniqueArity: Option[Int] = if (arities.size != 1) None else arities.headOption

  /**
    * Whether the transpiled multi-function uses JS rest parameters. This is needed if the arity of functions
    * differs.
    */
  private lazy val usingRestParameters = uniqueArity.isEmpty

  /**
    * Whether we can bypass creating product types around argument types on both the left and right side of
    * fits. If there is only a single argument across the board, product types are useless overhead. We can
    * easily optimize this overhead away.
    */
  private lazy val canUnpackInputProduct = uniqueArity.contains(1)

  /**
    * The list of multi-function parameters.
    *
    * TODO: We could technically even use the proper names if they don't differ across functions.
    */
  private lazy val jsParameterNames: Vector[String] = uniqueArity match {
    case None => Vector("args")
    case Some(arity) => (0 until arity).map(index => s"arg$index").toVector
  }

  /**
    * The comma-separated string of multi-function parameters.
    */
  private lazy val jsParameters: String = {
    if (usingRestParameters) s"...${jsParameterNames.head}" else jsParameterNames.mkString(", ")
  }

  /**
    * Maps all possible input types to temporary variable names. The temporary variables are written as global
    * constants so that JS doesn't have to recreate these objects every time the multi-function is called.
    */
  private def prepareFunctionInputTypes(printer: PrintStream)(implicit runtimeTypeVariables: RuntimeTypeVariables): Unit = {
    val inputTypes = mf.functions.map(_.signature.inputType).toSet
    inputTypes.foreach { inputType =>
      val simplifiedInputType = if (canUnpackInputProduct) {
        assert(inputType.components.size == 1)
        inputType.components.head
      } else inputType
      val varType = nameProvider.createName()
      val typeExpr = RuntimeTypeTranspiler.transpile(simplifiedInputType)
      printer.println(s"const $varType = $typeExpr;")
      inputTypeJsNames.put(inputType, varType)
    }
  }

  /**
    * This heuristic decides whether the dispatch cache should be used for this multi-function or not.
    *
    * The algorithm for this decision is, as of yet, very simple:
    *   1. If ANY parameter type is polymorphic, using the cache will absolutely bring gains. Creating a type
    *      allocation takes so much time that creating a hash from a type is almost 4-5 times faster than checking
    *      one fit.
    *   2. If there are at least three functions in the multi-function, we can assume that many invocations of the
    *      function will require three fit tests. At that point a hash&cache operation is faster than testing multiple
    *      fits.
    */
  private lazy val shouldUseDispatchCache = mf.functions.size >= 3 || mf.functions.exists(f => f.isPolymorphic)

  /**
    * Prepares the dispatch cache for later use.
    */
  private def prepareDispatchCache(printer: PrintStream): Unit = {
    if (shouldUseDispatchCache) {
      printer.println(s"const $varDispatchCache = ${RuntimeApi.utils.typeMap.create}();")
    }
  }

  /**
    * Whether this multi-function consists of a single function that also happens to have as its input type
    * the unit type. This vastly simplifies our handling of
    *
    * TODO: Use this to get rid of fits calls entirely and just check the number of arguments coming in.
    *       That is, if args.length === 0, call the function, otherwise throw an emptyFit error.
    */
  private lazy val isSingleUnitFunction = isSingleFunction && mf.functions.head.signature.inputType == ProductType.UnitType

  /**
    * Transpiles the code that gathers the argument types into a product type.
    */
  private def transpileArgumentTypeGathering(printer: PrintStream): Unit = {
    // Single unit functions are the best.
    if (isSingleUnitFunction) {
      printer.println(s"const $varArgumentType = ${RuntimeApi.types.unit};")
      return
    }

    // If we can unpack the product types, the generated code is very simple.
    if (canUnpackInputProduct) {
      assert(jsParameterNames.size == 1)
      printer.println(s"const $varArgumentType = ${RuntimeApi.types.typeOf}(${jsParameterNames.head});")
      return
    }

    // If we don't use the cache, there is no reason to hash this transient product type.
    val productConstructor = if (shouldUseDispatchCache) RuntimeApi.types.product else RuntimeApi.types.unsafe.unhashedProduct

    // If we are using rest parameters, we will have to gather the argument type with a loop.
    if (usingRestParameters) {
      val varArgumentTypes = "argumentTypes"
      val varArgs = jsParameterNames.head
      printer.println(
        s"""const $varArgumentTypes = new Array($varArgs.length);
           |for (let i = 0; i < $varArgs.length; i += 1) {
           |  $varArgumentTypes[i] = ${RuntimeApi.types.typeOf}($varArgs[i]);
           |}
           |const $varArgumentType = $productConstructor($varArgumentTypes);
           |""".stripMargin
      )
      return
    }

    // Otherwise, we know the number of arguments exactly.
    val typeofCalls = jsParameterNames.map(parameter => s"${RuntimeApi.types.typeOf}($parameter)").mkString(", ")
    printer.println(s"const $varArgumentType = $productConstructor([$typeofCalls])")
  }

  /**
    * This function turns the dispatch hierarchy of the multi-function into Javascript code adhering to the
    * following algorithm:
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
  private def transpileDispatchCall(printer: PrintStream): Unit = {
    val varTarget = "target"
    val varCachedTarget = "cachedTarget"
    def varFits(index: Int): String = s"fits$index"

    def transpileFitsConsts(nodes: Vector[(mf.hierarchy.NodeT, Int)]): Unit = {
      nodes.foreach { case (node, index) =>
        // TODO: Assert (at run-time) that the input type isn't polymorphic?
        val varRightType = inputTypeJsNames(node.signature.inputType)
        // We can decide at compile-time which version of the fit should be used, because the type on the right side
        // is constant. If the parameter type isn't polymorphic now, it won't ever be, so we can skip all that testing
        // for polymorphy at run-time.
        val fitsFunction = if (node.isPolymorphic) RuntimeApi.types.fitsPolymorphic else RuntimeApi.types.fitsMonomorphic
        // If we invoke the fitsPolymorphic function and the type fits, an Assignments map is returned rather than a boolean.
        printer.println(s"const ${varFits(index)} = $fitsFunction($varArgumentType, $varRightType);")
      }
    }

    def transpileDispatchNode(node: mf.hierarchy.NodeT, varFitsX: String): Unit = {
      val successors = node.diSuccessors.toVector.zipWithIndex
      val function = node.value
      printer.println(s"if ($varFitsX) {")
      val addFunction = if (!function.isAbstract) {
        val candidateName = TranspiledNames.function(function)
        val candidate = if (function.isPolymorphic) {
          // The first parameter of a polymorphic function is the map of type variable assignments passed to it at
          // run-time. Hence, we have to bind that map (saved in the fitsX variable after being returned by
          // fitsPolymorphic) to the candidate function. This turns the n-ary function into an (n-1)-ary function
          // and also plays nice with the dispatch cache. WIN-WIN!
          s"$candidateName.bind(null, $varFitsX)" // null refers to the value of 'this'.
        } else candidateName
        s"""const candidate = $candidate;
           |if ($varTarget && $varTarget !== candidate) ${RuntimeApi.utils.error.ambiguousCall}('${mf.name}', $varArgumentType);
           |$varTarget = candidate;
           |""".stripMargin
      } else {
        s"${RuntimeApi.utils.error.missingImplementation}('${mf.name}', '${function.signature.inputType}', $varArgumentType);"
      }
      if (successors.nonEmpty) {
        transpileFitsConsts(successors)
        val anyFits = successors.map { case (_, index) => varFits(index) }.mkString(" || ")
        printer.println(s"if (!($anyFits)) { $addFunction }")
        // We don't need to test for successors when we've already determined that none of the successors
        // fit. Hence the else.
        printer.println("else {")
      } else {
        printer.println(addFunction)
      }
      successors.foreach { case (successor, index) =>
        transpileDispatchNode(successor, varFits(index))
      }
      if (successors.nonEmpty) {
        printer.println("}") // Close the else we opened above.
      }
      printer.println("}")
    }

    def transpileTargetCall(target: String): Unit = {
      printer.println(s"return $target($jsParameters);")
    }

    if (shouldUseDispatchCache) {
      printer.println()
      printer.println(s"const $varCachedTarget = $varDispatchCache.get($varArgumentType);")
      printer.print(s"if ($varCachedTarget) {")
      transpileTargetCall(varCachedTarget)
      printer.println("}")
    }

    printer.println()
    printer.println(s"let $varTarget;")
    val indexedRoots = mf.hierarchyRoots.zipWithIndex
    transpileFitsConsts(indexedRoots)
    indexedRoots.foreach { case (root, index) => transpileDispatchNode(root, varFits(index)) }

    printer.println()
    printer.println(s"if (!$varTarget) ${RuntimeApi.utils.error.emptyFit}('${mf.name}', $varArgumentType);")
    if (shouldUseDispatchCache) {
      printer.println(s"$varDispatchCache.set($varArgumentType, $varTarget);")
    }
    transpileTargetCall(varTarget)
  }

}
