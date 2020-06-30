package lore.compiler.phases.transpilation

import java.io.{ByteArrayOutputStream, PrintStream}

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation.C
import lore.compiler.core.Registry
import lore.compiler.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.types.Type

import scala.collection.mutable
import scala.util.Using

class MultiFunctionTranspiler(mf: MultiFunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry) {
  private val varInputType = "inputType"

  private val functionJsNames = mutable.HashMap[FunctionDefinition, String]()
  private val inputTypeJsNames = mutable.HashMap[Type, String]()

  private implicit val nameProvider: TemporaryNameProvider = new TemporaryNameProvider(s"${mf.name}__")

  def transpile: C[String] = {
    val out = new ByteArrayOutputStream()
    val tryCompilation = Using(new PrintStream(out, true, "utf-8")) { printer =>
      mf.functions.filterNot(_.isAbstract).zipWithIndex.map { case (function, index) =>
        val name = s"${function.name}$$$index" // TODO: We should probably rely on some reconstructable unique name, rather.
        functionJsNames.put(function, name)
        new FunctionTranspiler(function, name).transpile.map(printer.print)
      }.simultaneous.map { _=>
        prepareInputTypes(printer)
        val mfName = s"${mf.name}"
        printer.println(s"export function $mfName(...args) {")
        if (compilerOptions.runtimeLogging) {
          printer.println(s"console.info('Called multi-function $mfName.');")
        }

        transpileTypeofArguments(printer)
        transpileDispatchCall(printer)
        printer.println("}")
      }
    }

    tryCompilation.map(c => c.map(_ => out.toString("utf-8"))).get
  }

  /**
    * Maps all possible input types to temporary variable names. The temporary variables are written as global
    * constants so that JS doesn't have to recreate these objects every time the multi-function is called.
    */
  private def prepareInputTypes(printer: PrintStream): Unit = {
    val inputTypes = mf.functions.map(_.signature.inputType).toSet
    inputTypes.foreach { inputType =>
      val varType = s"${nameProvider.createName()}"
      val chunk = RuntimeTypeTranspiler.transpile(inputType)
      printer.println(chunk.statements)
      printer.println(s"const $varType = ${chunk.expression.get}")
      inputTypeJsNames.put(inputType, varType)
    }
  }

  /**
    * Transpiles the code that gathers the argument types into a product type.
    */
  private def transpileTypeofArguments(printer: PrintStream): Unit = {
    val varArgumentTypes = "argumentTypes"
    printer.println(
      s"""const $varArgumentTypes = []
         |for (let i = 0; i < args.length; i += 1) {
         |  $varArgumentTypes.push(${LoreApi.varTypes}.typeOf(args[i]));
         |}
         |const $varInputType = ${LoreApi.varTypes}.product($varArgumentTypes);
         |""".stripMargin)
  }

  /**
    * This function turns the dispatch hierarchy of the multi-function into Javascript code adhering to the
    * following algorithm:
    *
    *   1. First, using a "hierarchy" of nested if statements derived from the dispatch graph, build a list
    *      of functions that would be called with the given input type. In addition, throw an error for any
    *      ABSTRACT functions that are selected.
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
    * Hence, we can cache the result of this dispatch algorithm at runtime for real values. (Maybe we should
    * limit the cache to the 100 most accessed types, or a number depending on the total size of the multi-function).
    *
    *
    */
  private def transpileDispatchCall(printer: PrintStream): Unit = {
    val varFunctions = "functions"
    def varFits(index: Int): String = s"fits$index"



    def transpileFitsConsts(nodes: List[(mf.hierarchy.NodeT, Int)]): Unit = {
      nodes.foreach { case (node, index) =>
        // TODO: Assert (at run-time) that the input type isn't polymorphic?
        val varRightType = inputTypeJsNames(node.signature.inputType)
        printer.println(
          // TODO: Optimize: Pull the object creation of the function's input type into some kind of cached variable.
          //       Right now, every time the multi-function is called, all these instances get created again and
          //       again.
          s"const ${varFits(index)} = ${LoreApi.varTypes}.fits($varInputType, $varRightType);"
        )
      }
    }

    def transpileDispatchNode(node: mf.hierarchy.NodeT, varFitsX: String): Unit = {
      val successors = node.diSuccessors.toList.zipWithIndex
      printer.println(s"if ($varFitsX) {")
      val addFunction = if (!node.isAbstract) {
        s"${LoreApi.varTinySet}.add($varFunctions, ${functionJsNames(node.value)});"
      } else {
        s"throw new Error(`The abstract function ${mf.name}${node.signature.inputType} is missing an" +
          s" implementation for $${$varInputType}.`);"
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

    printer.println()
    printer.println(s"const $varFunctions = [];")
    val indexedRoots = mf.hierarchyRoots.zipWithIndex
    transpileFitsConsts(indexedRoots)
    indexedRoots.foreach { case (root, index) => transpileDispatchNode(root, varFits(index)) }

    printer.println()
    printer.println(
      s"""if ($varFunctions.length < 1) {
         |  throw new Error(`Could not find an implementation of ${mf.name} for the input type $${$varInputType}.`);
         |} else if ($varFunctions.length > 1) {
         |  throw new Error(`The multi-function ${mf.name} is ambiguous for the input type $${$varInputType}.`);
         |} else {
         |  return $varFunctions[0](...args);
         |}
         |""".stripMargin
    )
  }
}
