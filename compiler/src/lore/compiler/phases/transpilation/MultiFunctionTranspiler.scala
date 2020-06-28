package lore.compiler.phases.transpilation

import java.io.{ByteArrayOutputStream, PrintStream}

import lore.compiler.CompilerOptions
import lore.compiler.core.Compilation.C
import lore.compiler.core.Registry
import lore.compiler.functions.{FunctionDefinition, MultiFunctionDefinition}

import scala.collection.mutable
import scala.util.Using

class MultiFunctionTranspiler(mf: MultiFunctionDefinition)(implicit compilerOptions: CompilerOptions, registry: Registry) {
  private val varInputType = "inputType"
  private val varChosenFunction = "chosenFunction"

  private val functionJsNames = mutable.HashMap[FunctionDefinition, String]()

  private implicit val nameProvider: TemporaryNameProvider = new TemporaryNameProvider

  def transpile: C[String] = {
    val out = new ByteArrayOutputStream()
    val tryCompilation = Using(new PrintStream(out, true, "utf-8")) { printer =>
      mf.functions.filterNot(_.isAbstract).zipWithIndex.map { case (function, index) =>
        val name = s"${function.name}$$$index" // TODO: We should probably rely on some reconstructable unique name, rather.
        functionJsNames.put(function, name)
        new FunctionTranspiler(function, name).transpile.map(printer.print)
      }.simultaneous.map { _=>
        val mfName = s"${mf.name}"
        printer.println(s"export function $mfName(...args) {")
        if (compilerOptions.runtimeLogging) {
          printer.println(s"console.info('Called multi-function $mfName.');")
        }
        printer.println(s"const $varInputType = ${LoreApi.varTypes}.product(args.map(arg => ${LoreApi.varTypes}.typeOf(arg)));")
        printer.println(s"let $varChosenFunction;")
        transpileDispatchHierarchy(printer)
        printer.println(s"return $varChosenFunction(...args);")
        printer.println("}")
      }
    }

    tryCompilation.map(c => c.map(_ => out.toString("utf-8"))).get
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
    */
  private def transpileDispatchHierarchy(printer: PrintStream): Unit = {
    val varFunctions = "functions"
    def varFits(index: Int): String = s"fits$index"

    // TODO: Assert (at run-time) that the input type isn't polymorphic!
    def transpileInputType(node: mf.hierarchy.NodeT): TranspiledChunk = RuntimeTypeTranspiler.transpile(node.signature.inputType)

    def transpileFitsConsts(nodes: List[(mf.hierarchy.NodeT, Int)]): Unit = {
      nodes.foreach { case (node, index) =>
        val transpiledInputType = transpileInputType(node)
        printer.println(transpiledInputType.statements)
        printer.println(
          // TODO: Optimize: Pull the object creation of the function's input type into some kind of cached variable.
          //       Right now, every time the multi-function is called, all these instances get created again and
          //       again.
          s"const ${varFits(index)} = ${LoreApi.varTypes}.fits($varInputType, ${transpiledInputType.expression.get});"
        )
      }
    }

    def transpileDispatchNode(node: mf.hierarchy.NodeT, varFitsX: String): Unit = {
      val successors = node.diSuccessors.toList.zipWithIndex
      printer.println(s"if ($varFitsX) {")
      val addFunction = if (!node.isAbstract) {
        s"$varFunctions.add(${functionJsNames(node.value)});"
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
    printer.println(s"const $varFunctions = new Set();")
    val indexedRoots = mf.hierarchyRoots.zipWithIndex
    transpileFitsConsts(indexedRoots)
    indexedRoots.foreach { case (root, index) => transpileDispatchNode(root, varFits(index)) }

    printer.println()
    printer.println(
      s"""if ($varFunctions.size < 1) {
         |  throw new Error(`Could not find an implementation of ${mf.name} for the input type $${$varInputType}.`);
         |} else if ($varFunctions.size > 1) {
         |  throw new Error(`The multi-function ${mf.name} is ambiguous for the input type $${$varInputType}.`);
         |} else {
         |  $varChosenFunction = $varFunctions.values().next().value;
         |}
         |
         |""".stripMargin
    )
  }
}
