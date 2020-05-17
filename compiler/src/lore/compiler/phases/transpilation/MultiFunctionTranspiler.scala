package lore.compiler.phases.transpilation

import java.io.{ByteArrayOutputStream, PrintStream}

import lore.compiler.Compilation
import lore.compiler.Compilation.C
import lore.compiler.definitions.{FunctionDefinition, MultiFunctionDefinition}

import scala.collection.mutable
import scala.util.Using

class MultiFunctionTranspiler(mf: MultiFunctionDefinition) {
  private val varInputType = "inputType"
  private val varChosenFunction = "chosenFunction"

  private case class GroupedFunction(function: FunctionDefinition, jsName: String, index: Int)

  private val functionJsNames = mutable.HashMap[FunctionDefinition, String]()

  def transpile: C[String] = {
    val out = new ByteArrayOutputStream()
    Using(new PrintStream(out, true, "utf-8")) { printer =>
      mf.functions.zipWithIndex.foreach { case (function, index) =>
        val name = s"${function.name}$$$index"
        functionJsNames.put(function, name)

        // TODO: Move this into a FunctionTranspiler.
        // TODO: Skip a function if it's abstract.
        val parameters = function.parameters.map(_.name).mkString(", ")
        printer.print(
          s"""function $name($parameters) {
             |  console.log('Called function $name');
             |}
             |
             |""".stripMargin
        )
      }

      val mfName = s"${mf.name}"
      printer.println(s"function $mfName(...args) {")
      printer.println(s"const $varInputType = Types.product(args.map(arg => Types.typeof(arg)));")
      printer.println(s"let $varChosenFunction;")
      transpileDispatchHierarchy(printer)
      printer.println(s"console.log('Called multi-function $mfName');")
      printer.println("}")
    }

    Compilation.succeed(out.toString("utf-8"))
  }

  /**
    * This function turns the dispatch hierarchy of the multi-function into Javascript code adhering to the
    * following algorithm:
    *
    *   1. First, using a "hierarchy" of nested if statements derived from the dispatch graph, build a list
    *      of functions that would be called with the given input type.
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

    def transpileFitsConsts(nodes: List[(mf.hierarchy.NodeT, Int)]): Unit = {
      nodes.foreach { case (node, index) =>
        printer.println(
          // TODO: Optimize: Pull the object creation of the function's input type into some kind of cached variable.
          //       Right now, every time the multi-function is called, all these instances get created again and
          //       again.
          s"const ${varFits(index)} = Types.isSubtype($varInputType, ${RuntimeTypeTranspiler.transpile(node.signature.inputType)});"
        )
      }
    }

    def transpileDispatchNode(node: mf.hierarchy.NodeT, varFitsX: String): Unit = {
      val successors = node.diSuccessors.toList.zipWithIndex
      printer.println(s"if ($varFitsX) {")
      val push = s"$varFunctions.push(${functionJsNames(node.value)});" // TODO: Rather throw an error if the function is abstract.
      if (successors.nonEmpty) {
        transpileFitsConsts(successors)
        val anyFits = successors.map { case (_, index) => varFits(index) }.mkString(" || ")
        printer.println(s"if (!($anyFits)) { $push }")
        // We don't need to test for successors when we've already determined that none of the successors
        // fit. Hence the else.
        printer.println("else {")
      } else {
        printer.println(push)
      }
      successors.foreach { case (successor, index) =>
        transpileDispatchNode(successor, varFits(index))
      }
      if (successors.nonEmpty) {
        printer.println("}") // Close the else we opened above.
      }
      printer.println("}")
    }

    printer.println(s"const $varFunctions = [];")
    val indexedRoots = mf.hierarchyRoots.zipWithIndex
    transpileFitsConsts(indexedRoots)
    indexedRoots.foreach { case (root, index) => transpileDispatchNode(root, varFits(index)) }

    // TODO: Unique the function array and see if there is one function or multiple.
  }
}
