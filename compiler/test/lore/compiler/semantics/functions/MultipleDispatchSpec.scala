package lore.compiler.semantics.functions

import lore.compiler.test.{BaseSpec, TypeSyntax}
import lore.compiler.types.Type
import org.scalatest.Assertion

class MultipleDispatchSpec extends BaseSpec with TypeSyntax {
  def testFitAndMin(mf: MultiFunctionDefinition)(t: Type, expectedFit: Set[FunctionDefinition], expectedMin: Set[FunctionDefinition]): Assertion = {
    mf.fit(t).toSet should be (expectedFit)
    mf.min(t).toSet should be (expectedMin)
  }

  "The multi-function fit and min" should "be correctly defined for concat.lore" in {
    implicit val registry = prepareRegistry("concat")
    val concat = registry.getMultiFunction("concat").value
    val test = testFitAndMin(concat) _
    val setToString = Set(concat.exactGet(("ToString", "ToString")))
    val setList = Set(concat.exactGet(("List", "List")))
    val setLinkedList = Set(concat.exactGet(("LinkedList", "LinkedList")))
    val setLinkedListSorted = Set(concat.exactGet(("LinkedList", "LinkedList" & "Sorted")))

    test(("ToString", "ToString"), setToString, setToString)
    test(("ToString", "List"), setToString, setToString)
    test(("ToString" & "List", "List" & "ToString" & "ToString"), setToString ++ setList, setList)
    test(("LinkedList", "List"), setToString ++ setList, setList)
    test(("LinkedList", "LinkedList" & "Sorted"), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
    test(("LinkedList" & "Sorted", "LinkedList"), setToString ++ setList ++ setLinkedList, setLinkedList)
    test(("LinkedList" & "Sorted", "LinkedList" & "Sorted"), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
  }

  it should "be correctly defined for abstract.lore" in {
    implicit val registry = prepareRegistry("abstract")
    val mf = registry.getMultiFunction("f").value
    val test = testFitAndMin(mf) _
    val setT = Set(mf.exactGet("A" | "B" | "C"))
    val setA = Set(mf.exactGet("A"))
    val setA1 = Set(mf.exactGet("A1"))
    val setA2 = Set(mf.exactGet("A2"))
    val setB = Set(mf.exactGet("B"))
    val setC = Set(mf.exactGet("C"))

    setT.head should beAbstract
    setA.head should beAbstract

    test("A1", setT ++ setA ++ setA1, setA1)
    test("B", setT ++ setB, setB)
    test("A2", setT ++ setA ++ setA2, setA2)
    test("C", setT ++ setC, setC)
  }
}
