package lore.test.functions

import lore.definitions.{FunctionDefinition, MultiFunctionDefinition}
import lore.test.BaseSpec
import lore.types.{Type, TypeSyntax}
import org.scalatest.Assertion

class MultipleDispatchSpec extends BaseSpec with TypeSyntax {
  def testFitAndMin(mf: MultiFunctionDefinition)(t: Type, expectedFit: Set[FunctionDefinition], expectedMin: Set[FunctionDefinition]): Assertion = {
    val fit = mf.fit(t)
    fit should be (expectedFit)
    fit.multiMin should be (expectedMin)
  }

  "The multi-function fit and min" should "be correctly defined for concat.lore" in {
    implicit val registry = concatRegistry
    val concat = registry.getMultiFunction("concat").value
    println(concat.functions)
    val test = testFitAndMin(concat) _
    val setToString = Set(concat.exactGet(("ToString", "ToString")))
    println(1)
    val setList = Set(concat.exactGet(("List", "List")))
    println(2)
    val setLinkedList = Set(concat.exactGet(("LinkedList", "LinkedList")))
    println(3)
    val setLinkedListSorted = Set(concat.exactGet(("LinkedList", "LinkedList" & "Sorted")))
    println(4)

    test(("ToString", "ToString"), setToString, setToString)
    test(("ToString", "List"), setToString, setToString)
    test(("ToString" & "List", "List" & "ToString" & "ToString"), setToString ++ setList, setList)
    test(("LinkedList", "List"), setToString ++ setList, setList)
    test(("LinkedList", "LinkedList" & "Sorted"), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
    test(("LinkedList" & "Sorted", "LinkedList"), setToString ++ setList ++ setLinkedList, setLinkedList)
    test(("LinkedList" & "Sorted", "LinkedList" & "Sorted"), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
  }

  it should "be correctly defined for area.lore" in {
    implicit val registry = areaRegistry
    val area = registry.getMultiFunction("area").value
    val test = testFitAndMin(area) _
    val setCircle = Set(area.exactGet("Circle"))
    val setBoundingBox = Set(area.exactGet("BoundingBox"))

    test("Circle", setCircle, setCircle)
    test("BoundingBox", setBoundingBox, setBoundingBox)
    test("Circle" & "BoundingBox", setCircle ++ setBoundingBox, setCircle ++ setBoundingBox)
    test("Rectangle", Set.empty, Set.empty)
    test("Rectangle" & "BoundingBox", setBoundingBox, setBoundingBox)
  }

  it should "be correctly defined for abstract.lore" in {
    implicit val registry = abstractRegistry
    val mf = registry.getMultiFunction("f").value
    val test = testFitAndMin(mf) _
    val setT = Set(mf.exactGet("T"))
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
