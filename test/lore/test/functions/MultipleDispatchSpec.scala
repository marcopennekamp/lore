package lore.test.functions

import lore.functions.{LoreFunction, MultiFunction}
import lore.test.BaseSpec
import lore.types.{Type, TypeSyntax}

class MultipleDispatchSpec extends BaseSpec with TypeSyntax {

  def testFitAndMin(mf: MultiFunction)(t: Type, expectedFit: Set[LoreFunction], expectedMin: Set[LoreFunction]) = {
    val fit = mf.fit(t)
    fit should be (expectedFit)
    fit.multiMin should be (expectedMin)
  }

  "The multi-function fit and min" should "be correctly defined for concat.lore" in {
    implicit val context = concatContext
    val concat = context.multiFunctions("concat")
    val test = testFitAndMin(concat) _
    val setToString = Set(concat.exact(("ToString", "ToString")))
    val setList = Set(concat.exact(("List", "List")))
    val setLinkedList = Set(concat.exact(("LinkedList", "LinkedList")))
    val setLinkedListSorted = Set(concat.exact(("LinkedList", "LinkedList" & "Sorted")))

    test(("ToString", "ToString"), setToString, setToString)
    test(("ToString", "List"), setToString, setToString)
    test(("ToString" & "List", "List" & "ToString" & "ToString"), setToString ++ setList, setList)
    test(("LinkedList", "List"), setToString ++ setList, setList)
    test(("LinkedList", "LinkedList" & "Sorted"), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
    test(("LinkedList" & "Sorted", "LinkedList"), setToString ++ setList ++ setLinkedList, setLinkedList)
    test(("LinkedList" & "Sorted", "LinkedList" & "Sorted"), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
  }

  it should "be correctly defined for area.lore" in {
    implicit val context = areaContext
    val area = context.multiFunctions("area")
    val test = testFitAndMin(area) _
    val setCircle = Set(area.exact("Circle"))
    val setBoundingBox = Set(area.exact("BoundingBox"))

    test("Circle", setCircle, setCircle)
    test("BoundingBox", setBoundingBox, setBoundingBox)
    test("Circle" & "BoundingBox", setCircle ++ setBoundingBox, setCircle ++ setBoundingBox)
    test("Rectangle", Set.empty, Set.empty)
    test("Rectangle" & "BoundingBox", setBoundingBox, setBoundingBox)
  }

  it should "be correctly defined for abstract.lore" in {
    implicit val context = abstractContext
    val mf = context.multiFunctions("f")
    val test = testFitAndMin(mf) _
    val setT = Set(mf.exact("T"))
    val setA = Set(mf.exact("A"))
    val setA1 = Set(mf.exact("A1"))
    val setA2 = Set(mf.exact("A2"))
    val setB = Set(mf.exact("B"))
    val setC = Set(mf.exact("C"))

    setT.head should beAbstract
    setA.head should beAbstract

    test("A1", setT ++ setA ++ setA1, setA1)
    test("B", setT ++ setB, setB)
    test("A2", setT ++ setA ++ setA2, setA2)
    test("C", setT ++ setC, setC)
  }

}
