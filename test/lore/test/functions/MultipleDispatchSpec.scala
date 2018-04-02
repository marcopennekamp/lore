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

  "The multi-function fit and min" should "be correctly defined for concat" in {
    implicit val context = concatContext
    val concat = context.multiFunctions("concat")
    val test = testFitAndMin(concat) _
    val setToString = Set(concat.exact(('ToString, 'ToString)))
    val setList = Set(concat.exact(('List, 'List)))
    val setLinkedList = Set(concat.exact(('LinkedList, 'LinkedList)))
    val setLinkedListSorted = Set(concat.exact(('LinkedList, 'LinkedList & 'Sorted)))

    test(('ToString, 'ToString), setToString, setToString)
    test(('ToString, 'List), setToString, setToString)
    test(('ToString & 'List, 'List & 'ToString & 'ToString), setToString ++ setList, setList)
    test(('LinkedList, 'List), setToString ++ setList, setList)
    test(('LinkedList, 'LinkedList & 'Sorted), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
    test(('LinkedList & 'Sorted, 'LinkedList), setToString ++ setList ++ setLinkedList, setLinkedList)
    test(('LinkedList & 'Sorted, 'LinkedList & 'Sorted), setToString ++ setList ++ setLinkedList ++ setLinkedListSorted, setLinkedListSorted)
  }

}
