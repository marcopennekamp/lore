package lore.compiler.benchmarks

import lore.compiler.test.TypeSyntax
import lore.compiler.types.SumType
import lore.compiler.utils.Timer.{TimerUnit, timed}

/**
  * Benchmarks equality and subtyping operations. This should be kept in tune with the runtime type benchmarks, found
  * at `runtime/benchmarks/types.ts`.
  */
object TypeBenchmarks extends TypeSyntax {

  private val iterations = 10000000

  private val sum1 = SumType(Set(string | int, boolean))
  private val sum2 = string | int | boolean
  private val sum3 = string | int | boolean

  private val intersection1 = string & int & boolean
  private val intersection2 = boolean & string & int
  private val intersection3 = string & int & boolean

  private val list1 = list(int)
  private val list2 = list(int)
  private val list3 = list(string #> int)
  private val list4 = list(string #> int)

  private val tuple1 = tuple(sum2, intersection1, list3)
  private val tuple2 = tuple(sum2, intersection1, list3)
  private val tuple3 = tuple(string | int | boolean, string & int & boolean, list(string #> int))
  private val tuple4 = tuple(string | int | boolean, string & int & boolean, list(string #> int))

  private val shape1 = shape("x" -> real, "y" -> real)
  private val shape2 = shape("x" -> real, "y" -> real, "c" -> int, "d" -> string)
  private val shape3 = shape("d" -> string, "c" -> int, "x" -> real, "y" -> real)

  private def benchmark[A](name: String) = timed[A](name, iterations, println, TimerUnit.Nanoseconds) _

  def benchmarkAreEqual(): Unit = {
    println("Benchmarking equality:")

    benchmark("nested sum")(sum1 == sum2)
    benchmark("equal sum (reference)")(sum2 == sum2)
    benchmark("equal sum (structural)")(sum2 == sum3)
    benchmark("basic types")(int == string)
    benchmark("equal intersection (structural)")(intersection1 == intersection2)
    benchmark("equal intersection (structural, favorable order)")(intersection1 == intersection3)
    benchmark("equal basic type list (structural)")(list1 == list2)
    benchmark("equal map list (structural)")(list3 == list4)
    benchmark("not equal list")(list1 == list3)
    benchmark("equal tuple (structural, shallow)")(tuple1 == tuple2)
    benchmark("equal tuple (structural, deep)")(tuple3 == tuple4)
    benchmark("equal shape (structural)")(shape2 == shape3)
    benchmark("empty run")(() => {})

    println()
  }

  def benchmarkIsSubtype(): Unit = {
    println("Benchmarking subtyping:")

    benchmark("nested sum")(sum1 <= sum2)
    benchmark("subtyped sum (reference)")(sum2 <= sum2)
    benchmark("subtyped sum (structural)")(sum2 <= sum3)
    benchmark("not subtyped basic types")(int <= string)
    benchmark("subtyped intersection (structural)")(intersection1 <= intersection2)
    benchmark("subtyped intersection (structural, favorable order)")(intersection1 <= intersection3)
    benchmark("subtyped basic type list (structural)")(list1 <= list2)
    benchmark("subtyped map list (structural)")(list3 <= list4)
    benchmark("not subtyped list")(list1 <= list3)
    benchmark("subtyped tuple (structural, shallow)")(tuple1 <= tuple2)
    benchmark("subtyped tuple (structural, deep)")(tuple3 <= tuple4)
    benchmark("subtyped shape (structural)")(shape2 <= shape1)
    benchmark("empty run")({ })

    println()
  }

  def main(args: Array[String]): Unit = {
    benchmarkAreEqual()
    benchmarkIsSubtype()
  }

}
