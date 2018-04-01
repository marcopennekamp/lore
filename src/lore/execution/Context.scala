package lore.execution

import lore.ast._
import lore.exceptions.TypeNotFoundException
import lore.execution.Context._
import lore.functions.{LoreFunction, MultiFunction, Parameter, TotalityConstraint}
import lore.parser.FragmentParser
import lore.types._

import scala.collection.mutable
import scala.io.Source

class Context(val types: Map[String, Type], val multiFunctions: Map[String, MultiFunction], val calls: Seq[Call]) {
  implicit private val context = this

  def verify(): VerificationResult = {
    val multiFunctionErrors = multiFunctions.values.flatMap { mf =>
      val violatingFunctions = TotalityConstraint.verify(mf)
      if (violatingFunctions.nonEmpty) {
        Seq((mf, MultiFunctionError(violatingFunctions.map((_, TotalityConstraintViolation)).toMap)))
      } else {
        Seq.empty
      }
    }.toMap

    if (multiFunctionErrors.nonEmpty) {
      VerificationFailure(multiFunctionErrors)
    } else {
      VerificationSuccess
    }
  }
}

object Context {

  sealed trait FunctionError
  case object TotalityConstraintViolation extends FunctionError

  case class MultiFunctionError(functionErrors: Map[LoreFunction, FunctionError])

  sealed trait VerificationResult {
    def print(): Unit
  }
  case object VerificationSuccess extends VerificationResult {
    override def print(): Unit = println("Context verification was successful.")
  }
  case class VerificationFailure(multiFunctionErrors: Map[MultiFunction, MultiFunctionError]) extends VerificationResult {
    override def print(): Unit = {
      multiFunctionErrors.foreach { case (mf, mfe) =>
        println(s"The multi-function ${mf.name} has functions that are not valid:")
        mfe.functionErrors.foreach { case (f, fe) =>
          println(s"  $f has a $fe")
        }
      }
    }
  }

  /**
    * @return An unverified context built from the example source.
    */
  def fromExample(name: String): Context = {
    // A new line is added at the end so the last statement has a closing newline.
    val source = Source.fromFile(s"examples/$name.lore").getLines.filter(_.trim.nonEmpty).mkString("\n") + "\n"
    val elements = FragmentParser.parse(source)
    Context.build(elements)
  }

  /**
    * @return An unverified context built from the given sequence of program elements.
    */
  def build(statements: Seq[TopLevelElement]): Context = {
    val types = mutable.HashMap[String, Type]()
    val multiFunctions = mutable.HashMap[String, MultiFunction]()
    val calls = mutable.ListBuffer[Call]()

    def getType(name: String): Type = {
      types.getOrElse(name, throw TypeNotFoundException(name))
    }

    def addFunction(function: LoreFunction): Unit = {
      val multiFunction = multiFunctions.getOrElse(function.name, MultiFunction(function.name, Set()))
      multiFunctions.put(function.name, MultiFunction(function.name, Set(function) ++ multiFunction.functions))
    }

    def evaluateTypeExpression(expression: TypeExpression): Type = {
      expression match {
        case TypeVariable(name) => getType(name)
        case TupleTypeExpression(expressions) =>
          val types = expressions.map(evaluateTypeExpression)
          TupleType(types)
        case IntersectionTypeExpression(expressions) =>
          val types = expressions.map(evaluateTypeExpression)
          IntersectionType(types)
        case SumTypeExpression(expressions) =>
          val types = expressions.map(evaluateTypeExpression)
          SumType(types)
      }
    }

    statements.foreach {
      case LabelTypeDeclaration(name, maybeSupertypeName) =>
        val supertype = maybeSupertypeName
          .map(supertypeName => getType(supertypeName))
          .getOrElse(AnyType)
        types.put(name, LabelType(name, supertype))
      case TypeDeclaration(name, typeExpression) =>
        val tpe = evaluateTypeExpression(typeExpression)
        types.put(name, tpe)
      case FunctionDeclaration(name, parameterDeclarations, isAbstract) =>
        val parameters = parameterDeclarations.map { decl =>
          Parameter(decl.name, evaluateTypeExpression(decl.typeExpression))
        }
        addFunction(LoreFunction(name, parameters, isAbstract))
      case CallWith(functionName, typeExpression) =>
        val argumentTypes = evaluateTypeExpression(typeExpression)
        calls += Call(functionName, argumentTypes.toTuple)
    }

    new Context(types.toMap, multiFunctions.toMap, calls)
  }

}
