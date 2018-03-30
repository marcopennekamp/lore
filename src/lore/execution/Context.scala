package lore.execution

import lore.ast._
import lore.exceptions.TypeNotFoundException
import lore.functions.{LoreFunction, MultiFunction, Parameter}
import lore.types._

import scala.collection.mutable

class Context(val types: Map[String, Type], val multiFunctions: Map[String, MultiFunction], val calls: Seq[Call])

object Context {

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
      }
    }

    statements.foreach {
      case TypeDeclaration(name, maybeSupertypeName) =>
        val supertype = maybeSupertypeName
          .map(supertypeName => getType(supertypeName))
          .getOrElse(AnyType)
        types.put(name, LabelType(name, supertype))
      case FunctionDeclaration(name, parameterDeclarations, isAbstract) =>
        val parameters = parameterDeclarations.map { decl =>
          Parameter(decl.name, evaluateTypeExpression(decl.typeExpression))
        }
        addFunction(LoreFunction(name, parameters, isAbstract))
      case CallWith(functionName, typeExpression) =>
        val argumentType = evaluateTypeExpression(typeExpression)
        calls += Call(functionName, argumentType)
    }

    new Context(types.toMap, multiFunctions.toMap, calls)
  }

}
