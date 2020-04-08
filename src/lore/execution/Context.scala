package lore.execution

import lore.ast.{DeclNode, TypeDeclNode, TypeExprNode}
import lore.compiler.CompilationErrors
import lore.definitions.{ClassDefinition, LabelDefinition}
import lore.execution.Context._
import lore.functions.{InputAbstractnessConstraint, LoreFunction, MultiFunction, Parameter, TotalityConstraint}
import lore.parser.FragmentParser
import lore.types._

import scala.collection.mutable
import scala.io.Source

class Context(val types: Map[String, Type], val multiFunctions: Map[String, MultiFunction]) {
  implicit private val context: Context = this

  def verify(): VerificationResult = {
    val multiFunctionErrors = multiFunctions.values.flatMap { mf =>
      val violations =
        TotalityConstraint.verify(mf).map((_, TotalityConstraintViolation)) ++
        InputAbstractnessConstraint.verify(mf).map((_, InputAbstractnessConstraintViolation))
      if (violations.nonEmpty) {
        Seq((mf, MultiFunctionError(violations.toMap)))
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
  case object InputAbstractnessConstraintViolation extends FunctionError

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
    * Creates an unverified context from the example source.
    */
  def fromExample(name: String): Option[Context] = {
    // A new line is added at the end so the last statement has a closing newline.
    val source = Source.fromFile(s"examples/$name.lore").getLines.filter(_.trim.nonEmpty).mkString("\n") + "\n"
    val elements = FragmentParser.parse(source)
    if (elements.isEmpty) {
      println("Parsing failed with an error. Aborting context creation...")
    }
    elements.map(Context.build)
  }

  /**
    * Creates an unverified context from the given sequence of program elements.
    */
  def build(statements: Seq[DeclNode]): Context = {
    val types = mutable.HashMap[String, Type]()
    val multiFunctions = mutable.HashMap[String, MultiFunction]()

    def getType(name: String): Type = {
      types.getOrElse(name, throw CompilationErrors.TypeNotFound(name))
    }

    def resolveSupertype(maybeName: Option[String]): Option[Type] = {
      maybeName.map(name => getType(name))
    }

    def addFunction(function: LoreFunction): Unit = {
      val multiFunction = multiFunctions.getOrElse(function.name, MultiFunction(function.name, Set()))
      multiFunctions.put(function.name, MultiFunction(function.name, Set(function) ++ multiFunction.functions))
    }

    def evaluateTypeExpression(expression: TypeExprNode): Type = {
      expression match {
        case TypeExprNode.NominalNode(name) => getType(name)
        case TypeExprNode.ProductNode(expressions) =>
          val types = expressions.map(evaluateTypeExpression)
          ProductType(types)
        case TypeExprNode.IntersectionNode(expressions) =>
          val types = expressions.map(evaluateTypeExpression)
          IntersectionType.construct(types)
        case TypeExprNode.SumNode(expressions) =>
          val types = expressions.map(evaluateTypeExpression)
          SumType.construct(types)
      }
    }

    statements.foreach {
      case TypeDeclNode.LabelNode(name, supertypeName) =>
        val supertype = resolveSupertype(supertypeName)
        supertype match {
          // Note that we can't match Option[LabelType] directly because of JVM type erasure.
          case Some(_: LabelType) | None =>
            val tpe = new LabelType(supertype.asInstanceOf[Option[LabelType]])
            val definition = new LabelDefinition(name, tpe)
            tpe.initialize(definition)
            types.put(name, tpe)
          case _ => throw CompilationErrors.LabelMustExtendLabel(name)
        }
      case TypeDeclNode.ClassNode(name, supertypeName, isAbstract, _, _) =>
        // TODO: Duplicate code?
        val supertype = resolveSupertype(supertypeName)
        supertype match {
          case Some(_: ClassType) | None =>
            val tpe = new ClassType(supertype.asInstanceOf[Option[ClassType]], isAbstract)
            val definition = new ClassDefinition(name, tpe, List.empty) // TODO: Parse properties and such.
            tpe.initialize(definition)
            types.put(name, tpe)
          case _ => throw CompilationErrors.ClassMustExtendClass(name)
        }
      case TypeDeclNode.AliasNode(name, typeExpression) =>
        val tpe = evaluateTypeExpression(typeExpression)
        types.put(name, tpe)
      case DeclNode.FunctionNode(name, parameterDeclarations, isAbstract, _, _) =>
        val parameters = parameterDeclarations.map { decl =>
          Parameter(decl.name, evaluateTypeExpression(decl.tpe))
        }
        addFunction(LoreFunction(name, parameters, isAbstract))
    }

    new Context(types.toMap, multiFunctions.toMap)
  }

}
