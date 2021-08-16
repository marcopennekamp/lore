package lore.compiler.phases.generation

import lore.compiler.core.CompilationException
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.{Target, TargetOperator}

object JavascriptGenerator {

  def generate(statement: TargetStatement): String = statement match {
    case Target.Empty => ""
    case Target.Divider => s"\n\n/* ${"=".repeat(74)} */\n\n"

    case Target.Block(statements) => s"{ ${statements.map(generate).mkString("\n")} }"

    case Target.IfElse(condition, thenStatement, elseStatement) =>
      val ifPart = s"if (${generate(condition)}) ${generate(thenStatement)}"
      if (!Target.isEmpty(elseStatement)) ifPart + s" else ${generate(elseStatement)}" else ifPart

    case Target.While(condition, body) => s"while (${generate(condition)}) ${generate(body)}"

    case Target.For(init, condition, post, body) =>
      val header = connectWith(';')(Vector(generate(init), generate(condition), generate(post)))
      s"for ($header) ${generate(body)}"

    case Target.Iteration(collection, elementName, body) =>s"for (const $elementName of ${generate(collection)}) ${generate(body)}"

    case Target.Return(value) => s"return ${generate(value)};"

    case Target.VariableDeclaration(name, value, isMutable) =>
      val modifier = if (isMutable) "let" else "const"
      s"$modifier $name = ${generate(value)};"

    case Target.Assignment(left, right) => s"${generate(left)} = ${generate(right)};"

    case Target.Variable(name) => name.toString

    case Target.Function(name, parameters, body, shouldExport) =>
      val exportKeyword = if (shouldExport) "export " else ""
      val params = parameters.map(generateParameter).mkString(", ")
      s"${exportKeyword}function $name($params) ${generate(body)}"

    case Target.Lambda(parameters, body) =>
      val params = parameters.map(generateParameter).mkString(", ")
      s"($params) => ${generate(body)}"

    case Target.Call(function, arguments, isRestCall) =>
      val rest = if (isRestCall) "..." else ""
      val args = arguments.map(generate).mkString(", ")
      s"${generate(function)}($rest$args)"

    case Target.New(constructor, arguments) =>
      val args = arguments.map(generate).mkString(",")
      s"new ${generate(constructor)}($args)"

    case Target.RealLiteral(value) => value.toString
    case Target.IntLiteral(value) => value.toString
    case Target.BooleanLiteral(value) => value.toString
    case Target.StringLiteral(value) =>
      // Escaped characters such as \` need to be handled correctly. This is why we're using a proper JSON library.
      import org.json4s.JsonDSL._
      import org.json4s.native.JsonMethods._
      compact(render(value))
    case Target.Undefined => "undefined"
    case Target.Null => "null"

    case Target.Dictionary(properties) => s"{ ${properties.map(generateProperty).mkString(", ")} }"
    case Target.List(elements) => s"[${elements.map(generate).mkString(", ")}]"

    case Target.Operation(operator, operands) =>
      val args = operands.map(generate)
      def xary(op: String) = args.mkString(s" $op ")
      def binary(op: String) = {
        if (args.length != 2) throw CompilationException("Binary operations must have exactly two operands.")
        s"${args.head} $op ${args.last}"
      }
      def unary(op: String) = {
        if (args.length != 1) throw CompilationException("Unary operations must have exactly one operand.")
        s"$op${args.head}"
      }
      val expression = operator match {
        case TargetOperator.Addition => xary("+")
        case TargetOperator.Subtraction => xary("-")
        case TargetOperator.Multiplication => xary("*")
        case TargetOperator.Division => xary("/")
        case TargetOperator.Negation => unary("-")
        case TargetOperator.Truthy => unary("")
        case TargetOperator.Equals => binary("===")
        case TargetOperator.NotEquals => binary("!==")
        case TargetOperator.LessThan => binary("<")
        case TargetOperator.LessThanEquals => binary("<=")
        case TargetOperator.And => xary("&&")
        case TargetOperator.Or => xary("||")
        case TargetOperator.Not => unary("!")
        case TargetOperator.Concat => xary("+")
      }
      s"($expression)"
    case Target.PropertyAccess(instance, name) => s"${generate(instance)}.$name"
    case Target.ListAccess(list, key) => s"${generate(list)}[${generate(key)}]"
  }

  private def generateParameter(parameter: Target.Parameter): String = {
    val rest = if (parameter.isRestParameter) "..." else ""
    val default = parameter.default.map(generate).map(v => " = v").getOrElse("")
    s"$rest${parameter.name}$default"
  }

  private def generateProperty(property: Target.Property): String = s"${generate(Target.StringLiteral(property.name))}: ${generate(property.value)}"

  /**
    * Connects the strings with the given connector. If a string already ends with the connector, the connector is not
    * appended a second time.
    */
  private def connectWith(connector: Char)(strings: Vector[String]): String = {
    strings.map(s => s.trim.reverse.dropWhile(_ == connector).reverse).mkString(connector.toString)
  }

}
