package lore.compiler.generation

import lore.compiler.core.CompilationException
import lore.compiler.target.Target.{TargetName, TargetStatement}
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

    case Target.Iteration(collection, elementName, body) =>s"for (const ${generate(elementName)} of ${generate(collection)}) ${generate(body)}"

    case Target.Return(value) => s"return ${generate(value)};"

    case Target.VariableDeclaration(name, value, isMutable, shouldExport) =>
      val export = if (shouldExport) "export " else ""
      val modifier = if (isMutable) "let" else "const"
      s"$export$modifier ${generate(name)} = ${generate(value)};"

    case Target.Assignment(left, right) => s"${generate(left)} = ${generate(right)};"

    case Target.Variable(name) => generate(name)

    case Target.Function(name, parameters, body, shouldExport) =>
      val exportKeyword = if (shouldExport) "export " else ""
      val params = parameters.map(generateParameter).mkString(", ")
      s"${exportKeyword}function ${generate(name)}($params) ${generate(body)}"

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

    case Target.NumberLiteral(value) => value.toString
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
    case Target.PropertyAccess(instance, name) => s"${generate(instance)}${generatePropertyAccess(name.name)}"
    case Target.ListAccess(list, key) => s"${generate(list)}[${generate(key)}]"
  }

  /**
    * Target names need to be converted to a proper identifier that's legal in Javascript.
    */
  private def generate(targetName: TargetName): String = {
    val name2 = targetName.name.replace('?', '\u0294')
    name2.replace('!', '\u01c3')
  }

  private def generateParameter(parameter: Target.Parameter): String = {
    val rest = if (parameter.isRestParameter) "..." else ""
    val default = parameter.default.map(generate).map(v => s" = $v").getOrElse("")
    s"$rest${generate(parameter.name)}$default"
  }

  /**
    * Property names should be stringified so that special characters don't lead to errors in the generated code. The
    * prettifier will later remove quotation marks where possible.
    */
  private def generateProperty(property: Target.Property): String = s"${generate(Target.StringLiteral(property.name))}: ${generate(property.value)}"

  /**
    * We have to represent property names containing question marks as string literals, as question marks cannot be
    * parts of a Javascript identifier.
    */
  private def generatePropertyAccess(name: String): String = {
    if (name.contains("?") || name.contains("!")) {
      return s"[${generate(Target.StringLiteral(name))}]"
    }
    s".$name"
  }

  /**
    * Connects the strings with the given connector. If a string already ends with the connector, the connector is not
    * appended a second time.
    */
  private def connectWith(connector: Char)(strings: Vector[String]): String = {
    strings.map(s => s.trim.reverse.dropWhile(_ == connector).reverse).mkString(connector.toString)
  }

}
