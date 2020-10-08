package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.structures.{MemberDefinition, StructDefinition}

object InstantiationTransformation {

  def transformCallStyleInstantiation(struct: StructDefinition, arguments: Vector[Expression])(implicit position: Position): Compilation[Expression] = {
    ExpressionVerification.adhereToSignature(arguments, struct.constructorSignature, position).map { _ =>
      val instantiationArguments = struct.members.zip(arguments).map(Expression.Instantiation.Argument.tupled)
      Expression.Instantiation(struct, instantiationArguments, position)
    }
  }

  case class DuplicateMember(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"The member $name occurs more than once. Members must be unique in map-style instantiation."
  }

  case class MissingMember(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"This map-style instantiation is missing a member $name."
  }

  case class IllegalEntry(name: String)(implicit position: Position) extends Error(position) {
    override def message: String = s"The struct to be instantiated does not have a member $name."
  }

  case class IllegallyTypedMember(member: MemberDefinition, expression: Expression) extends Error(expression) {
    override def message: String =
      s"The member ${member.name} is supposed to be assigned a value of type ${expression.tpe}. However," +
        s" the member itself has the type ${member.tpe}, which is not a subtype of ${expression.tpe}."
  }

  def transformMapStyleInstantiation(struct: StructDefinition, entries: Vector[(String, Expression)])(implicit position: Position): Compilation[Expression] = {
    // TODO: Deal with default values.

    def verifyNamesUnique(): Verification = {
      entries.map(_._1).groupBy(identity).map {
        case (name, vector) if vector.length > 1 => Compilation.fail(DuplicateMember(name))
        case _ => Verification.succeed
      }.toVector.simultaneous.verification
    }

    /**
      * Assigns entries to members, potentially filling missing members with their default values.
      */
    def correlateEntries(): Compilation[Vector[(MemberDefinition, Expression)]] = {
      var pairs = Vector.empty[(MemberDefinition, Expression)]
      var missing = Vector.empty[String]
      val illegal = entries.map(_._1).diff(struct.members.map(_.name))

      struct.members.foreach { member =>
        entries.find { case (name, _) => name == member.name } match {
          case Some((_, expression)) =>
            pairs = pairs :+ (member, expression)
          case None =>
            member.defaultValue match {
              case Some(defaultValue) => pairs = pairs :+ (member, Expression.Call(defaultValue.callTarget, Vector.empty, position))
              case None => missing = missing :+ member.name
            }
        }
      }

      if (missing.isEmpty && illegal.isEmpty) {
        Compilation.succeed(pairs)
      } else {
        Compilation.fail(missing.map(MissingMember(_)) ++ illegal.map(IllegalEntry(_)): _*)
      }
    }

    def verifyEntryTypes(pairs: Vector[(MemberDefinition, Expression)]): Verification = {
      pairs.map { case (member, expression) =>
        if (expression.tpe <= member.tpe) Verification.succeed else Compilation.fail(IllegallyTypedMember(member, expression))
      }.simultaneous.verification
    }

    for {
      _ <- verifyNamesUnique()
      pairs <- correlateEntries()
      _ <- verifyEntryTypes(pairs)
      arguments = pairs.map(Expression.Instantiation.Argument.tupled)
    } yield Expression.Instantiation(struct, arguments, position)
  }

}
