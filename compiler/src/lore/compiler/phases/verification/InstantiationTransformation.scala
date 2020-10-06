package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.structures.{MemberDefinition, StructDefinition}
import lore.compiler.utils.CollectionExtensions.VectorExtension

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

    def correlateEntries(): Compilation[Vector[(MemberDefinition, Expression)]] = {
      val (pairs, missing, illegal) = struct.members.correlate(entries) { case (member, (name, _)) => member.name == name }

      if (missing.isEmpty && illegal.isEmpty) {
        Compilation.succeed(pairs.map { case (member, (_, expression)) => (member, expression) })
      } else {
        Compilation.fail(missing.map(member => MissingMember(member.name)) ++ illegal.map { case (name, _) => IllegalEntry(name) }: _*)
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
