package lore.lsp.index

import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{DeclaredType, IntersectionType, ShapeType, Type, TypeVariable}
import lore.lsp.index.IndexTypeDeclaration.IndexMemberDeclaration
import lore.lsp.utils.PositionUtil

object MemberUsageBuilder {

  def buildMemberUsage(expression: Expression.MemberAccess)(implicit globalIndex: GlobalIndex): IndexMemberUsage = {
    val declarations = getIndexMemberDeclarations(expression.instance.tpe, expression.name)
    IndexMemberUsage(declarations, PositionUtil.toLocation(expression.position))
  }

  /**
    * For a given instance type (which may be a constructed type, such as an intersection type), return all index
    * member declarations for a member of the given name.
    *
    * Declared types will always return a vector with a single member declaration, because there can only be one point
    * of declaration. Intersection types can result in multiple declaration locations, however.
    */
  def getIndexMemberDeclarations(instanceType: Type, memberName: String)(implicit globalIndex: GlobalIndex): Vector[IndexMemberDeclaration] = {
    instanceType match {
      case dt: DeclaredType => globalIndex.getTypeDeclaration(dt.name).flatMap(_.getMemberDeclaration(memberName)).toVector
      case tv: TypeVariable => getIndexMemberDeclarations(tv.upperBound, memberName)
      case IntersectionType(types) => types.toVector.flatMap(getIndexMemberDeclarations(_, memberName))
      case _ => Vector.empty
    }
  }

}
