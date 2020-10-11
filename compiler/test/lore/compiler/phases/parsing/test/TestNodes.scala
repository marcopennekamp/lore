package lore.compiler.phases.parsing.test

import lore.compiler.core.Position
import lore.compiler.syntax.{DeclNode, TypeDeclNode, StmtNode, TopLevelExprNode, ExprNode, TypeExprNode}

object TestNodes {
  private def withWildcard[R](construct: Position => R) =
    () => construct(Position.Wildcard)
  private def withWildcard[A, R](construct: (A, Position) => R) =
    (a: A) => construct(a, Position.Wildcard)
  private def withWildcard[A, B, R](construct: (A, B, Position) => R) =
    (a: A, b: B) => construct(a, b, Position.Wildcard)
  private def withWildcard[A, B, C, R](construct: (A, B, C, Position) => R) =
    (a: A, b: B, c: C) => construct(a, b, c, Position.Wildcard)
  private def withWildcard[A, B, C, D, R](construct: (A, B, C, D, Position) => R) =
    (a: A, b: B, c: C, d: D) => construct(a, b, c, d, Position.Wildcard)
  private def withWildcard[A, B, C, D, E, R](construct: (A, B, C, D, E, Position) => R) =
    (a: A, b: B, c: C, d: D, e: E) => construct(a, b, c, d, e, Position.Wildcard)
  private def withWildcard[A, B, C, D, E, F, R](construct: (A, B, C, D, E, F, Position) => R) =
    (a: A, b: B, c: C, d: D, e: E, f: F) => construct(a, b, c, d, e, f, Position.Wildcard)
  private def withWildcard[A, B, C, D, E, F, G, R](construct: (A, B, C, D, E, F, G, Position) => R) =
    (a: A, b: B, c: C, d: D, e: E, f: F, g: G) => construct(a, b, c, d, e, f, g, Position.Wildcard)

  object Decl {
    import DeclNode._
    import TypeDeclNode._

    val Function = withWildcard(FunctionNode.apply _)
    val Parameter = withWildcard(ParameterNode)
    val TypeVariable = withWildcard(TypeVariableNode)
    val Struct = withWildcard(StructNode)
    val Trait = withWildcard(TraitNode)
    val Property = withWildcard(PropertyNode)
    val Component = withWildcard(ComponentNode)
  }

  object Stmt {
    import StmtNode._
    import TopLevelExprNode._
    import ExprNode._

    val Return = withWildcard(ReturnNode)

    val VariableDeclaration = withWildcard(VariableDeclarationNode)
    val Assignment = withWildcard(AssignmentNode)

    val Variable = withWildcard(VariableNode)
    val RealLiteral = withWildcard(RealLiteralNode)
    val IntLiteral = withWildcard(IntLiteralNode)
    val Addition = withWildcard(AdditionNode)
    val Subtraction = withWildcard(SubtractionNode)
    val Multiplication = withWildcard(MultiplicationNode)
    val Division = withWildcard(DivisionNode)
    val Negation = withWildcard(NegationNode)
    val BoolLiteral = withWildcard(BoolLiteralNode)
    val Conjunction = withWildcard(ConjunctionNode)
    val Disjunction = withWildcard(DisjunctionNode)
    val LogicalNot = withWildcard(LogicalNotNode)
    val Equals = withWildcard(EqualsNode)
    val NotEquals = withWildcard(NotEqualsNode)
    val LessThan = withWildcard(LessThanNode)
    val LessThanEquals = withWildcard(LessThanEqualsNode)
    val GreaterThan = withWildcard(GreaterThanNode)
    val GreaterThanEquals = withWildcard(GreaterThanEqualsNode)
    val StringLiteral = withWildcard(StringLiteralNode)
    val Concatenation = withWildcard(ConcatenationNode)
    val Tuple = withWildcard(TupleNode)
    val Unit = withWildcard(UnitNode)
    val List = withWildcard(ListNode)
    val Map = withWildcard(MapNode)
    val KeyValue = withWildcard(KeyValueNode)
    val PropertyAccess = withWildcard(MemberAccessNode)
    val Block = withWildcard(BlockNode)
    val SimpleCall = withWildcard(SimpleCallNode)
    val FixedFunctionCall = withWildcard(FixedFunctionCallNode)
    val DynamicCall = withWildcard(DynamicCallNode)
    val IfElse = withWildcard(IfElseNode)
    val Repetition = withWildcard(WhileNode)
    val Iteration = withWildcard(ForNode)
    val Extractor = withWildcard(ExtractorNode)
  }

  object Type {
    import TypeExprNode._

    val Identifier = withWildcard(IdentifierNode)
    val Sum = withWildcard(SumNode)
    val Intersection = withWildcard(IntersectionNode)
    val Product = withWildcard(ProductNode)
    val Unit = withWildcard(UnitNode)
    val List = withWildcard(ListNode)
    val Map = withWildcard(MapNode)
    val Component = withWildcard(ComponentNode)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Types.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  lazy val tReal = Type.Identifier("Real")
  lazy val tInt = Type.Identifier("Int")
  lazy val tString = Type.Identifier("String")
  lazy val tBoolean = Type.Identifier("Boolean")

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Variables.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  lazy val va = Stmt.Variable("a")
  lazy val vb = Stmt.Variable("b")
  lazy val vc = Stmt.Variable("c")
  lazy val vx = Stmt.Variable("x")
  lazy val vy = Stmt.Variable("y")
  lazy val vz = Stmt.Variable("z")
  lazy val vi = Stmt.Variable("i")
  lazy val vk = Stmt.Variable("k")
}
