package lore.compiler.syntax

import lore.compiler.core
import lore.compiler.core.{Fragment, Position}

// TODO: As "state handling" and such becomes more messy, meaning we perform more and more transformations that bring
//       us further and further from the AST status quo after parsing, it might be useful to define a new intermediate
//       structure for each phase. So the parsing phase would produce an AST, which is then transformed into some other
//       kind of tree during verification. This would be consumed during transpilation. Verification would thus become
//       verification AND transformation. The ConcatToStringTransformer already exemplifies a good step in such a type
//       of pipeline.

trait Node {
  // TODO: We could turn nodes into an algebraic data type where we substitute node children with arbitrary
  //       types. This would allow us to define a statement visitor without the mass of ugly visit methods.
  //       We could simply have one visit method on which we match the original tree input IfELseNode[StmtNode]
  //       and the output IfElseNode[String] (in case of transpilation, for example).

  /**
    * The node's state, which at least contains the node's index.
    */
  def state: Node.State

  /**
    * The first position of the current node in the original source code.
    */
  def position: Position = state.position
}

object Node {
  type Index = Int

  /**
    * A node's State is mutable or secondary information associated with the node. Most state information is set during
    * later stages of the compilation process. A State object is explicitly declared to encapsulate the state in a
    * single object per node and to make it more explicit. It also has the advantage that copying a node (which are all
    * case classes) allows us to also copy the state.
    */
  trait State {
    // TODO: If we don't want copied nodes to share states, we will have to implement states as case classes... This
    //       should easily be possible.

    /**
      * The first position of the current node in the original source code.
      *
      * Implementing the position using State has the advantage that we don't need to add an explicit position property
      * to every node case class, while still being copy-able and semi-explicit.
      */
    var position: Position = _

    /**
      * For the purposes of node equality, states are always equal no matter their contents. This is mainly needed
      * for tests. Just don't ever test for state equality!
      *
      * I know this is a hack, but ScalaTest is not powerful enough to abstract this neatly. I tried implementing
      * custom equality, but it's a LOT of boilerplate just for some tests. I also tried to implement property
      * equality, but that sucks, too. It's a shame that you can tell ScalaTest to ignore the state property during
      * equality checking.
      *
      * TODO: The proper solution for this is to generate node classes with macros... We could then just ignore the
      *       state when testing node equality. It would also allow us to automatically generate visitors and such.
      *       AST classes are becoming harder and harder to maintain.
      */
    override def equals(obj: Any): Boolean = obj match {
      case _: State => true
      case _ => false
    }
    override def hashCode(): Int = throw new RuntimeException("Don't use a State as a key!")
    override def toString: String = "<state>"
  }

  /**
    * A "default state" that doesn't implement any further state properties.
    */
  class DefaultState extends State

  // TODO: Rename the following functions to attachPosition/withPosition? We are now attaching the position directly
  //       to the node instead of just the index, so the function names should reflect that.

  /**
    * Attaches the given index to the given node.
    */
  def attachIndex[R <: Node](node: R)(index: Index)(implicit fragment: Fragment): R = {
    node.state.position = core.Position(fragment, index)
    node
  }

  /**
    * Construct a 1-element node and set its index as given by the parser.
    */
  def withIndex[T1, R <: Node](construct: T1 => R)(args: (Index, T1))(implicit fragment: Fragment): R = {
    val (index, p1) = args
    attachIndex(construct(p1))(index)
  }

  /**
    * Construct a 1-element node and set its index as given by the parser.
    */
  def withIndexUntupled[T1, R <: Node](construct: T1 => R)(index: Index, p1: T1)(implicit fragment: Fragment): R = {
    attachIndex(construct(p1))(index)
  }

  /**
    * Construct a 2-element node and set its index as given by the parser.
    */
  def withIndex[T1, T2, R <: Node](construct: (T1, T2) => R)(args: (Index, T1, T2))(implicit fragment: Fragment): R = {
    val (index, p1, p2) = args
    attachIndex(construct(p1, p2))(index)
  }

  /**
    * Construct a 2-element node and set its index as given by the parser.
    */
  def withIndexUntupled[T1, T2, R <: Node](construct: (T1, T2) => R)(index: Index, p1: T1, p2: T2)(implicit fragment: Fragment): R = {
    attachIndex(construct(p1, p2))(index)
  }

  /**
    * Construct a 3-element node and set its index as given by the parser.
    */
  def withIndex[T1, T2, T3, R <: Node](construct: (T1, T2, T3) => R)(args: (Index, T1, T2, T3))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3) = args
    attachIndex(construct(p1, p2, p3))(index)
  }

  /**
    * Construct a 4-element node and set its index as given by the parser.
    */
  def withIndex[T1, T2, T3, T4, R <: Node](construct: (T1, T2, T3, T4) => R)(args: (Index, T1, T2, T3, T4))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3, p4) = args
    attachIndex(construct(p1, p2, p3, p4))(index)
  }

  /**
    * Construct a 5-element node and set its index as given by the parser.
    */
  def withIndex[T1, T2, T3, T4, T5, R <: Node](construct: (T1, T2, T3, T4, T5) => R)(args: (Index, T1, T2, T3, T4, T5))(implicit fragment: Fragment): R = {
    val (index, p1, p2, p3, p4, p5) = args
    attachIndex(construct(p1, p2, p3, p4, p5))(index)
  }
}
