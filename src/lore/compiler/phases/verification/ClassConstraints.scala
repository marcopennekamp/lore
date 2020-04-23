package lore.compiler.phases.verification

import lore.ast.{ExprNode, TopLevelExprNode}
import lore.ast.visitor.StmtVisitor
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.{Compilation, Fragment, Registry}
import lore.definitions.{ClassDefinition, ComponentDefinition, ConstructorDefinition, MemberDefinition}
import lore.types.{AnyType, Subtyping, Type}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

object ClassConstraints {
  /**
    * Verifies:
    *   1. Non-entity classes may not extend entities.
    *   2. The owned-by type of a class must be a subtype of the owned-by type of its superclass. If the class
    *      or superclass has no owned-by type, assume Any.
    *   3. Class properties and components must be unique.
    *   4. If a component member has an ownedBy type, we verify that the component can be in fact owned by this entity.
    *   5. Each component overriding another component must be a subtype of the overridden component.
    *   6. Each constructor must end with a continuation node and may not have such a node in any other place.
    *      Continuations must be acyclic and must result in a construct continuation.
    */
  def verify(definition: ClassDefinition)(implicit registry: Registry): Verification = {
    (
      verifyEntityInheritance(definition),
      verifyOwnedBy(definition),
      verifyMembersUnique(definition),
      definition.localComponents.map(verifyCanOwn(definition, _)).simultaneous,
      definition.localComponents.map(verifyOverrides(definition, _)).simultaneous,
      verifyContinuations(definition),
    ).simultaneous.verification
  }

  case class ClassMayNotExtendEntity(definition: ClassDefinition) extends Error(definition) {
    override def message = s"The class ${definition.name} extends an entity but is not an entity itself."
  }

  /**
    * Verifies that the given class does not extend an entity if it is itself not an entity.
    */
  def verifyEntityInheritance(definition: ClassDefinition): Verification = {
    // If the definition is not an entity, its supertype may not be an entity.
    if (!definition.isEntity && definition.supertypeDefinition.exists(_.isEntity)) {
      Compilation.fail(ClassMayNotExtendEntity(definition))
    } else Verification.succeed
  }

  case class OwnedByMustBeSubtype(definition: ClassDefinition, ownedBy: Type, superOwnedBy: Type) extends Error(definition) {
    override def message = s"The owned-by type $ownedBy of class ${definition.name} must be a subtype of the superclass's owned-by type $superOwnedBy."
  }

  /**
    * Verifies that the owned-by type of the given class is a subtype of the owned-by type of the superclass.
    */
  def verifyOwnedBy(definition: ClassDefinition): Verification = {
    // We have to assume Any, because this handles a special case where the owned-by declaration of a class has
    // been forgotten, despite the superclass having its own owned-by type.
    val ownedBy = definition.tpe.ownedBy.map(_.tpe).getOrElse(AnyType)

    // Here, we assume Any in case the supertype is None or its owned-by type is None. In both cases, the omission
    // of such a declaration means that the supertype's owned-by type is effectively Any.
    val superOwnedBy = definition.tpe.supertype.flatMap(_.ownedBy).map(_.tpe).getOrElse(AnyType)

    // Now we just have to check whether the owned-by type is actually a subtype.
    if (!Subtyping.isSubtype(ownedBy, superOwnedBy)) {
      Compilation.fail(OwnedByMustBeSubtype(definition, ownedBy, superOwnedBy))
    } else Verification.succeed
  }

  case class MemberAlreadyExistsInSuperclass(definition: ClassDefinition, member: MemberDefinition[Type]) extends Error(member) {
    override def message = s"The member ${member.name} is already declared in a superclass of class ${definition.name}."
  }

  case class MemberDuplicateDeclaration(definition: ClassDefinition, member: MemberDefinition[Type]) extends Error(member) {
    override def message = s"The member ${member.name} is already declared twice in the class ${definition.name}."
  }

  /**
    * Verifies that this class's local members are unique and haven't already been declared in a superclass
    * or twice in the given class.
    */
  def verifyMembersUnique(definition: ClassDefinition): Verification = {
    val superMembers = definition.supertypeDefinition.map(_.members).getOrElse(List.empty)
    val superMemberNames = superMembers.map(_.name)
    definition.localMembers.map { member =>
      (

        if (superMemberNames.contains(member.name)) {
          Compilation.fail(MemberAlreadyExistsInSuperclass(definition, member))
        } else Verification.succeed,

        if (definition.localMembers.filterNot(_ == member).map(_.name).contains(member.name)) {
          Compilation.fail(MemberDuplicateDeclaration(definition, member))
        } else Verification.succeed,
      ).simultaneous
    }.simultaneous.verification
  }

  case class ClassCannotOwnComponent(definition: ClassDefinition, component: ComponentDefinition) extends Error(component) {
    override def message = s"The class ${definition.name} cannot own the component ${component.name} due to the component's owned-by restriction."
  }

  /**
    * Verifies that the given class can in fact own the given component. (In principle, with the information
    * available at compile-time.)
    */
  def verifyCanOwn(definition: ClassDefinition, component: ComponentDefinition): Verification = {
    val ownershipType = component.tpe.ownedBy.map(_.tpe).getOrElse(AnyType)
    if (!Subtyping.isSubtype(definition.tpe, ownershipType)) {
      Compilation.fail(ClassCannotOwnComponent(definition, component))
    } else Verification.succeed
  }

  case class OverriddenComponentDoesNotExist(definition: ClassDefinition, component: ComponentDefinition) extends Error(component) {
    val overriddenName: String = component.overrides.getOrElse(throw new RuntimeException("Compilation bug: component.overrides should exist."))
    override def message: String = s"The component ${component.name} is supposed to override the component $overriddenName, " +
      s"but $overriddenName is not a supertype component or has already been overridden."
  }

  case class ComponentMustSubtypeOverriddenComponent(definition: ClassDefinition, component: ComponentDefinition) extends Error(component) {
    val overriddenName: String = component.overrides.getOrElse(throw new RuntimeException("Compilation bug: component.overrides should exist."))
    override def message: String = s"The component ${component.name} is trying to override the component $overriddenName, " +
      s"but ${component.name} is not a subtype of $overriddenName."
  }

  /**
    * Verifies that a component C2 overriding a component C1 is a subtype of C1. Also ensures that C1 even exists.
    */
  def verifyOverrides(definition: ClassDefinition, component: ComponentDefinition)(implicit registry: Registry): Verification = {
    component.overrides.map { overriddenName =>
      // Verify that the overridden component even exists.
      val superComponentNames = definition.supertypeDefinition.map(_.components.map(_.name)).getOrElse(List.empty)
      val exists = if (!superComponentNames.contains(overriddenName)) {
        Compilation.fail(OverriddenComponentDoesNotExist(definition, component))
      } else Verification.succeed

      // Verify that the overriding component is a subtype of the overridden component.
      val subtypes = registry.resolveType(overriddenName, component.position).flatMap { overriddenType =>
        if (!Subtyping.isSubtype(component.tpe, overriddenType)) {
          Compilation.fail(ComponentMustSubtypeOverriddenComponent(definition, component))
        } else Verification.succeed
      }

      (exists, subtypes).simultaneous
    }.toCompiledOption.verification
  }

  case class ConstructorMustEndInContinuation(definition: ClassDefinition, constructor: ConstructorDefinition) extends Error(constructor) {
    override def message = s"The constructor ${constructor.name} of the class ${definition.name} should end in a continuation."
  }

  case class ContinuationCallsAreCyclic(definition: ClassDefinition) extends Error(definition) {
    override def message = s"Constructor calls within the class ${definition.name} are cyclic."
  }

  case class ContinuationsMustEndInConstruct(definition: ClassDefinition, constructor: ConstructorDefinition) extends Error(constructor) {
    override def message = s"The ${definition.name} construction chain starting with the constructor ${constructor.name} must end in a construct call, but doesn't."
  }

  /**
    * Verifies that the constructors end in a continuation, that no continuation appears in any other places, and
    * that continuations are acyclic and end in a construct continuation.
    */
  def verifyContinuations(definition: ClassDefinition): Verification = {
    implicit val fragment: Fragment = definition.position.fragment

    // We check first that all constructors end in a continuation and that no continuation appears in any other places.
    // This is deliberately followed by a flatMap, because we don't want to check the graph parts of this verification
    // if not all continuations are in the right spot.
    val correctPlacement = definition.constructors.map { constructor =>
      val statements = constructor.body.statements
      val endsInContinuation = if (!statements.lastOption.exists(_.isInstanceOf[TopLevelExprNode.ContinuationNode])) {
        Compilation.fail(ConstructorMustEndInContinuation(definition, constructor))
      } else Verification.succeed
      // Visit all the other nodes except the last one (which should be a continuation) and check that they ARE NOT
      // a continuation.
      val noContinuationOtherwise = StmtVisitor.visit(new NoContinuationVisitor()) {
        ExprNode.BlockNode(statements.dropRight(1))
      }
      (endsInContinuation, noContinuationOtherwise).simultaneous
    }.simultaneous

    // Now that placement has been verified, we can check continuation flow.
    val correctFlow = correctPlacement.flatMap { _ =>
      val flowGraph: Graph[String, DiEdge] = Graph()
      implicit val edgeFactory = DiEdge
      val constructName = "!construct"

      // We build a flow graph following constructor calls.
      definition.constructors.foreach { constructor =>
        // The cast is now safe because we have previously verified that the last expression in the block is
        // a continuation.
        val continuation = constructor.body.statements.last.asInstanceOf[TopLevelExprNode.ContinuationNode]
        continuation match {
          case TopLevelExprNode.ConstructorCallNode(name, _) =>
            flowGraph.addEdge(constructor.name, name.getOrElse(definition.name))
          case TopLevelExprNode.ConstructNode(_, _) =>
            flowGraph.addEdge(constructor.name, constructName)
        }
      }

      // Now we first verify that the flow graph is acyclic.
      val isCyclic = if (flowGraph.isCyclic) {
        Compilation.fail(ContinuationCallsAreCyclic(definition))
      } else Verification.succeed

      isCyclic.flatMap { _ =>
        // TODO: Do we even need to verify this? Or do the properties that all constructors have to end in a
        //       continuation and that no cycles may exist suffice to also prove this property?
        // And then we can verify that every call ends in a construct continuation.
        definition.constructors.map { constructor =>
          // We look for the constructor's node in the flow graph and then try to find a construct successor.
          flowGraph.get(constructor.name).findSuccessor(_.value == constructName) match {
            case Some(_) => Verification.succeed
            case None => Compilation.fail(ContinuationsMustEndInConstruct(definition, constructor))
          }
        }.simultaneous
      }
    }

    correctFlow.verification
  }
}
