package lore.compiler.constraints

import lore.compiler.feedback.{Reporter, SchemaFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.types.TypeVariable.Variance
import lore.compiler.types.{DeclaredSchema, StructSchema, TraitType}

object DeclaredSchemaConstraints {

  /**
    * Verifies:
    *   1. Co-/contra-/invariant type parameters must be used in appropriate positions in extended types.
    *   2. Supertrait invariance consistency: All types assigned to an invariant type parameter `A` of a supertrait `X`
    *      are equal.
    *   3. All struct constraints if the given definition is a struct.
    */
  def verify(schema: DeclaredSchema)(implicit registry: Registry, reporter: Reporter): Unit = {
    verifyVariancePositions(schema)
    verifySupertraitInvarianceConsistency(schema)
    schema match {
      case schema: StructSchema => StructConstraints.verify(schema)
      case _ =>
    }
  }

  /**
    * Verifies that co-/contra-/invariant type parameters are used in appropriate positions in extended types.
    */
  private def verifyVariancePositions(schema: DeclaredSchema)(implicit reporter: Reporter): Unit = {
    schema.supertypes.foreach {
      supertype => VarianceConstraints.verifyVariance(supertype, Variance.Covariant, schema.position)
    }
  }

  /**
    * Verifies that all types assigned to an invariant type parameter `A` of a supertrait `X` are equal.
    *
    * For example, if a struct `Z` extends both `X[Int]` and `X[Real]` directly or indirectly, and the type parameter
    * `A` is invariant, `A` isn't assigned to consistently, as `Int` and `Real` aren't equal.
    */
  private def verifySupertraitInvarianceConsistency(schema: DeclaredSchema)(implicit reporter: Reporter): Unit = {
    val supertraitsBySchema = schema.indirectDeclaredSupertypes.filter {
      // This filter combines a type filter on TraitType with a check that the supertrait schema even has invariant
      // type parameters. Traits without invariant type parameters can be ignored.
      case supertrait: TraitType => supertrait.schema.hasInvariantTypeParameters
      case _ => false
    }.asInstanceOf[Set[TraitType]].groupBy(_.schema)

    supertraitsBySchema.foreach {
      case (supertraitSchema, traitTypes) if traitTypes.size > 1 =>
        val invariantTypeParameters = supertraitSchema.typeParameters.filter(_.variance == Variance.Invariant)
        invariantTypeParameters.foreach { typeParameter =>
          val typeArguments = traitTypes.map(_.assignments(typeParameter))
          if (typeArguments.size > 1) {
            reporter.error(
              SchemaFeedback.SupertraitInvarianceInconsistent(
                schema,
                supertraitSchema,
                typeParameter,
                typeArguments.toVector,
              )
            )
          }
        }

      case _ =>
    }
  }

}
