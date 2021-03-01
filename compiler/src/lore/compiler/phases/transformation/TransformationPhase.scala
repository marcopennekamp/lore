package lore.compiler.phases.transformation

import lore.compiler.core.Compilation._
import lore.compiler.phases.typing.InferringExpressionTransformationVisitor
import lore.compiler.phases.typing.inference.InferenceResolution
import lore.compiler.semantics.Registry
import lore.compiler.semantics.scopes.FunctionVariableScope
import lore.compiler.syntax.visitor.TopLevelExprVisitor
import lore.compiler.types.StructType
import lore.compiler.utils.CollectionExtensions.VectorExtension

object TransformationPhase {
  def process(implicit registry: Registry): Verification = {
    registry.getMultiFunctions.values.toVector.map { mf =>
      mf.functions.map { f =>
        f.bodyNode.map { body =>
          val visitor = new InferringExpressionTransformationVisitor(
            f.signature.outputType,
            f.typeScope,
            new FunctionVariableScope(f.signature, registry.variableScope)
          )
          val result = TopLevelExprVisitor.visitCompilation(visitor)(body)

          println(s"Typing judgments for function $f:")
          visitor.typingJudgments.foreach(println)
          println()

          println("Inferred types:")
          println(InferenceResolution.infer(visitor.typingJudgments))
          println()
          println()

          result
        }.toCompiledOption
      }.simultaneous
    }.simultaneous

    val withVerifiedConstraints = (
      registry.getTypeDefinitions.values.toVector.map(DeclaredTypeConstraints.verify).simultaneous,
      registry.getMultiFunctions.values.toVector.map(MultiFunctionConstraints.verify).simultaneous,
    ).simultaneous

    val withTransformedStructs = withVerifiedConstraints.flatMap { _ =>
      registry.getTypeDeclarationsInOrder.map(_._2).filterType[StructType].map { structType =>
        StructTransformer.transform(structType.definition)
      }.simultaneous
    }

    val withTransformedFunctions = withTransformedStructs.flatMap { _ =>
      registry.getMultiFunctions.values.toVector.map { mf =>
        mf.functions.map(FunctionTransformer.transform).simultaneous
      }.simultaneous
    }

    withTransformedFunctions.verification
  }
}
