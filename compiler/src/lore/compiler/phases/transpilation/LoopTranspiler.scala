package lore.compiler.phases.transpilation

import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.RuntimeTypeTranspiler.TranspiledTypeVariables
import lore.compiler.semantics.expressions.Expression.{Extractor, ForLoop, Loop, WhileLoop}
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl._
import lore.compiler.target.{Target, TargetOperator}
import lore.compiler.types.{ListType, MapType, ProductType}

case class LoopTranspiler()(implicit nameProvider: TemporaryNameProvider, typeVariables: TranspiledTypeVariables) {

  def transpile(loop: WhileLoop, condition: Chunk, body: Chunk): Chunk = {
    transpile(loop, body) { body: Vector[TargetStatement] =>
      condition.statements :+ Target.While(condition.expression, Target.Block(body))
    }
  }

  /**
    * Transpile a for-loop. We use direct iteration for this instead of a more succinct forEach call because we need to
    * be able to return from within the loop.
    */
  def transpile(loop: ForLoop, collections: Vector[Chunk], body: Chunk): Chunk = {
    transpile(loop, body)(
      loop.extractors.zip(collections).map {
        case (extractor, collection) => extractor.collection.tpe match {
          case ListType(_) => extractorListShell(extractor, collection) _
          case MapType(_, _) => extractorMapShell(extractor, collection) _
          case _ => throw CompilationException("Currently, only lists and maps can be used as collections in a for loop.")
        }
      }.foldRight(identity: Vector[TargetStatement] => Vector[TargetStatement]) { case (enclose, function) => function.andThen(enclose) }
    )
  }

  private def extractorListShell(extractor: Extractor, collection: Chunk)(inner: Vector[TargetStatement]): Vector[TargetStatement] = {
    val varList = nameProvider.createName().asVariable
    collection.statements ++ Vector(
      varList.declareAs(collection.expression),
      Target.Iteration(
        varList.prop("array"),
        extractor.variable.transpiledName,
        Target.Block(inner)
      )
    )
  }

  private def extractorMapShell(extractor: Extractor, collection: Chunk)(inner: Vector[TargetStatement]): Vector[TargetStatement] = {
    val varIterator = nameProvider.createName().asVariable
    val varNext = nameProvider.createName().asVariable
    val varExtractor = extractor.variable.transpiledName.asVariable
    val callNext = varIterator.prop("next").call()
    collection.statements ++ Vector(
      varIterator.declareAs(RuntimeApi.maps.entries(collection.expression)),
      varNext.declareMutableAs(callNext),
      Target.While(
        TargetOperator.Not(varNext.prop("done")),
        Target.Block(
          Vector(varExtractor.declareAs(varNext.prop("value"))) ++
            inner ++
            Vector(varNext.assign(callNext))
        )
      )
    )
  }

  /**
    * Transpiles a loop, combining the scaffolding of loopShell with an already transpiled body.
    */
  def transpile(loop: Loop, body: Chunk)(loopShell: Vector[TargetStatement] => Vector[TargetStatement]): Chunk = {
    // TODO: We also need to ignore the resulting list if it isn't used as an expression.
    // The loop's inferred type is Unit if its body type is Unit, so this checks out.
    val ignoreResult = loop.tpe == ProductType.UnitType

    def loopCode(result: Option[(Target.Variable, TargetExpression)]) = {
      loopShell(
        body.statements ++ body.meaningfulExpression.map { e =>
          result.map { case (varResult, resultType) =>
            varResult.assign(RuntimeApi.lists.append(varResult, e, resultType))
          }.getOrElse(e)
        }.toVector
      )
    }

    if (ignoreResult) {
      Chunk.unit(loopCode(None): _*)
    } else {
      val varResult = nameProvider.createName().asVariable
      val resultType = RuntimeTypeTranspiler.transpileSubstitute(loop.tpe)
      val resultVarDeclaration = varResult.declareMutableAs(RuntimeApi.lists.value(Vector.empty, resultType))
      Chunk(resultVarDeclaration +: loopCode(Some(varResult, resultType)), varResult)
    }
  }

}
