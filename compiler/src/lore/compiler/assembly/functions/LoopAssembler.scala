package lore.compiler.assembly.functions

import lore.compiler.assembly.types.TypeAssembler
import lore.compiler.assembly.{Chunk, RegisterProvider}
import lore.compiler.poem.{Poem, PoemInstruction, PoemOperation}
import lore.compiler.semantics.expressions.Expression

object LoopAssembler {

  def generate(
    loop: Expression.WhileLoop,
    conditionChunk: Chunk,
    bodyChunk: Chunk,
  )(implicit registerProvider: RegisterProvider): Chunk = {
    val (regResult, initializationChunk) = generateResultInitialization(loop)

    // We need two labels. One label for jumping from the body back to the condition for the next iteration, and one
    // label to skip past the body.
    val conditionLabel = new Poem.Label(loop.condition.position)
    val postBodyLabel = new Poem.Label(loop.body.position.end)

    val checkConditionChunk = Chunk(
      conditionChunk.instructions,
      PoemInstruction.JumpIfFalse(
        Poem.LabelLocation(postBodyLabel),
        conditionChunk.forceResult(loop.condition.position)
      ),
    )
    checkConditionChunk.labelFirst(conditionLabel)

    val fullBodyChunk = (
      bodyChunk ++ generateResultAppend(loop, regResult, bodyChunk) ++ Chunk(
        // We have to jump back to the beginning of the loop.
        PoemInstruction.Jump(Poem.LabelLocation(conditionLabel)),
      )
    ).withPostLabel(postBodyLabel)

    initializationChunk ++ checkConditionChunk ++ fullBodyChunk ++ generateResultGet(regResult)
  }

  /**
    * Generates instructions for a for-loop.
    *
    * Example:
    *
    * {{{
    * for x <- xs, y <- ys
    *   x + y
    * end
    * }}}
    *
    * The following instructions are generated from the above code:
    *
    * {{{
    * ----- initialization -----
    * List reg(result) <tpe>
    *
    * ----- pre-body 1 -----
    * IntConst reg(x_i) 0
    * ListLength reg(x_len) reg(xs)
    * loop_1: IntLt reg(flag) reg(x_i) reg(x_len)
    * JumpIfFalse reg(flag) post_loop
    * ListGet reg(x) reg(xs) reg(x_i)
    *
    * ----- pre-body 2 -----
    * IntConst reg(y_i) 0
    * ListLength reg(y_len) reg(ys)
    * loop_2: IntLt reg(flag) reg(y_i) reg(y_len)
    * JumpIfFalse reg(flag) post_loop_1
    * ListGet reg(y) reg(ys) reg(y_i)
    *
    * ----- body -----
    * IntAdd reg(xy) reg(x) reg(y)
    * ListAppendUntyped reg(result) reg(result) reg(xy)
    *
    * ----- post-body 2 -----
    * post_loop_2: IntAdd reg(y_i) reg(y_i) 1
    * Jump loop_2
    *
    * ----- post-body 1 -----
    * post_loop_1: IntAdd reg(x_i) reg(x_i) 1
    * Jump loop_1
    * post_loop: ...
    * }}}
    */
  def generate(
    loop: Expression.ForLoop,
    collectionChunks: Vector[Chunk],
    bodyChunk: Chunk,
  )(implicit registerProvider: RegisterProvider, variableRegisterMap: VariableRegisterMap): Chunk = {
    val (regResult, initializationChunk) = generateResultInitialization(loop)

    // We have to support multiple extractors. This requires us to build nested loops recursively. Each extractor part
    // has a "pre-body" and a "post-body". The pre-body part initializes the registers for the index `i` and list
    // length `len`, checks `i < len` and jumps to the OUTER loop's post-body if it's false, and initializes the
    // register for the extracted element. The post-body increments `i` and jumps again to the checking portion of its
    // own pre-body. (See also the example above, which shows generated instructions for a nested for loop.)
    //
    // Because the pre-body check must jump to the outer loop's post-body (to start the next iteration of the outer
    // loop), we have to pass the outer loop's post-body label to the nested loop.
    //
    // Because passing outer labels is left-to-right and building the nested loop is right-to-left, we're building up
    // ExtractorParts first from left-to-right and then build up the whole loop from right-to-left.
    case class ExtractorPart(preBodyChunk: Chunk, postBodyChunk: Chunk)

    // This label will point to an instruction after the whole `loop` expression.
    val postExpressionLabel = new Poem.Label(loop.position.end)

    var extractorParts = Vector.empty[ExtractorPart]
    loop.extractors.zip(collectionChunks).foldLeft(postExpressionLabel) {
      case (outerLoopLabel, (expressionExtractor, collectionChunk)) =>
        val checkLabel = new Poem.Label(expressionExtractor.collection.position)

        // This is NOT a "post label".
        val postBodyLabel = new Poem.Label(loop.body.position)

        val regList = collectionChunk.forceResult(loop.position)
        val regIndex = registerProvider.fresh()
        val regListLength = registerProvider.fresh()
        val regFlag = registerProvider.fresh()

        // The element variable is pre-registered in the ExpressionAssembler so that the body chunk can use it.
        val regElement = variableRegisterMap(expressionExtractor.variable.uniqueKey)

        // We have to add the collection chunk to the initialization of its corresponding loop. Say we have a loop
        // with two extractors, where the inner collection depends on the outer collection, for example:
        //
        //    for xs <- xss, x <- map(xs, foo) yield x
        //
        // Every time the `x <- map(xs, foo)` loop is initialized, we have to apply `map(foo)` to `xs`. This requires
        // the inner loop's initializer to include the collection chunk.
        // TODO (assembly): Write a test that checks that `for xs <- xss, x <- map(xs, foo) yield x` indeed works as a
        //                  flatten with a map on each `xs`.
        val preBodyChunk = collectionChunk ++ Chunk(
          PoemInstruction.IntConst(regIndex, 0),
          PoemInstruction.ListLength(regListLength, regList),
          PoemInstruction.BinaryOperation(PoemOperation.IntLt, regFlag, regIndex, regListLength).withLabel(checkLabel),
          PoemInstruction.JumpIfFalse(Poem.LabelLocation(outerLoopLabel), regFlag),
          PoemInstruction.ListGet(regElement, regList, regIndex),
        )

        val regConst1 = registerProvider.fresh()
        val postBodyChunk = Chunk(
          PoemInstruction.IntConst(regConst1, 1).withLabel(postBodyLabel),
          PoemInstruction.BinaryOperation(PoemOperation.IntAdd, regIndex, regIndex, regConst1),
          PoemInstruction.Jump(Poem.LabelLocation(checkLabel)),
        )

        extractorParts = extractorParts :+ ExtractorPart(preBodyChunk, postBodyChunk)
        postBodyLabel
    }

    val fullBodyChunk = bodyChunk ++ generateResultAppend(loop, regResult, bodyChunk)

    val fullLoopChunk = extractorParts.foldRight(fullBodyChunk) { case (extractorPart, innerChunk) =>
      extractorPart.preBodyChunk ++ innerChunk ++ extractorPart.postBodyChunk
    }
    (initializationChunk ++ fullLoopChunk ++ generateResultGet(regResult)).withPostLabel(postExpressionLabel)
  }

  private def generateResultInitialization(loop: Expression.Loop)(implicit registerProvider: RegisterProvider): (Option[Poem.Register], Chunk) = {
    // We only need to build a result list if `loop` is used.
    if (loop.isUsed) {
      val regResult = registerProvider.fresh()
      val chunk = Chunk(PoemInstruction.List(regResult, TypeAssembler.generate(loop.tpe), Vector.empty))
      (Some(regResult), chunk)
    } else {
      println(s"Loop unused at ${loop.position}.")
      (None, Chunk.empty)
    }
  }

  private def generateResultAppend(loop: Expression.Loop, regResult: Option[Poem.Register], bodyChunk: Chunk): Chunk = {
    regResult match {
      case Some(regResult) => Chunk(
        PoemInstruction.ListAppendUntyped(regResult, regResult, bodyChunk.forceResult(loop.body.position)),
      )
      case None => Chunk.empty
    }
  }

  private def generateResultGet(regResult: Option[Poem.Register]): Chunk = regResult match {
    case Some(regResult) => Chunk(regResult)
    case None => Chunk.empty
  }

}
