package lore.compiler.assembly.optimization

import lore.compiler.poem.{Poem, PoemInstruction, PoemOperation}
import lore.compiler.test.BaseSpec

class RegisterAllocatorSpec extends BaseSpec {

  private implicit class RegisterExtension(val id: Int) {
    def reg: Poem.Register = Poem.Register(id)
  }

  "Register allocation" should "allocate registers correctly for simple arithmetic" in {
    RegisterAllocator.optimize(Vector(
      PoemInstruction.IntConst(2.reg, 5),
      PoemInstruction.BinaryOperation(PoemOperation.IntMul, 3.reg, 1.reg, 2.reg),
      PoemInstruction.BinaryOperation(PoemOperation.IntAdd, 4.reg, 0.reg, 3.reg),
      PoemInstruction.Return(4.reg),
    ), 2) shouldEqual Vector(
      PoemInstruction.IntConst(2.reg, 5),
      PoemInstruction.BinaryOperation(PoemOperation.IntMul, 1.reg, 1.reg, 2.reg),
      PoemInstruction.BinaryOperation(PoemOperation.IntAdd, 0.reg, 0.reg, 1.reg),
      PoemInstruction.Return(0.reg),
    )
  }

  it should "allocate registers correctly for a backwards jump loop" in {
    RegisterAllocator.optimize(Vector(
      PoemInstruction.IntConst(0.reg, 5),
      PoemInstruction.IntConst(1.reg, 10),
      PoemInstruction.BinaryOperation(PoemOperation.IntAdd, 1.reg, 0.reg, 1.reg),
      PoemInstruction.IntConst(2.reg, 20),
      PoemInstruction.BinaryOperation(PoemOperation.IntLt, 3.reg, 1.reg, 2.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(2), 3.reg),
      PoemInstruction.Return(1.reg),
    ), 0) shouldEqual Vector(
      PoemInstruction.IntConst(0.reg, 5),
      PoemInstruction.IntConst(1.reg, 10),
      PoemInstruction.BinaryOperation(PoemOperation.IntAdd, 1.reg, 0.reg, 1.reg),
      PoemInstruction.IntConst(2.reg, 20),
      PoemInstruction.BinaryOperation(PoemOperation.IntLt, 2.reg, 1.reg, 2.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(2), 2.reg),
      PoemInstruction.Return(1.reg),
    )
  }

  it should "allocate registers correctly for a 'use-first, define-later' register" in {
    // This ensures that the liveness intervals are computed correctly even if the first liveness instance is a use,
    // not an assignment.
    RegisterAllocator.optimize(Vector(
      PoemInstruction.IntConst(0.reg, 2),
      PoemInstruction.Jump(Poem.AbsoluteLocation(4)),
      PoemInstruction.BinaryOperation(PoemOperation.IntAdd, 2.reg, 0.reg, 1.reg),
      PoemInstruction.Jump(Poem.AbsoluteLocation(6)),
      PoemInstruction.IntConst(1.reg, 3),
      PoemInstruction.Jump(Poem.AbsoluteLocation(2)),
      PoemInstruction.Return(2.reg),
    ), 0) shouldEqual Vector(
      PoemInstruction.IntConst(2.reg, 2),
      PoemInstruction.Jump(Poem.AbsoluteLocation(4)),
      PoemInstruction.BinaryOperation(PoemOperation.IntAdd, 1.reg, 2.reg, 0.reg),
      PoemInstruction.Jump(Poem.AbsoluteLocation(6)),
      PoemInstruction.IntConst(0.reg, 3),
      PoemInstruction.Jump(Poem.AbsoluteLocation(2)),
      PoemInstruction.Return(1.reg),
    )
  }

  it should "allocate registers correctly for a cond expression" in {
    // func calculate(n: Int): Int = cond
    //   n < 5   => 0
    //   n > 15  => 1
    //   n == 11 => 2
    //   true    => 3
    // end
    RegisterAllocator.optimize(Vector(
      PoemInstruction.IntConst(1.reg, 5),
      PoemInstruction.BinaryOperation(PoemOperation.IntLt, 2.reg, 0.reg, 1.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(5), 2.reg),
      PoemInstruction.IntConst(12.reg, 0),
      PoemInstruction.Jump(Poem.AbsoluteLocation(16)),

      PoemInstruction.IntConst(4.reg, 15),
      PoemInstruction.BinaryOperation(PoemOperation.IntLt, 5.reg, 4.reg, 0.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(10), 5.reg),
      PoemInstruction.IntConst(12.reg, 1),
      PoemInstruction.Jump(Poem.AbsoluteLocation(16)),

      PoemInstruction.IntConst(7.reg, 11),
      PoemInstruction.BinaryOperation(PoemOperation.IntEq, 8.reg, 0.reg, 7.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(15), 8.reg),
      PoemInstruction.IntConst(12.reg, 2),
      PoemInstruction.Jump(Poem.AbsoluteLocation(16)),

      PoemInstruction.IntConst(12.reg, 3),

      PoemInstruction.Return(12.reg),
    ), 1) shouldEqual Vector(
      PoemInstruction.IntConst(1.reg, 5),
      PoemInstruction.BinaryOperation(PoemOperation.IntLt, 1.reg, 0.reg, 1.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(5), 1.reg),
      PoemInstruction.IntConst(1.reg, 0),
      PoemInstruction.Jump(Poem.AbsoluteLocation(16)),

      PoemInstruction.IntConst(2.reg, 15),
      PoemInstruction.BinaryOperation(PoemOperation.IntLt, 2.reg, 2.reg, 0.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(10), 2.reg),
      PoemInstruction.IntConst(1.reg, 1),
      PoemInstruction.Jump(Poem.AbsoluteLocation(16)),

      PoemInstruction.IntConst(2.reg, 11),
      PoemInstruction.BinaryOperation(PoemOperation.IntEq, 0.reg, 0.reg, 2.reg),
      PoemInstruction.JumpIfFalse(Poem.AbsoluteLocation(15), 0.reg),
      PoemInstruction.IntConst(1.reg, 2),
      PoemInstruction.Jump(Poem.AbsoluteLocation(16)),

      PoemInstruction.IntConst(1.reg, 3),

      PoemInstruction.Return(1.reg),
    )
  }

}
