extern crate lore;

use lore::evaluator::evaluate;
use lore::instruction::Instruction;
use lore::instruction::Operation;

fn main() {
    let result = evaluate(&vec![
        Instruction { operation: Operation::IntPush, arguments: [1, 0] },
        Instruction { operation: Operation::IntPush, arguments: [2, 0] },
        Instruction { operation: Operation::IntAdd, arguments: [0, 0] },
        Instruction { operation: Operation::Return, arguments: [0, 0] },
    ], 0);

    match result {
        None => {
            println!("Could not execute!");
        }
        Some(result) => unsafe {
            println!("{:?}", (*result).to_string());
        }
    }
}
