extern crate smallvec;

use std::alloc::alloc;
use self::smallvec::SmallVec;

use instruction::{Instruction, Operation};
use tpe::Type;
use value::{IntValue, Value};

pub fn evaluate(instructions: &Vec<Instruction>, frame_size: usize) -> Option<*mut dyn Value> {
    let mut index = 0;
    let mut instruction;
    /* let mut frame = SmallVec::<[Box<Value>; 16]>::new();
    frame.grow(frame_size);
    let mut stack = SmallVec::<[Box<Value>; 16]>::new(); */
    let mut stack: Vec<*mut dyn Value> = vec![];
    let mut heap: Vec<*mut dyn Value> = vec![];
    
    'main_loop: loop {
        instruction = unsafe { instructions.get_unchecked(index) };
        index += 1;

        match instruction.operation {
            Operation::IntAdd => {
                let stack_length = stack.len();
                let v2 = stack.remove(stack_length - 1);
                let v1 = stack.remove(stack_length - 2);
                unsafe {
                    let r2 = v2 as *mut IntValue;
                    let r1 = v1 as *mut IntValue;
                    let result = (*r1).value + (*r2).value;
                    stack.push(allocate(&mut heap, Box::new(IntValue { value: result })));
                };
            }

            Operation::IntPush => {
                let value = instruction.arguments[0] as i64;
                unsafe {
                    stack.push(allocate(&mut heap, Box::new(IntValue { value })));
                }
            }

            Operation::Return => {
                break 'main_loop
            }
        }
    }

    for ptr in heap {
        unsafe {
            // This frees the pointer.
            Box::from_raw(ptr);
        }
    }

    stack.pop()
}

unsafe fn allocate(heap: &mut Vec<*mut dyn Value>, value: Box<dyn Value>) -> *mut dyn Value {
    let ptr = Box::into_raw(value);
    heap.push(ptr);
    ptr
}
