#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Operation {
    IntAdd,
    IntPush,
    Return,
}

pub struct Instruction {
    pub operation: Operation,
    pub arguments: [u16; 2],
}
