use std::any::Any;
use std::fmt::Debug;

use tpe::Type;

pub trait Value {
    fn tpe(&self) -> Type;
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

impl Debug for dyn Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self.to_string())
    }
}

#[derive(Debug)]
pub struct IntValue {
    pub value: i64,
}

impl Value for IntValue {
    fn tpe(&self) -> Type {
        Type::Int
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}
