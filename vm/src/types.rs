pub enum Type {
    TypeVariable { name: String, lower_bound: Box<Type>, upper_bound: Box<Type> },
    Any,
    Nothing,
    Real,
    Int,
    Boolean,
    String,
    Class { name: String },
    Label { name: String },
    Sum { parts: Vec<Type> },
    Intersection { parts: Vec<Type> },
    Tuple { elements: Vec<Type> },
    Component { underlying: Box<Type> },
    List { element: Box<Type> },
    Map { key: Box<Type>, value: Box<Type> },
}

pub fn are_equal(t1: &Type, t2: &Type) -> bool {
    // If the two types are referentially equal, they are obviously the same!
    if t1 as *const Type == t2 as *const Type {
        return true;
    }

    // If the kinds of the types differ, they are trivially not equal.
    // Note: Already covered by the dual match.
    /* if mem::discriminant(t1) != mem::discriminant(t2) {
        return false;
    } */

    match (t1, t2) {
        (Type::Any, Type::Any) => true,
        (Type::Nothing, Type::Nothing) => true,
        (Type::Real, Type::Real) => true,
        (Type::Int, Type::Int) => true,
        (Type::Boolean, Type::Boolean) => true,
        (Type::String, Type::String) => true,
        (Type::Sum { parts: types1 }, Type::Sum { parts: types2 }) => {
            //has_equal_in(types1, types2) && has_equal_in(types2, types1)
            types_exactly_equal(types1, types2)
        }
        (Type::Intersection { parts: types1 }, Type::Intersection { parts: types2 }) => {
            //has_equal_in(types1, types2) && has_equal_in(types2, types1)
            types_exactly_equal(types1, types2)
        }
        (Type::Tuple { elements: types1 }, Type::Tuple { elements: types2 }) => {
            types_exactly_equal(types1, types2)
        },
        (Type::Component { underlying: u1 }, Type::Component { underlying: u2 }) => are_equal(u1, u2),
        (Type::List { element: e1 }, Type::List { element: e2 }) => are_equal(e1, e2),
        (Type::Map { key: k1, value: v1 }, Type::Map { key: k2, value: v2 }) => {
            are_equal(k1, k2) && are_equal(v1, v2)
        },
        _ => false,
    }
}

#[inline]
fn has_equal_in(types1: &Vec<Type>, types2: &Vec<Type>) -> bool {
    for t1 in types1 {
        let mut found = false;
        for t2 in types2 {
            if are_equal(t1, t2) {
                found = true;
            }
        }
        if !found {
            return false;
        }
    }
    true
}

#[inline]
fn types_exactly_equal(types1: &Vec<Type>, types2: &Vec<Type>) -> bool {
    if types1.len() != types2.len() {
        return false;
    }
    for i in 0..types1.len() {
        if !are_equal(&types1[i], &types2[i]) {
            return false;
        }
    }
    true
}
