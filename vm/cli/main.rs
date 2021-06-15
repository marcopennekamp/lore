extern crate lore;

use lore::types::*;
use lore::types::are_equal;

fn main() {
    //let sum1 = Type::Sum {  } sum([sum([string, int]), boolean]);
    let sum2 = Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] };
    let sum3 = Type::Sum { parts: vec![Type::String, Type::Int, Type::Boolean] };
    /* const int1 = intersection([string, int, boolean])
    const int2 = intersection([boolean, string, int])
    const int3 = intersection([string, int, boolean])
    const list1 = list(int)
    const list2 = list(int)
    const list3 = list(map(string, int))
    const list4 = list(map(string, int))
    const tuple1 = tuple([sum2, int1, list3])
    const tuple2 = tuple([sum2, int1, list3])
    const tuple3 = tuple([sum([string, int, boolean]), intersection([string, int, boolean]), list(map(string, int))])
    const tuple4 = tuple([sum([string, int, boolean]), intersection([string, int, boolean]), list(map(string, int))]) */

    println!("Are equal? {}", are_equal(&sum2, &sum3));
}
