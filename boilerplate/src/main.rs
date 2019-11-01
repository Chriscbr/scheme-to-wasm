#![feature(type_alias_impl_trait)]
#![feature(associated_type_defaults)]

use boilerplate::*;
use std::fmt::Display;

#[derive(Clone)]
pub struct Record0 {
    pub num: IntVal,
}

pub trait Record0Trait {}
impl Record0Trait for Record0 {}

impl DisplayType for Record0 {
    fn fmt_type() -> String {
        String::from("(record (num : int))")
    }
}

impl Display for Record0 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(make-record (num {}))", self.num)
    }
}

#[derive(Clone)]
struct Func1 {}

impl Function2<Record0, IntVal, IntVal> for Func1 {
    fn apply(&self, env: Record0, x: IntVal) -> IntVal {
        x + env.num
    }
}

// This way of representing a packed tuple won't work
// because it allows the outer scope to see the specific type of T2
// (recall that in Rust, this struct will get monomorphized)
struct PackedTuple0<T2>(Box<dyn Function2<T2, IntVal, IntVal>>, T2);

// trait PackedTuple1Trait {
//     type T2;

//     fn get_0(&self) -> Box<dyn Function2<Self::T2, IntVal, IntVal>>;
//     fn get_1(&self) -> Self::T2;
// }

// struct PackedTuple1(Box<dyn Function2<Record0, IntVal, IntVal>>, Record0);

// impl PackedTuple1Trait for PackedTuple1 {
//     type T2 = impl Record0Trait;
//     fn get_0(&self) -> Box<dyn Function2<Self::T2, IntVal, IntVal>> {
//         self.0.clone()
//     }
//     fn get_1(&self) -> Self::T2 {
//         self.1
//     }
// }

fn main() {
    let mut heap = Heap::new();

    // try storing and retrieving a list value
    let val = ListVal::Cons(IntVal(3), Box::from(ListVal::Null));
    let index = heap.alloc(Box::from(val));
    let retrieved_val = heap.get_copy(index);
    let downcasted = as_list::<IntVal>(retrieved_val);
    println!("{}", downcasted);

    let val = IntVal(7);
    let index = heap.alloc(Box::from(val));
    let retrieved_val = heap.get_copy(index);
    let downcasted = as_int(retrieved_val);
    println!("{}", downcasted);

    let int_val = IntVal(4);
    let rec_val = Record0 { num: int_val };
    println!("{}", rec_val.num);

    let y = IntVal(4);
    let func1 = Func1 {};
    let rec = Record0 { num: y.clone() };
    let tup = PackedTuple0(Box::from(func1), rec);
    // let out = tup.1.num; // this should be an error, but it is not...
    let out = tup.0.apply(tup.1, IntVal(3));
    println!("{}", out);
}
