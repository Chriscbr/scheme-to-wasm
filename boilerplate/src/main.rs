use boilerplate::*;
use im::hashmap::HashMap;
use std::fmt::Display;

// ex. (record-ref (make-record (num 3) (name "hello")) num)

#[derive(Clone)]
pub struct Record0 {
    pub num: IntVal,
    pub name: StrVal,
}

impl DisplayType for Record0 {
    fn fmt_type() -> String {
        String::from("(record (num int) (name string))")
    }
}

impl Display for Record0 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(record (num {}) (name {}))", self.num, self.name)
    }
}

struct Func1 {
    env: HashMap<&'static str, usize>,
}

impl Function1<IntVal, IntVal> for Func1 {
    fn apply(self, x: IntVal) -> IntVal {
        x + IntVal(3)
    }
}

fn run(heap: &mut Heap) -> impl HeapVal {
    let val = IntVal(5);
    let func1 = Func1 {
        env: HashMap::new(),
    };
    let out = func1.apply(val);
    let nil = ListVal::Null;
    let pair = ListVal::Cons(out, Box::new(nil));
    pair
}

fn main() {
    let mut heap = Heap::new();

    // try storing and retrieving a list value
    let val = ListVal::Cons(IntVal(3), Box::from(ListVal::Null));
    let index = heap.alloc(Box::from(val));
    let retrieved_val = heap.get_copy(index);
    let downcasted = as_list::<IntVal>(retrieved_val);
    println!("{}", downcasted);

    let val = IntVal(3);
    let index = heap.alloc(Box::from(val));
    let retrieved_val = heap.get_copy(index);
    let downcasted = as_int(retrieved_val);
    println!("{}", downcasted);

    let int_val = IntVal(3);
    let str_val = StrVal(String::from("hello"));
    let rec_val = Record0 {
        num: int_val,
        name: str_val,
    };
    println!("{}", rec_val.name);
}
