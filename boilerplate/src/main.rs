use boilerplate::*;
use im::hashmap::HashMap;

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
}
