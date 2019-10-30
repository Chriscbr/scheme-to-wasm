use boilerplate::*;
use im::hashmap::HashMap;

struct Func1 {
    env: HashMap<&'static str, usize>,
}

impl Function1<IntVal, IntVal> for Func1 {
    fn apply(self, x: IntVal) -> IntVal {
        x + IntVal::new(3)
    }
}

fn run(heap: &mut Heap) -> impl HeapVal {
    let val = IntVal::new(5);
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
    let val = ListVal::Cons(IntVal::new(3), Box::from(ListVal::Null));
    let index = heap.alloc(Box::from(val));
    let retrieved_val = heap.get_copy(index);
    let downcasted = as_list::<IntVal>(retrieved_val);
    println!("{}", downcasted);
}
