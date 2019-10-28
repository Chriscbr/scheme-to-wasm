use boilerplate::*;
use im::hashmap::HashMap;

struct Func1 {
    env: HashMap<&'static str, usize>,
}

impl Function1<IntVal, IntVal> for Func1 {
    fn apply(self, x: IntVal) -> IntVal {
        x + IntVal::from(3)
    }
}

fn run(heap: &mut Heap) -> impl HeapVal {
    let val = IntVal::from(5);
    let func1 = Func1 {
        env: HashMap::new(),
    };
    func1.apply(val)
}

fn main() {
    let mut heap = Heap::new();
    let result = run(&mut heap);
    println!("{}", result);
}
