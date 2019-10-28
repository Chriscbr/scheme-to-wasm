use std::any::Any;
use std::fmt::Display;
use std::ops;

pub trait HValClone {
    fn hval_clone(&self) -> Box<dyn HeapVal>;
}

impl<T: 'static> HValClone for T
where
    T: HeapVal + Clone,
{
    fn hval_clone(&self) -> Box<dyn HeapVal> {
        Box::new(self.clone())
    }
}

/// A trait required by all values that are put on the virtual heap.
/// It will be implemented automatically as long as you implement `Clone`
/// (or use the `#[derive(Clone)]` macro).
///
/// What it does:
///
/// This trait guarantees that there is a mechanism by which values can be
/// cloned on the heap. In particular, trait objects are not allowed
/// to implement the `Clone` or `Sized` traits. See the following references
/// for more information:
/// - https://doc.rust-lang.org/1.30.0/book/2018-edition/ch17-02-trait-objects.html?highlight=trait,object#object-safety-is-required-for-trait-objects
/// - https://stackoverflow.com/questions/51822118/why-a-function-on-a-trait-object-cannot-be-called-when-bounded-with-self-sized
///
/// To work around this, `HeapVal` objects implement the `HValClone` trait,
/// which guarantees that all `HeapVal` objects can return _boxed_ cloned
/// versions of themselves. Returning boxed clones is sufficient for
/// manipulating the various objects.
///
/// Furthermore, this trait requires there to be an `as_any` method. This
/// allows values on the Heap to be unwrapped as `Any` objects, and then
/// safely dereferenced using `Any::downcast_ref`. This is done because it
/// is not possible to cast _any_ object to `Any`.
pub trait HeapVal: HValClone + Display {
    fn as_any(&self) -> &dyn Any;
    fn as_int(&self) -> IntVal;
    fn as_bool(&self) -> BoolVal;
    fn as_str(&self) -> StrVal;
}

impl<T: Display + Clone + 'static> HeapVal for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_int(&self) -> IntVal {
        match self.as_any().downcast_ref::<IntVal>() {
            Some(x) => x.clone(),
            None => panic!(),
        }
    }

    fn as_bool(&self) -> BoolVal {
        match self.as_any().downcast_ref::<BoolVal>() {
            Some(x) => x.clone(),
            None => panic!(),
        }
    }

    fn as_str(&self) -> StrVal {
        match self.as_any().downcast_ref::<StrVal>() {
            Some(x) => x.clone(),
            None => panic!(),
        }
    }
}

#[derive(Clone)]
pub struct IntVal {
    value: i64,
}

impl IntVal {
    pub fn get_value(&self) -> i64 {
        self.value
    }
}

impl From<i64> for IntVal {
    fn from(value: i64) -> IntVal {
        IntVal { value }
    }
}

impl Display for IntVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl ops::Add<IntVal> for IntVal {
    type Output = IntVal;

    fn add(self, rhs: IntVal) -> IntVal {
        IntVal::from(self.value + rhs.get_value())
    }
}

#[derive(Clone)]
pub struct BoolVal {
    value: bool,
}

impl BoolVal {
    pub fn get_value(&self) -> bool {
        self.value
    }
}

impl From<bool> for BoolVal {
    fn from(value: bool) -> BoolVal {
        BoolVal { value }
    }
}

impl Display for BoolVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone)]
pub struct StrVal {
    value: String,
}

impl StrVal {
    pub fn get_value(&self) -> String {
        self.value.clone()
    }
}

impl Display for StrVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<String> for StrVal {
    fn from(value: String) -> StrVal {
        StrVal { value }
    }
}

// The trait for all 0-arg functions
pub trait Function0<R> {
    fn apply(self) -> R;
}

// The trait for all 1-arg functions
pub trait Function1<X, R> {
    fn apply(self, x: X) -> R;
}

// The trait for all 2-arg functions
pub trait Function2<X, Y, R> {
    fn apply(self, x: X, y: Y) -> R;
}

// The trait for all 3-arg functions
pub trait Function3<X, Y, Z, R> {
    fn apply(self, x: X, y: Y, z: Z) -> R;
}

#[derive(Default)]
pub struct Heap {
    pub mem: Vec<Box<dyn HeapVal>>,
}

impl Heap {
    pub fn new() -> Self {
        Heap { mem: vec![] }
    }

    pub fn alloc(&mut self, v: Box<dyn HeapVal>) -> usize {
        self.mem.push(v);
        self.mem.len() - 1
    }

    pub fn get(&self, i: usize) -> &dyn HeapVal {
        &*self.mem[i]
    }

    pub fn set(&mut self, i: usize, v: Box<dyn HeapVal>) {
        self.mem[i] = v;
    }

    pub fn copy(&mut self, into: usize, from: usize) {
        self.mem[into] = self.mem[from].hval_clone();
    }
}

pub fn and_bool(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_bool().get_value();
    let b_val = heap.get(b).as_bool().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val && b_val)))
}

pub fn or_bool(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_bool().get_value();
    let b_val = heap.get(b).as_bool().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val || b_val)))
}

pub fn add_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(IntVal::from(a_val + b_val)))
}

pub fn sub_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(IntVal::from(a_val - b_val)))
}

pub fn mul_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(IntVal::from(a_val * b_val)))
}

pub fn div_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(IntVal::from(a_val / b_val)))
}

pub fn gt_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val > b_val)))
}

pub fn lt_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val < b_val)))
}

pub fn eq_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val == b_val)))
}

pub fn geq_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val >= b_val)))
}

pub fn leq_int(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_int().get_value();
    let b_val = heap.get(b).as_int().get_value();
    heap.alloc(Box::from(BoolVal::from(a_val <= b_val)))
}

pub fn concat_str(a: usize, b: usize, heap: &mut Heap) -> usize {
    let a_val = heap.get(a).as_str().get_value();
    let b_val = heap.get(b).as_str().get_value();
    heap.alloc(Box::from(StrVal::from(format!("{}{}", a_val, b_val))))
}
