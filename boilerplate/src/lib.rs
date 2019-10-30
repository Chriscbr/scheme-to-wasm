use std::any::Any;
use std::cmp;
use std::fmt::Display;
use std::ops;

// TODO: consider changing this to return a Cow<'a, str>
// see: https://stackoverflow.com/a/51332822
pub trait DisplayType {
    fn fmt_type() -> String
    where
        Self: Sized;
}

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
pub trait HeapVal: HValClone + Display + DisplayType {
    fn as_any(&self) -> &dyn Any;
    // TODO: consider changing these to just be global functions?
    fn as_int(&self) -> IntVal;
    fn as_bool(&self) -> BoolVal;
    fn as_str(&self) -> StrVal;
    // TODO: implement `as_list`.
    // This is not currently possible using the current ListVal structure,
    // since ListVal is parameterized, and hence this "as_list" function
    // would need to be generic, or need to return a generic ListVal.
    // It can't be generic, since then HeapVal would become

    // fn as_list(&self) -> ListVal<dyn HeapVal>;
}

impl<T: Display + DisplayType + Clone + 'static> HeapVal for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_int(&self) -> IntVal {
        match self.as_any().downcast_ref::<IntVal>() {
            Some(x) => x.clone(),
            None => panic!("Called as_int on non-IntVal value."),
        }
    }

    fn as_bool(&self) -> BoolVal {
        match self.as_any().downcast_ref::<BoolVal>() {
            Some(x) => x.clone(),
            None => panic!("Called as_bool on non-BoolVal value."),
        }
    }

    fn as_str(&self) -> StrVal {
        match self.as_any().downcast_ref::<StrVal>() {
            Some(x) => x.clone(),
            None => panic!("Called as_str on non-StrVal value."),
        }
    }
}

pub fn as_list<U: HeapVal + Clone + 'static>(value: Box<dyn HeapVal>) -> ListVal<U> {
    match (*value).as_any().downcast_ref::<ListVal<U>>() {
        Some(x) => x.clone(),
        None => panic!("Called as_list on non-ListVal value."),
    }
}

#[derive(Clone, Eq)]
pub struct IntVal {
    value: i64,
}

impl DisplayType for IntVal {
    fn fmt_type() -> String {
        String::from("int")
    }
}

impl IntVal {
    pub fn new(value: i64) -> IntVal {
        IntVal { value }
    }

    pub fn get_value(&self) -> i64 {
        self.value
    }
}

impl Display for IntVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Ord for IntVal {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.value.cmp(&other.get_value())
    }
}

impl PartialOrd for IntVal {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for IntVal {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.get_value()
    }
}

impl ops::Add<IntVal> for IntVal {
    type Output = IntVal;

    fn add(self, rhs: IntVal) -> IntVal {
        IntVal::new(self.value + rhs.get_value())
    }
}

impl ops::Sub<IntVal> for IntVal {
    type Output = IntVal;

    fn sub(self, rhs: IntVal) -> IntVal {
        IntVal::new(self.value - rhs.get_value())
    }
}

impl ops::Mul<IntVal> for IntVal {
    type Output = IntVal;

    fn mul(self, rhs: IntVal) -> IntVal {
        IntVal::new(self.value * rhs.get_value())
    }
}

impl ops::Div<IntVal> for IntVal {
    type Output = IntVal;

    fn div(self, rhs: IntVal) -> IntVal {
        IntVal::new(self.value / rhs.get_value())
    }
}

#[derive(Clone)]
pub struct BoolVal {
    value: bool,
}

impl DisplayType for BoolVal {
    fn fmt_type() -> String {
        String::from("bool")
    }
}

impl BoolVal {
    pub fn new(value: bool) -> BoolVal {
        BoolVal { value }
    }

    pub fn get_value(&self) -> bool {
        self.value
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

impl DisplayType for StrVal {
    fn fmt_type() -> String {
        String::from("string")
    }
}

impl StrVal {
    pub fn new(value: String) -> StrVal {
        StrVal { value }
    }

    pub fn get_value(&self) -> String {
        self.value.clone()
    }
}

impl Display for StrVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Clone)]
pub enum ListVal<T: HeapVal + Clone> {
    Null,
    Cons(T, Box<ListVal<T>>),
}

impl<T: HeapVal + Clone> DisplayType for ListVal<T> {
    fn fmt_type() -> String {
        format!("(list {})", T::fmt_type().as_str())
    }
}

impl<T: HeapVal + Clone> ListVal<T> {
    pub fn get_car(&self) -> T {
        if let &ListVal::Cons(car, cdr) = &self {
            car.clone()
        } else {
            panic!("Tried calling get_car on Null value.")
        }
    }

    pub fn get_cdr(&self) -> ListVal<T> {
        if let ListVal::Cons(car, cdr) = self {
            *cdr.clone()
        } else {
            panic!("Tried calling get_cdr on Null value.")
        }
    }

    pub fn is_null(&self) -> BoolVal {
        match self {
            ListVal::Cons(_, _) => BoolVal::new(false),
            ListVal::Null => BoolVal::new(true),
        }
    }
}

impl<T: HeapVal + Clone> Display for ListVal<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let ListVal::Cons(car, cdr) = self {
            write!(f, "(cons {} {})", car, *cdr)
        } else {
            write!(f, "(null {})", T::fmt_type().as_str())
        }
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

    pub fn get_copy(&self, i: usize) -> Box<dyn HeapVal> {
        self.mem[i].hval_clone()
    }

    pub fn set(&mut self, i: usize, v: Box<dyn HeapVal>) {
        self.mem[i] = v;
    }

    pub fn copy(&mut self, into: usize, from: usize) {
        self.mem[into] = self.mem[from].hval_clone();
    }
}

pub fn concat(a: StrVal, b: StrVal) -> StrVal {
    let a_val = a.get_value();
    let b_val = b.get_value();
    StrVal::new(format!("{}{}", a_val, b_val))
}
