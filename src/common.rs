use im_rc::Vector;

#[derive(Default, Debug)]
pub struct TypeEnv<T: Clone> {
    bindings: Vector<(String, T)>,
}

// New values are appended to the front of the frame
impl<T: Clone> TypeEnv<T> {
    pub fn new() -> Self {
        TypeEnv {
            bindings: Vector::new(),
        }
    }

    /// Returns a new environment extended with the provided binding.
    pub fn add_binding(&self, new_binding: (String, T)) -> TypeEnv<T> {
        let mut bindings = self.bindings.clone();
        bindings.push_front(new_binding);
        TypeEnv { bindings }
    }

    /// Returns a new environment extended with the provided bindings.
    pub fn add_bindings(&self, new_bindings: Vector<(String, T)>) -> TypeEnv<T> {
        let mut bindings = self.bindings.clone();
        for binding in new_bindings {
            bindings.push_front(binding);
        }
        TypeEnv { bindings }
    }

    pub fn find(&self, key: &str) -> Option<&T> {
        for pair in self.bindings.iter() {
            if pair.0 == key {
                return Some(&pair.1);
            }
        }
        None
    }
}
