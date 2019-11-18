use im_rc::Vector;

/// Prints a vector as space separated values
pub fn format_vector<T: Clone + std::fmt::Display>(arr: Vector<T>) -> String {
    if arr.is_empty() {
        String::new()
    } else {
        let mut result = String::new();
        for typ in arr {
            result.push_str(format!("{}", typ).as_str());
            result.push_str(" ");
        }
        result.pop();
        result
    }
}
