pub fn matches(name: &crate::processing::ObjectName) -> bool {
    name.ends_with(".c") || name.ends_with(".h")
}
