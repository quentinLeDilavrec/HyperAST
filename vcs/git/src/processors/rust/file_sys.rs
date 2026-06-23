use crate::processing::ObjectName;

pub fn matches(name: &ObjectName) -> bool {
    name.ends_with(".rs")
}
