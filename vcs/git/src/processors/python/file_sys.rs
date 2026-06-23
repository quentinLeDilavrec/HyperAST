use crate::processing::ObjectName;

pub fn matches(name: &ObjectName) -> bool {
    name.ends_with(".py") || name.ends_with(".pyi")
}
