use crate::processing::ObjectName;

pub fn matches(name: &ObjectName) -> bool {
    name.ends_with(".ts") || name.ends_with(".d.ts") //|| name.ends_with(".tsx")
}
