//! The java scheme,
//! made of packages and modules https://docs.oracle.com/javase/specs/jls/se11/html/jls-7.html

use crate::processing::ObjectName;

pub fn matches(name: &ObjectName) -> bool {
    name.ends_with(".java")
}
