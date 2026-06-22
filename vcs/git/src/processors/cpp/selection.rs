/// CAUTION about when you change this value,
/// advice: change it only at the very beginning
#[doc(hidden)]
pub static mut ONLY_SWITCHES: bool = false;

pub fn matches(name: &crate::processing::ObjectName) -> bool {
    if unsafe { ONLY_SWITCHES } {
        name.ends_with("switches.h") || name.ends_with("switches.cc")
    } else {
        name.ends_with(".cpp")
            || name.ends_with(".c")
            || name.ends_with(".cc")
            || name.ends_with(".cxx")
            || name.ends_with(".h")
            || name.ends_with(".hpp")
    }
}
