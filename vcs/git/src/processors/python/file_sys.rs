#[cfg(feature = "python")]
use crate::processing::{CachesHolding, InFiles, ObjectName};

/// The Python scheme,
#[cfg(feature = "python")]
pub struct Python;

#[cfg(feature = "python")]
impl CachesHolding for Python {
    type Caches = super::caches::Python;
}

#[cfg(feature = "python")]
impl InFiles for Python {
    fn matches(name: &ObjectName) -> bool {
        name.ends_with(".py")
    }
}
