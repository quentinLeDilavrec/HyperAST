// TODO move these things to their respective modules
use super::{CachesHolding, ObjectName};

// /// The default file system, directories and files
// pub struct Any;

/// The maven scheme https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html ,
/// made of nested maven modules.
/// Each maven module has a config file (often a pom.xml),
/// a src/main/java/ directory that contains production code for java,
/// a src/test/java/ directory that contains tests for java,
/// a src/test/resources/ directory that contains resources that should not be compiled (most of the time),
/// ... (see ref.)
#[cfg(feature = "maven")]
pub struct Maven;

impl CachesHolding for Maven {
    type Caches = super::caches::Maven;
}

#[cfg(feature = "maven")]
pub struct Pom;

#[cfg(feature = "maven")]
impl CachesHolding for Pom {
    type Caches = super::caches::Pom;
}

impl super::InFiles for Pom {
    fn matches(name: &ObjectName) -> bool {
        name.eq_str("pom.xml")
    }
}

/// The java scheme,
/// made of packages and modules https://docs.oracle.com/javase/specs/jls/se11/html/jls-7.html
#[cfg(feature = "maven")]
pub struct Java;

impl CachesHolding for Java {
    type Caches = super::caches::Java;
}

impl super::InFiles for Java {
    fn matches(name: &ObjectName) -> bool {
        name.ends_with(".java")
    }
}

/// The make scheme,
/// It contains a Makefile and different directories, often src/ or lib/, tests/ or tests/, and also third-party/ docs/ script/,
/// but it is mostly community and programming language dependent.
#[cfg(feature = "make")]
pub struct Make;

impl CachesHolding for Make {
    type Caches = super::caches::Make;
}

#[cfg(feature = "make")]
pub struct MakeFile;

impl CachesHolding for MakeFile {
    type Caches = super::caches::Makefile;
}

impl super::InFiles for MakeFile {
    fn matches(name: &ObjectName) -> bool {
        name.eq_str("Makefile")
    }
}

#[cfg(feature = "cpp")]
pub struct Cpp;

impl CachesHolding for Cpp {
    type Caches = super::caches::Cpp;
}

/// CAUTION about when you change this value,
/// advice: change it only at the very begining
#[doc(hidden)]
pub static mut ONLY_SWITCHES: bool = false;

impl super::InFiles for Cpp {
    fn matches(name: &ObjectName) -> bool {
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
}

/// The npm scheme,
/// it contains a package.json then,
/// in its simplest form contains an index.js and a src/ directory,
/// or is a collection of packages that contains a packages/ directory where each package is located
#[cfg(feature = "npm")]
pub struct Npm;
