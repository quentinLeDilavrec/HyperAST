//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use super::Type;

impl Type {
    pub(crate) fn is_repeat(&self) -> bool {
        is!(
            self,
            ProgramRepeat1,
            _StringRepeat1,
            ParametersRepeat1,
            ListRepeat1,
            GroupingRepeat1,
            NamedNodeRepeat1,
        )
    }
}

macro_rules! is {
($e:expr, $($p:ident $(if $guard:expr)?, )*) => {
    match $e {$(
        Type::$p $(if $guard)? => true,)*
        _ => false
    }
};
}
pub(super) use is;
