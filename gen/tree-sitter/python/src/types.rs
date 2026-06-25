//! Definition of the Java tree-sitter node types.
// TODO auto generate the variants, conversions, and predicates

use hyperast::tree_gen::TsEnableTS;

pub mod impls;
pub mod more;

pub type Role = hyperast::types::Role;

#[derive(Clone, Copy, Default)]
pub struct TStore;

#[derive(Debug)]
pub struct Lang;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TIdN<IdN>(IdN);

pub type TType = hyperast::types::TypeU16<Lang>;

// do not hesitate to extend this macro
macro_rules! declare_type {
    ( $($t:ident = $s:literal);* $(;)? ) => {
        #[repr(u16)]
        #[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
        pub enum Type {
            $( $t, )*
            Directory = TStore::DIRECTORY,
            Spaces = TStore::SPACES,
            _ERROR = TStore::_ERROR,
            ERROR = TStore::ERROR,
        }
        impl Type {
            pub fn from_u16(t: u16) -> Type {
                match t {
                    TStore::DIRECTORY => Type::Directory,
                    TStore::SPACES => Type::Spaces,
                    TStore::_ERROR => Type::_ERROR,
                    TStore::ERROR => Type::ERROR,
                    1376 => Type::_ERROR, // weird, look at `test_plot_py_error_type` to reproduce
                    x => if let Some(t) = S_T_L.get(x as usize) { *t } else {
                        panic!("id of python type should be in 0..{} but is {x}", S_T_L.len());
                        // dbg!(x);
                        // dbg!(crate::language().node_kind_for_id(x));
                        // panic!();
                        // Type::_ERROR
                    },
                }
            }
            #[allow(unreachable_patterns)]
            pub fn from_str(t: &str) -> Option<Type> {
                Some(match t {
                    $( $s => Type::$t, )*
                    "Directory" => Type::Directory,
                    "Spaces" => Type::Spaces,
                    "_ERROR" => Type::_ERROR,
                    "ERROR" => Type::ERROR,
                    _ => return None,
                })
            }
            pub fn to_str(&self) -> &'static str {
                match self {
                    $( Type::$t => $s, )*
                    Type::Directory => "Directory",
                    Type::Spaces => "Spaces",
                    Type::_ERROR => "_ERROR",
                    Type::ERROR => "ERROR",
                }
            }
        }

        const S_T_L: &[Type] = &[
            $( Type::$t, )*
        ];
    };
}

use more::is;

include!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/src/types/generated.rs"
));
