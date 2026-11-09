//! Handles C processing
//!
mod caches;
mod commit_proc;
mod impls;
mod processor;

use hyperast_gen_ts_c::TStore;

pub type SimpleStores = hyperast::store::SimpleStores<hyperast_gen_ts_c::TStore>;

use super::FullNode;
use super::PrecompQueries;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

pub(crate) type Proc = super::SimpleProc<Parameter, TStore>;
pub type CProc = Proc;

impl super::SimpleProcParam for Parameter {
    fn query(&self) -> Option<crate::processors::Query> {
        self.query.as_ref().map(|q| {
            use hyperast_tsquery::ArrayStr;
            crate::processors::Query::new(q.iter(), <Parameter>::ts_language())
        })
    }

    fn ts_language() -> tree_sitter::Language {
        Parameter::ts_language()
    }

    fn matches_file_name(name: &crate::processing::ObjectName) -> bool {
        name.ends_with(".c") || name.ends_with(".h")
    }

    fn matches_lang(lang: &str) -> bool {
        lang.eq_ignore_ascii_case("c")
    }
}

impl PartialEq<CProc> for Parameter {
    fn eq(&self, other: &CProc) -> bool {
        self == &other.parameter
    }
}

impl Parameter {
    pub(crate) fn new(query: impl hyperast_tsquery::ArrayStr) -> Self {
        Self {
            query: Some(query.iter().collect()),
        }
    }

    pub(crate) fn ts_language() -> tree_sitter::Language {
        hyperast_gen_ts_rust::language()
    }
}

pub(crate) type Acc = super::simple_helpers::SimpleAcc;
