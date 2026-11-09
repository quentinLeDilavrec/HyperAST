//! Handles Python processing
//!
mod caches;
mod commit_proc;
mod processor;

use hyperast_gen_ts_python::TStore;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;

use super::FullNode;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

pub(crate) type Proc = super::SimpleProc<Parameter, TStore>;
pub type PythonProc = Proc;

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
        name.ends_with(".py") || name.ends_with(".pyi")
    }

    fn matches_lang(lang: &str) -> bool {
        lang.eq_ignore_ascii_case("python")
    }
}

impl PartialEq<PythonProc> for Parameter {
    fn eq(&self, other: &PythonProc) -> bool {
        self == &other.parameter
    }
}

impl Parameter {
    pub(crate) fn new(query: impl hyperast_tsquery::ArrayStr) -> Self {
        Self {
            query: Some(query.iter().collect()),
            ..Default::default()
        }
    }

    pub(crate) fn ts_language() -> tree_sitter::Language {
        hyperast_gen_ts_python::language()
    }
}

pub(crate) type Acc = super::simple_helpers::SimpleAcc;
