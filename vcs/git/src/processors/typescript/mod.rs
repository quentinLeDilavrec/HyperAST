//! Handles TypeScript processing
//!
mod caches;
mod commit_proc;
mod processor;

use hyperast_gen_ts_typescript::TStore;

pub type SimpleStores = hyperast::store::SimpleStores<TStore>;

use super::FullNode;

pub(crate) type Proc = super::SimpleProc<Parameter, TStore>;
pub type TypescriptProc = Proc;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub(crate) query: Option<hyperast_tsquery::ZeroSepArrayStr>,
}

impl super::SimpleProcParam for Parameter {
    fn query(&self) -> Option<crate::processors::Query> {
        self.query.as_ref().map(|q| {
            use hyperast_tsquery::ArrayStr;
            crate::processors::Query::new(q.iter(), <Parameter>::ts_language())
        })
    }

    fn ts_language() -> tree_sitter::Language {
        hyperast_gen_ts_typescript::language()
    }

    fn matches_file_name(name: &crate::processing::ObjectName) -> bool {
        name.ends_with(".ts") || name.ends_with(".d.ts") //|| name.ends_with(".tsx")
    }

    fn matches_lang(lang: &str) -> bool {
        lang.eq_ignore_ascii_case("typescript")
    }
}

impl PartialEq<TypescriptProc> for Parameter {
    fn eq(&self, other: &TypescriptProc) -> bool {
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
}

pub(crate) type Acc = super::simple_helpers::SimpleAcc;
