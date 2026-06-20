use hyperast::store::nodes::legion::RawHAST;

use hyperast_gen_ts_java::legion_with_refs::Acc;

use crate::{processors::Query, utils::Str};

use super::{JavaProc, TStore};

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {
    pub query: Option<hyperast_tsquery::ZeroSepArrayStr>,
    pub tsg: Option<Str>,
    pub prepro: Option<Str>,
}

impl Parameter {
    pub(crate) fn with_query(query: impl Into<hyperast_tsquery::ZeroSepArrayStr>) -> Self {
        Self {
            query: Some(query.into()),
            ..Default::default()
        }
    }

    pub(crate) fn with_tsg(tsg: Str) -> Self {
        Self {
            tsg: Some(tsg),
            ..Default::default()
        }
    }
}

impl Into<JavaProc> for Parameter {
    fn into(self) -> JavaProc {
        make_proc(self)
    }
}

impl PartialEq<JavaProc> for Parameter {
    fn eq(&self, other: &JavaProc) -> bool {
        other.parameter == *self
    }
}

fn make_proc(t: Parameter) -> JavaProc {
    let query = t.query.as_ref().map(|q| {
        use hyperast_tsquery::ArrayStr;
        Query::new(q.iter(), hyperast_gen_ts_java::language())
    });

    #[cfg(feature = "tsg")]
    let tsg = if let Some(q) = &t.tsg {
        register_param_tsg(&t, q)
    } else {
        None
    };
    JavaProc {
        parameter: t,
        height_counts: vec![],
        query,
        #[cfg(feature = "tsg")]
        tsg,
        cache: Default::default(),
        commits: Default::default(),
    }
}

#[cfg(feature = "tsg")]
fn register_param_tsg(t: &Parameter, q: &Str) -> Option<TsgErzedSettings> {
    use std::ops::Deref;
    let tsg = q.deref();
    type ExtQ<'a, HAST, Acc> =
        hyperast_tsquery::ExtendingStringQuery<M<'a, HAST, Acc>, tree_sitter::Language>;

    let source: &str = tsg;
    let language = hyperast_gen_ts_java::language();

    let mut file = tree_sitter_graph::ast::File::<M<TStore, &Acc>>::new(language.clone());

    let query_source = if let Some(p) = &t.query {
        ExtQ::new(language.clone(), Box::new(p.clone()), source.len())
    } else {
        let x: &[&str] = &[];
        ExtQ::new(language.clone(), Box::new(x), source.len())
    };
    tree_sitter_graph::parser::Parser::<ExtQ<_, _>>::with_ext(query_source, source)
        .parse_into_file(&mut file)
        .unwrap();
    use tree_sitter_graph::GenQuery;

    M::check(&mut file).unwrap();

    let fcts = hyperast_tsquery::ErazedFcts::new::<hyperast_tsquery::ImmGraph<RawHAST<TStore>, &Acc>>(
        tree_sitter_graph::functions::Functions::stdlib(),
    );

    Some(TsgErzedSettings {
        file: file.as_any(),
        fcts,
    })
}

#[cfg(feature = "tsg")]
pub(super) struct TsgErzedSettings {
    pub(super) file: ErazedTSG,
    pub(super) fcts: hyperast_tsquery::ErazedFcts,
}

#[cfg(feature = "tsg")]
type ErazedTSG = Box<dyn std::any::Any + Send + Sync>;

type M<'a, TS, Acc> = hyperast_tsquery::QueryMatcher<RawHAST<'a, TS>, Acc>;

pub type GraphQuery<'a> = tree_sitter_graph::ast::File<M<'a, TStore, &'a Acc>>;
