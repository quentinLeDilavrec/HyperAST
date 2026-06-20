// # languages
#[cfg(feature = "cpp")]
pub mod cpp;
#[cfg(feature = "java")]
pub mod java;
#[cfg(feature = "python")]
pub mod python;

// # build systems
#[cfg(feature = "make")]
pub mod make;
#[cfg(feature = "maven")]
pub mod maven;

// pub mod file_sys;

#[derive(Clone)]
pub(crate) struct Query(pub(crate) hyperast_tsquery::Query, crate::Str);

impl PartialEq for Query {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Eq for Query {}

impl Query {
    pub(crate) fn new<'a>(
        precomputeds: impl Iterator<Item = &'a str>,
        language: tree_sitter::Language,
    ) -> Self {
        use crate::precomp_patterns::only_parse_query_precomp;
        let precomputeds = precomputeds.collect::<Vec<_>>();
        let precomp = only_parse_query_precomp(precomputeds.as_slice(), language);
        Self(precomp.unwrap(), precomputeds.join("\n").into())
    }
}
