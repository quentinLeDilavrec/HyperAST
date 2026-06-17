//! Fully compress all subtrees from a C++ syntax tree produced with tree-sitter.
//!
//! # Example
//! without any additional derived data
//! ```rust
//! # use hyperast::tree_gen::utils_ts::tree_sitter_parse;
//! # use hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen;
//! # let language = hyperast_gen_ts_cpp::language();
//! # let name = b"";
//! # let text = "class G {};";
//! let tree = tree_sitter_parse(text.as_bytes(), &language);
//! let mut stores = Default::default();
//! let mut g = CppTreeGen::bare(&mut stores);
//! let f = g.generate_file(name, text.as_bytes(), tree.walk());
//! use hyperast::nodes::SyntaxSerializer;
//! println!("{}", SyntaxSerializer::new(&stores, f.local.compressed_node));
//! # assert_eq!(text, hyperast::nodes::TextSerializer::new(&stores, f.local.compressed_node).to_string());
//! ```
//! # Example
//! precomputing patterns as defined by TreeSitter Queries
//! ```rust
//! # use hyperast::tree_gen::utils_ts::tree_sitter_parse;
//! # use hyperast_tsquery::{PreparedQuerying, Query};
//! # use hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen;
//! # use hyperast_gen_ts_cpp::legion_ts_simp::PatternPrecompExtra;
//! # use hyperast_gen_ts_cpp::{Type, TStore};
//! # type Acc = hyperast::tree_gen::zipped_ts_extra::Acc<Type>;
//! # let language = hyperast_gen_ts_cpp::language();
//! # let name = b"";
//! # let text = "class G {};";
//! # let tree = tree_sitter_parse(text.as_bytes(), &language);
//! # let query = "(identifier)\n(type_identifier)\n(primitive_type)";
//! # let precomputeds = ["(identifier)", "(type_identifier)"].as_slice();
//! # let mut stores = Default::default();
//! let (precomp, _q) = Query::with_precomputed(query, language, precomputeds).unwrap_or_else(|e| panic!("\n{e}"));
//! let more = PreparedQuerying::<_, TStore, Acc>::from(&precomp);
//! let mut extra = PatternPrecompExtra::<_, Acc, _>::from(more);
//! let mut g = CppTreeGen::new(&mut stores, &mut extra);
//! let f = g.generate_file(name, text.as_bytes(), tree.walk());
//! # use hyperast::nodes::SyntaxSerializer;
//! println!("{}", SyntaxSerializer::new(&stores, f.local.compressed_node));
//! println!("{:?}", f.extra);
//! # assert_eq!(text, hyperast::nodes::TextSerializer::new(&stores, f.local.compressed_node).to_string());
//! # assert_eq!(0b10, f.extra.0)
//! ```

use std::fmt::Debug;
use std::hash::Hash;

use hyperast::compat::HashMap;
use hyperast::store::nodes::legion::dyn_builder::EntityBuilder;
use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;
use hyperast::tree_gen::zipped_ts_extra::{FNode, Local};
use hyperast::tree_gen::{AccWithExtra, Accumulator};
use hyperast::tree_gen::{Extra, WithByteRange, WithExtra};
use hyperast::types::StoreRefAssoc;

use crate::TStore;

pub type CppTreeGen<'store, 'b, Extra> = TsTreeGen<'store, 'b, TStore, Extra, true>;

/// Extra data for the pattern pre-computation step.
///
/// More should be an instance of `hyperast_tsquery::PreparedQuerying<&Query, TS, Acc>,`
pub struct PatternPrecompExtra<IdN, Acc, More> {
    md_cache: HashMap<IdN, PrecompQueries>,
    more: More,
    _phantom: std::marker::PhantomData<Acc>,
}

impl<IdN, Acc, More> From<More> for PatternPrecompExtra<IdN, Acc, More> {
    fn from(value: More) -> Self {
        Self {
            md_cache: HashMap::default(),
            more: value.into(),
            _phantom: std::marker::PhantomData,
        }
    }
}

#[derive(Default, Clone)]
pub struct PrecompQueries(pub u16);

impl Debug for PrecompQueries {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PrecompQueries")
            .field(&format_args!("0b{:b}", self.0))
            .finish()
    }
}

impl std::ops::AddAssign for PrecompQueries {
    fn add_assign(&mut self, other: Self) {
        self.0 |= other.0;
    }
}

impl<HAST, Acc, More> Extra<HAST, Acc> for PatternPrecompExtra<HAST::IdN, Acc, More>
where
    HAST: StoreRefAssoc,
    Acc: Accumulator<Node = FNode<Local>>,
    Acc: WithByteRange,
    // caching
    HAST::IdN: Hash,
    // more
    More: hyperast::tree_gen::More<HAST, Acc = Acc>,
{
    type Acc = AccWithExtra<Acc, PrecompQueries>;

    type Node = <Self::Acc as Accumulator>::Node;

    fn from_cache(
        &mut self,
        id: HAST::IdN,
        or_else: impl FnOnce() -> <Acc as Accumulator>::Node,
    ) -> Self::Node {
        let precomp = self.md_cache.get(&id).cloned().unwrap_or_default();
        (or_else(), precomp).into()
    }

    fn extra(
        &mut self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        dyn_builder: &mut EntityBuilder,
        acc: &mut Self::Acc,
        label: Option<&str>,
    ) {
        use hyperast::tree_gen::More;
        acc.extra().0 |=
            More::<HAST>::match_precomp_queries(&self.more, stores, &acc, label.as_deref());
        hyperast::tree_gen::add_md_precomp_queries(dyn_builder, acc.extra().0);
    }

    fn to_cache(
        &mut self,
        id: HAST::IdN,
        node: <Acc as Accumulator>::Node,
        mut acc: Self::Acc,
    ) -> Self::Node {
        self.md_cache.insert(id, acc.extra().clone());
        (node, acc.extra().clone()).into()
    }
}
