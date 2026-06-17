use hyperast::nodes::{SyntaxSerializer, TextSerializer};
use hyperast::store::SimpleStores;
use hyperast::tree_gen::{self, utils_ts};

use crate::TStore;
use crate::Type;

#[test]
fn medium() {
    type CppGen<'store, 'b, More> = tree_gen::zipped_ts::TsTreeGen<'store, 'b, TStore, More, true>;
    let mut stores = Default::default();
    let mut md_cache = Default::default();
    let mut r#gen = CppGen::new(&mut stores, &mut md_cache);
    let text = EXAMPLE_SPACING;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    assert_eq!(f.local._ty, Type::TranslationUnit);
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
    dbg!(&f.local.metrics);
}

#[test]
fn medium_extra_bare() {
    type CppGen<'store, 'b, More> =
        tree_gen::zipped_ts_extra::TsTreeGen<'store, 'b, TStore, More, true>;
    let mut stores = Default::default();
    let mut more = tree_gen::zipped_ts_extra::NoOpExtra::<
        TStore,
        tree_gen::zipped_ts_extra::Acc<Type>,
    >::default();
    let mut r#gen = CppGen::with_preprocessing(&mut stores, &mut more);
    let text = EXAMPLE_SPACING;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    type SerWithRoles<'a, IdN, HAST> =
        hyperast::nodes::SimpleSerializer<'a, IdN, HAST, true, false, false, false, true>;
    println!("{}", SerWithRoles::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
    dbg!(&f.local.metrics);
}

#[test]
fn medium_extra_pattern_precomp() {
    use hyperast_tsquery::Query;
    use precomp_extra::PatternPrecompExtra as Precomp;
    use tree_gen::zipped_ts_extra::{Acc, TsTreeGen as TreeGen};

    let lang = crate::language();
    let name = b"";
    let text = EXAMPLE_SPACING;
    let query = "(identifier)\n(type_identifier)\n(primitive_type)";
    let precomp = [
        "(_declarator/operator_name)",                      // TODO
        "(parameter_declaration (_declarator/identifier))", // TODO
        "(parameter_declaration declarator: (identifier))", // TODO
        "(parameter_declaration type: (type_identifier) declarator: (identifier))", // TODO
        "(identifier)",
        "(reference_declarator (identifier))",
        "(type_identifier)",
        "(parameter_declaration (type_identifier) (identifier))",
        "(primitive_type)",
    ]
    .as_slice();

    type CppGen<'store, 'b, Extra> = TreeGen<'store, 'b, TStore, Extra, true>;

    let (precomp, _q) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));
    let mut more = Precomp::<TStore, _, Acc<Type>>::from(&precomp);

    let mut stores = Default::default();
    let mut r#gen = CppGen::with_preprocessing(&mut stores, &mut more);
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());

    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;

    println!("{}", SyntaxSerializer::new(&stores, id));
    type SerWithRoles<'a, IdN, HAST> =
        hyperast::nodes::SimpleSerializer<'a, IdN, HAST, true, false, false, false, true>;
    println!("{}", SerWithRoles::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));

    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
    dbg!(&f.local.metrics);
    dbg!(&f.extra);
    assert_eq!(0b111110000, f.extra.0)
}

mod precomp_extra {
    use std::fmt::Debug;
    use std::hash::Hash;

    use hashbrown::HashMap;
    use hyperast::store::nodes::legion::dyn_builder::EntityBuilder;
    use hyperast::tree_gen;
    use hyperast::tree_gen::WithByteRange;
    use hyperast::tree_gen::WithExtra;
    use hyperast::tree_gen::zipped_ts_extra::FNode;
    use hyperast::tree_gen::zipped_ts_extra::{Extra, Local};
    use hyperast::tree_gen::{AccWithExtra, Accumulator};
    use hyperast::types;
    use hyperast::types::LendT;
    use hyperast::types::StoreRefAssoc;
    use hyperast_tsquery::Query;

    pub struct PatternPrecompExtra<'a, TS, IdN, Acc> {
        md_cache: HashMap<IdN, PrecompQueries>,
        more: hyperast_tsquery::PreparedQuerying<&'a Query, TS, Acc>,
    }

    impl<'a, TS, IdN, Acc> From<&'a Query> for PatternPrecompExtra<'a, TS, IdN, Acc> {
        fn from(value: &'a Query) -> Self {
            Self {
                md_cache: HashMap::default(),
                more: value.into(),
            }
        }
    }

    #[derive(Default, Clone)]
    pub struct PrecompQueries(pub u16);

    impl Debug for PrecompQueries {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_tuple("PrecompQueries")
                .field(&format!("{:b}", self.0))
                .finish()
        }
    }

    impl std::ops::AddAssign for PrecompQueries {
        fn add_assign(&mut self, other: Self) {
            self.0 |= other.0;
        }
    }

    impl<HAST: StoreRefAssoc, Acc> Extra<HAST, Acc>
        for PatternPrecompExtra<'_, HAST::TS, HAST::IdN, Acc>
    where
        Acc: Accumulator<Node = FNode<Local>>,
        Acc: WithByteRange,
        //
        HAST: StoreRefAssoc,
        HAST::IdN: Copy + Hash + Debug,
        HAST::Idx: Hash,
        for<'t> LendT<'t, HAST>: types::WithSerialization + types::WithStats + types::WithRoles,
        HAST::TS: 'static + Clone,
        HAST::TS: types::ETypeStore<Ty2 = Acc::Type>,
        HAST::TS: types::RoleStore<IdF = u16, Role = types::Role>,
        Acc: tree_gen::WithRole<types::Role> + tree_gen::WithChildren<HAST::IdN> + types::Typed,
        for<'acc> &'acc Acc: tree_gen::WithLabel<L = &'acc str>,
    {
        type Acc = AccWithExtra<Acc, PrecompQueries>;
        type Node = <Self::Acc as Accumulator>::Node;
        fn from_cache(
            &mut self,
            id: HAST::IdN,
            otherwise: impl FnOnce() -> <Acc as Accumulator>::Node,
        ) -> Self::Node {
            let precomp = self.md_cache.get(&id).cloned().unwrap_or_default();
            (otherwise(), precomp).into()
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
            dbg!(&acc.extra().0);
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
}

#[test]
fn not_simple() {
    let mut stores = SimpleStores::<TStore>::default();
    let mut md_cache = Default::default();
    let mut r#gen = crate::legion::CppTreeGen::new(&mut stores, &mut md_cache);
    let text = EXAMPLE_SPACING;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
    dbg!(&f.local.metrics);
}

/// the parsing ERROR comes from the second `A()`
static EXAMPLE_SPACING: &str = r#"
class G {};
bool operator==(const G& x, t spec);
namespace d {
class A() A() B {
  B& operator=(const B&) = delete;
};
}  // namespace d
"#;
