use hyperast::tree_gen::NoOpMore;
use tree_sitter::Parser;

use crate::TStore;
use crate::legion::tree_sitter_parse;

type CTreeGen<'store, 'cache, HAST, Acc> =
    crate::legion::CTreeGen<'store, 'cache, TStore, NoOpMore<HAST, Acc>, true>;
type SimpleStores = hyperast::store::SimpleStores<TStore>;

static EX: &str = r#"
void read_string(char *buf) {
    scanf("%s ", buf);
}"#;

#[test]
pub(crate) fn cpp_tree_sitter_simple() {
    let mut parser = Parser::new();

    {
        parser.set_language(&crate::language()).unwrap();
    }

    let text = { EX.as_bytes() };
    let tree = parser.parse(text, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
}

#[test]
pub(crate) fn cpp_simple_test() {
    let text = { EX.as_bytes() };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    let x = x.compressed_node;
    println!("{}", hyperast::nodes::SyntaxSerializer::new(&stores, x));
    println!("{}", hyperast::nodes::SexpSerializer::new(&stores, x));
    println!("{}", hyperast::nodes::TextSerializer::new(&stores, x));
}

use crate::Type;
use hyperast::nodes::{SyntaxSerializer, TextSerializer};
use hyperast::tree_gen;
use hyperast::tree_gen::utils_ts;

#[test]
fn medium() {
    type CGen<'store, 'b, More> = tree_gen::zipped_ts::TsTreeGen<'store, 'b, TStore, More, true>;
    let mut stores = Default::default();
    let mut md_cache = Default::default();
    let mut r#gen = CGen::new(&mut stores, &mut md_cache);
    let text = EX;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    assert_eq!(f.local._ty, Type::TranslationUnit);
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    dbg!(&f.local.metrics);
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
}

#[test]
fn medium_extra_pattern_precomp() {
    use hyperast_tsquery::{PreparedQuerying, Query};
    use tree_gen::zipped_ts_extra::Acc;

    use crate::legion_ts_simp::CTreeGen;
    use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra as Precomp;

    let lang = crate::language();
    let name = b"";
    let text = EX;
    let query = "(identifier)\n(type_identifier)\n(primitive_type)";
    let precomp = [
        "(identifier)",
        "(type_identifier)",
        "(parameter_declaration (type_identifier) (identifier))",
        "(primitive_type)",
    ]
    .as_slice();

    let (precomp, _q) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));
    let more = PreparedQuerying::<_, TStore, Acc<Type>>::from(&precomp);
    let mut extra = Precomp::<_, Acc<Type>, _>::from(more);

    let mut stores = Default::default();
    let mut r#gen = CTreeGen::new(&mut stores, &mut extra);
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
    assert_eq!(0b1001, f.extra.0)
}

#[test]
fn not_simple() {
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut r#gen = crate::legion::CTreeGen::new(&mut stores, &mut md_cache);
    let text = EX;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    dbg!(&f.local.metrics);
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
}

#[test]
pub(crate) fn issue_npy_nditer_test() {
    let text = CODE_NPY_NDITER_CONSTR;
    let text = text.as_bytes();
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    use crate::legion_ts_simp::CTreeGen;
    let mut tree_gen = CTreeGen::bare(&mut stores);
    // let mut md_cache = Default::default();
    // let mut tree_gen = CTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).node.local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    // println!("{}", tree.root_node().to_sexp());
}

// https://github.com/numpy/numpy/blob/main/numpy/_core/src/multiarray/nditer_constr.c
pub(crate) const CODE_NPY_NDITER_CONSTR: &str = r#"NPY_NO_EXPORT NpyIter *
NpyIter_AdvancedNew()
{
    NIT_ITERINDEX(iter) = 0;
    memset(NIT_BASEOFFSETS(iter), 0, (nop+1)*NPY_SIZEOF_INTP);
}"#;
