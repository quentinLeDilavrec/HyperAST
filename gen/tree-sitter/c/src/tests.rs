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
pub(crate) fn c_simple_test() {
    let text = EX;
    let tree = tree_sitter_parse(text.as_bytes());
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = x.local.compressed_node;
    println!("{}", hyperast::nodes::SyntaxSerializer::new(&stores, x));
    println!("{}", hyperast::nodes::SexpSerializer::new(&stores, x));
    println!("{}", hyperast::nodes::TextSerializer::new(&stores, x));
    assert_eq!(
        text,
        hyperast::nodes::TextSerializer::new(&stores, x).to_string()
    );
}

// https://github.com/tursodatabase/libsql/blob/6f451a1fabacbcbc9960b232b4c1605a5021979b/libsql-sqlite3/src/shell.c.in
#[test]
pub(crate) fn c_sql_shell_main_begining_test() {
    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/tests/shell_main_begining.c");
    let text = std::fs::read_to_string(&path).unwrap();
    let tree = tree_sitter_parse(text.as_bytes());
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut tree_gen = crate::legion_ts_simp::CTreeGen::bare(&mut stores);
    let x = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = x.local.compressed_node;
    println!(
        "{}",
        hyperast::nodes::IndentedSerializer::<_, _, hyperast::nodes::Json, true>::new(&stores, x)
    );
    pretty_assertions::assert_eq!(
        text,
        hyperast::nodes::TextSerializer::new(&stores, x).to_string()
    );
}

#[test]
pub(crate) fn c_preproc_space_define_test() {
    let text = r##"#  define data shellState"##;
    // # define session_close_all(X,Y)
    let tree = tree_sitter_parse(text.as_bytes());
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut tree_gen = crate::legion_ts_simp::CTreeGen::bare(&mut stores);
    let x = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = x.local.compressed_node;
    println!(
        "{}",
        hyperast::nodes::IndentedSerializer::<_, _, hyperast::nodes::Json, true>::new(&stores, x)
    );
    pretty_assertions::assert_eq!(
        text,
        hyperast::nodes::TextSerializer::new(&stores, x).to_string()
    );
}

/// A bare list of bytes, as the FPGA bitstreams of u-boot (`board/**/fpgadata.c`, included
/// inside an array initializer elsewhere): not valid C on its own, its error node gets more than
/// 10,000 children, so the generator splits them into repeat nodes (`zipped_ts_extra::_post`).
fn bare_byte_list(indent: &str) -> String {
    (0..6_600)
        .map(|i: usize| {
            let start = if i % 16 == 0 { indent } else { "" };
            let end = if i % 16 == 15 { "\n" } else { "" };
            format!("{}0x{:02x},{}", start, (i * 37) % 256, end)
        })
        .collect()
}

fn assert_round_trip(text: &str) {
    let tree = tree_sitter_parse(text.as_bytes());
    let mut stores = SimpleStores::default();
    let mut tree_gen = crate::legion_ts_simp::CTreeGen::bare(&mut stores);
    let x = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = x.local.compressed_node;
    pretty_assertions::assert_eq!(
        text,
        hyperast::nodes::TextSerializer::new(&stores, x).to_string()
    );
}

/// The split kept the parent's start but moved its padding start to the current position: when
/// the parent did not start at byte 0 (here a leading space), `_post` met padding > start and
/// panicked (`unreachable!`, "should normally handle all cases").
#[test]
pub(crate) fn c_many_children_after_leading_space_test() {
    assert_round_trip(&bare_byte_list(" "));
}

/// The same without the leading space (the case the "repeat trick" exception of `_post` covered).
#[test]
pub(crate) fn c_many_children_test() {
    assert_round_trip(&bare_byte_list(""));
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
    let mut extra = Precomp::<_, Acc<Type>, _>::with_cache(more, Default::default());

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
