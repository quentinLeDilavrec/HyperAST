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

/// The matches of a query on a C file with HyperAST (the generator of the git processor).
fn hyperast_matches(query: &str, text: &str) -> usize {
    let query = hyperast_tsquery::Query::new(query, crate::language()).unwrap();
    let tree = tree_sitter_parse(text.as_bytes());
    let mut stores = SimpleStores::default();
    let mut tree_gen = crate::legion_ts_simp::CTreeGen::bare(&mut stores);
    let file = tree_gen
        .generate_file(b"", text.as_bytes(), tree.walk())
        .local
        .compressed_node;
    let pos = hyperast::position::StructuralPosition::new(file);
    let cursor = hyperast_tsquery::hyperast_cursor::TreeCursor::new(&stores, pos);
    let mut matches = query.matches(cursor);
    let mut count = 0;
    while matches.next().is_some() {
        count += 1;
    }
    count
}

/// The matches of a query on a C file with Tree-sitter's own query cursor.
fn tree_sitter_matches(query: &str, text: &str) -> usize {
    use streaming_iterator::StreamingIterator;
    let query = tree_sitter::Query::new(&crate::language(), query).unwrap();
    let tree = tree_sitter_parse(text.as_bytes());
    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(&query, tree.root_node(), text.as_bytes());
    let mut count = 0;
    while matches.next().is_some() {
        count += 1;
    }
    count
}

/// A query on a node whose children are wrapped in hidden supertypes (an initializer list: each
/// element is an `expression`) is not quadratic in the width of the node: `current_status` kept
/// walking the later siblings after a supertype (fixed by a `break`, 224861a). On 4,000 elements:
/// 7.3 s before the fix, 39 ms after (release; 0.4 s in debug), 1.8 ms with Tree-sitter. The
/// query matches nowhere, as most queries on most files.
#[test]
fn c_query_on_wide_nodes_test() {
    const NO_MATCH: &str = "(if_statement condition: (parenthesized_expression \
        (binary_expression left: (identifier) right: (number_literal)))) @if";
    let initializer = |n: usize| {
        let elements: Vec<String> = (0..n).map(|i| i.to_string()).collect();
        format!("int table[] = {{{}}};\n", elements.join(", "))
    };
    // the sibling walks of `current_status` are not counted by the cursor (`status_count`): the
    // time is the measure, with a wide margin
    let text = initializer(4_000);
    let start = std::time::Instant::now();
    let matches = hyperast_matches(NO_MATCH, &text);
    let elapsed = start.elapsed();
    assert_eq!(matches, 0);
    assert!(elapsed < std::time::Duration::from_secs(2), "{:?} for 4,000 elements", elapsed);
}
