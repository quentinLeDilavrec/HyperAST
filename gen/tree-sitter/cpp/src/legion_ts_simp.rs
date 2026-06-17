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
fn not_simple() {
    let mut stores = SimpleStores::<TStore>::default();
    let mut md_cache = Default::default();
    let mut r#gen = crate::legion::CppTreeGen::new(&mut stores, &mut md_cache);
    let text = EXAMPLE_SPACING;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    dbg!(&f.local.metrics);
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
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
