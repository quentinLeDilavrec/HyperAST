use crate::{TStore, Type};
use hyperast::nodes::{SyntaxSerializer, TextSerializer};
use hyperast::store::SimpleStores;
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
    let tree = match tree {
        Ok(t) => t,
        Err(t) => t,
    };
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    assert_eq!(f.local._ty, Type::TranslationUnit);
    dbg!(&f.local.metrics);
}

#[test]
fn not_simple() {
    let mut stores = SimpleStores::<TStore>::default();
    let mut md_cache = Default::default();
    let mut r#gen = crate::legion::CTreeGen::new(&mut stores, &mut md_cache);
    let text = EX;
    let tree = match utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language()) {
        Ok(t) => t,
        Err(t) => t,
    };
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    dbg!(&f.local.metrics);
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
}

static EX: &str = r#"
void read_string(char *buf) {
    scanf("%s ", buf);
}
"#;
