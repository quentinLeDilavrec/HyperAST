use hyperast::nodes::{SyntaxSerializer, TextSerializer};
use hyperast::store::SimpleStores;
use hyperast::tree_gen;
use hyperast::tree_gen::utils_ts;

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
    use crate::legion_ts_simp::CppTreeGen;
    let mut stores = Default::default();
    let mut r#gen = CppTreeGen::bare(&mut stores);
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
    use hyperast_tsquery::{PreparedQuerying, Query};
    use tree_gen::zipped_ts_extra::Acc;

    use crate::legion_ts_simp::CppTreeGen;
    use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra as Precomp;

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

    let (precomp, _q) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));
    let more = PreparedQuerying::<_, TStore, Acc<Type>>::from(&precomp);
    let mut extra = Precomp::<_, Acc<Type>, _>::from(more);

    let mut stores = Default::default();
    let mut r#gen = CppTreeGen::new(&mut stores, &mut extra);
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

#[test]
fn medium_extra_double_pattern_precomp() {
    use hyperast_tsquery::{PreparedQuerying, Query};
    use tree_gen::extra_chain::ChainedExtra;
    use tree_gen::zipped_ts_extra::Acc;

    use crate::legion_ts_simp::CppTreeGen;
    use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra as Precomp;

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

    let (precomp, _q) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));
    let more = PreparedQuerying::<_, TStore, Acc<Type>>::from(&precomp);
    let extra = Precomp::<_, Acc<Type>, _>::from(more);
    let more = PreparedQuerying::<_, TStore, Acc<Type>>::from(&precomp);
    let extra2 = Precomp::<_, Acc<Type>, _>::from(more);
    let mut extra = ChainedExtra(extra, extra2);

    let mut stores = Default::default();
    let mut r#gen = CppTreeGen::new(&mut stores, &mut extra);
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
    assert_eq!(0b111110000, f.extra.0.0)
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
