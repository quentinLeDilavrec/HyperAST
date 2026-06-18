//! Fully compress all subtrees from a Java syntax tree produced with tree-sitter.
//!
//! Check the `hyperast_gen_ts_cpp` crate for usage examples.

use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;

use crate::TStore;

pub type TypeScriptTreeGen<'store, 'b, Extra> = TsTreeGen<'store, 'b, TStore, Extra, true>;

#[test]
fn medium_extra_pattern_precomp() {
    use hyperast::nodes::{SyntaxSerializer, TextSerializer};
    use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra as Precomp;
    use hyperast::tree_gen::utils_ts;
    use hyperast::tree_gen::zipped_ts_extra::Acc;
    use hyperast_tsquery::{PreparedQuerying, Query};

    use crate::Type;

    const EX: &str = r#"function f() {return 42;}"#;

    let lang = crate::language();
    let name = b"";
    let text = EX;
    let query = "(identifier)\n(type_identifier)";
    let precomp = ["(type_identifier)", "(identifier)"].as_slice();

    let (precomp, _q) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));
    let more = PreparedQuerying::<_, TStore, Acc<Type>>::from(&precomp);
    let mut extra = Precomp::<_, Acc<Type>, _>::from(more);

    let mut stores = Default::default();
    let mut r#gen = TypeScriptTreeGen::new(&mut stores, &mut extra);
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
    assert_eq!(0b10, f.extra.0)
}
