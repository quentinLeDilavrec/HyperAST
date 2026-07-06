use std::str::FromStr;

use hyperast::nodes::SyntaxSerializer;
use hyperast::store::SimpleStores;
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;
use hyperast::{tree_gen, types};
use hyperast_vcs_git::processing::RepoConfig;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    let path = args.get(1).expect("path");
    let text = std::fs::read_to_string(path).unwrap();

    let language = args.get(2).expect("language");

    let c = RepoConfig::from_str(language).expect("available language");
    if c == RepoConfig::C {
        use hyperast_gen_ts_c as ts_gen;
        let language = ts_gen::language();
        let meta_gen = r##"[
        "{" "}" ";" "." "," "=" "(" ")" "[" "]" "!"
        "#ifdef" "#endif" "#else" "#include"
        "return"
        ; "try" "catch" "import" "finally" "return" "throw" "if" "else" "while" "for" "throws"
        ; (line_comment) (block_comment)
        ] @skip
        ; (type_identifier) @label
        (identifier) @label
        ; (_literal) @abstract
        "##;

        aux::<ts_gen::TStore, ts_gen::TIdN<NodeIdentifier>>(text, language, meta_gen);
    } else if c == RepoConfig::Cpp {
        use hyperast_gen_ts_cpp as ts_gen;
        let language = ts_gen::language();
        let meta_gen = r##"[
            "{" "}" ";" "." "," "=" "(" ")" "[" "]" "!"
            "#ifdef" "#endif" "#else" "#include"
            "return"
        ] @skip
        ; (type_identifier) @label
        (identifier) @label
        ; (_literal) @abstract
        "##;

        aux::<ts_gen::TStore, ts_gen::TIdN<NodeIdentifier>>(text, language, meta_gen);
    } else if c == RepoConfig::Java {
        use hyperast_gen_ts_java as ts_gen;
        let language = ts_gen::language();
        let meta_gen = r##"[
            "{" "}" ";" "." "," "=" "(" ")" "[" "]" "!"
        ] @skip
        ; (type_identifier) @label
        (identifier) @label
        ; (_literal) @abstract
        "##;

        aux::<ts_gen::TStore, ts_gen::TIdN<NodeIdentifier>>(text, language, meta_gen);
    } else if c == RepoConfig::Typescript {
        use hyperast_gen_ts_typescript as ts_gen;
        let language = ts_gen::language();
        let meta_gen = r##"[
            "{" "}" ";" "." "," "=" "(" ")" "[" "]" "!" "\""
            "=>" "try" "await" "catch" "break" ":" "default"
            (string_fragment)
        ] @skip
        ; (type_identifier) @label
        (identifier) @label
        ; (_literal) @abstract
        "##;
        aux::<ts_gen::TStore, ts_gen::TIdN<NodeIdentifier>>(text, language, meta_gen);
    } else if c == RepoConfig::Python {
        use hyperast_gen_ts_python as ts_gen;
        let language = ts_gen::language();
        let meta_gen = r##"[
            "{" "}" ";" "." "," "=" "(" ")" "[" "]"
        ] @skip
        ; (type_identifier) @label
        (identifier) @label
        ; (_literal) @abstract
        "##;
        aux::<ts_gen::TStore, ts_gen::TIdN<NodeIdentifier>>(text, language, meta_gen);
    } else if c == RepoConfig::Rust {
        use hyperast_gen_ts_rust as ts_gen;
        let language = ts_gen::language();
        let meta_gen = r##"[
            "{" "}" ";" "." "," "=" "(" ")" "[" "]" "!"
            "let" "if" "else" "return" "continue" "break"
        ] @skip
        ; (type_identifier) @label
        (identifier) @label
        ; (_literal) @abstract
        "##;
        aux::<ts_gen::TStore, ts_gen::TIdN<NodeIdentifier>>(text, language, meta_gen);
    } else if c == RepoConfig::Any {
        panic!("Need to provide a language")
    } else {
        panic!("Unsupported language {:?}", c)
    }
}

fn aux<TStore, TIdN>(text: String, language: tree_sitter::Language, meta_gen: &str)
where
    // TODO simplify those bounds
    TStore: 'static + Default + tree_gen::TsEnabledTS + types::RoleStore,
    <TStore as types::ETypeStore>::Ty2: tree_gen::TsType + 'static,
    TIdN: types::TypedNodeId<IdN = NodeIdentifier>,
    <TIdN as types::TypedNodeId>::Ty: types::TypeTrait,
    <TStore as types::RoleStore>::IdF: From<u16>,
    u16: From<<TStore as types::RoleStore>::IdF>,
{
    // let meta_simp = hyperast_benchmark_search::meta_queries::META_SIMP.to_string();
    // let meta_gen = hyperast_benchmark_search::meta_queries::META_GEN; // only Java ...
    let meta_gen = hyperast_tsquery::Query::new(meta_gen, language.clone()).unwrap();
    dbg!();

    let tree = tree_gen::utils_ts::tree_sitter_parse(text.as_bytes(), &language);
    if tree.root_node().has_error() {
        eprintln!("parsed tree has errors");
    }
    dbg!();
    println!("{}", tree.root_node().to_sexp());
    dbg!();
    // let sss: &hyperast::store::SimpleStores<ts_gen::TStore> = stores.with_ts();
    let mut stores = SimpleStores::<TStore>::default();
    let mut tgen = TsTreeGen::bare(&mut stores);

    let root = tgen.generate_file(b"", text.as_bytes(), tree.walk());
    let root = root.local.compressed_node;

    println!("{}", SyntaxSerializer::new(&stores, root));

    use hyperast_gen_ts_tsquery::auto::tsq_ser_meta2::TreeToQuery;
    let query = TreeToQuery::<_, TIdN>::new(&stores, root, meta_gen);
    dbg!();
    println!("{} @_root", query);
    // let query_poset = hyperast_benchmark_search::synth::synth(
    //     &timeout, &inst, sss, meta_gen, &meta_simp, &config,
    // );
}
