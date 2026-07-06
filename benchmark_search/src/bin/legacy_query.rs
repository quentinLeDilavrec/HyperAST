use hyperast::tree_gen::utils_ts::tree_sitter_parse;
use tree_sitter::StreamingIterator as _;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    let path = (args.get(1).map(|p| std::path::PathBuf::from(p)))
        .unwrap_or(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("queries/c/improper.c"));
    let text = std::fs::read_to_string(path).unwrap();

    let language = args.get(2).map(|p| p.as_str()).unwrap_or("C");
    assert_eq!(language, "C", "for now only C is supported"); // easy change
    // let lang = hyperast_vcs_git::resolve_language(&language).unwrap();

    use hyperast_gen_ts_c as ts_gen;

    // let meta_simp = hyperast_benchmark_search::meta_queries::META_SIMP.to_string();

    dbg!();
    let query = if let Some(query) = args.get(3) {
        query.as_str()
    } else {
        // readblock from stdin and end on EOF
        let mut block = String::new();
        use std::io::Read;
        std::io::stdin().read_to_string(&mut block).unwrap();
        &block.trim().to_owned()
    };
    let query = tree_sitter::Query::new(&ts_gen::language(), query).unwrap();
    dbg!();

    let tree = tree_sitter_parse(text.as_bytes(), &ts_gen::language());
    if tree.root_node().has_error() {
        eprintln!("parsed tree has errors");
    }
    dbg!();
    println!("{}", tree.root_node().to_sexp());
    dbg!();

    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(&query, tree.root_node(), text.as_bytes());
    loop {
        matches.advance();
        let Some(m) = matches.get() else {
            break;
        };
        dbg!(m.pattern_index);
    }
    dbg!();
}
