use std::str::FromStr as _;

use hyperast::tree_gen::utils_ts::tree_sitter_parse;
use hyperast_vcs_git::processing::RepoConfig;

fn main() {
    // env_logger::Builder::new()
    //     .filter(None, log::LevelFilter::Trace)
    //     .init();
    let args = std::env::args().collect::<Vec<_>>();

    // let path = args.get(1).expect("path");
    let path = (args.get(1).map(|p| std::path::PathBuf::from(p)))
        .unwrap_or(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("queries/c/improper.c"));
    let text = std::fs::read_to_string(&path).unwrap();

    // let language = args.get(2).expect("language");
    let language = args.get(2).map(|p| p.as_str()).unwrap_or("C");
    // assert_eq!(language, "C", "for now only C is supported"); // easy change
    // let lang = hyperast_vcs_git::resolve_language(&language).unwrap();

    // let meta_simp = hyperast_benchmark_search::meta_queries::META_SIMP.to_string();

    let query = if let Some(query) = args.get(3) {
        query.as_str()
    } else {
        // readblock from stdin and end on EOF
        let mut block = String::new();
        use std::io::Read;
        std::io::stdin().read_to_string(&mut block).unwrap();
        &block.trim().to_owned()
    };

    // let query = r##"(translation_unit
    //     (preproc_ifdef
    //         (function_definition
    //             (function_declarator)
    //         )
    //         (preproc_else
    //             (function_definition
    //                 (function_declarator)
    //             )
    //         )
    //     )
    // )"##;
    // let query = r##"(translation_unit
    //     (ERROR
    //         (function_declarator)
    //         (preproc_call (preproc_directive) @els (#eq? @els "#else"))
    //         (function_definition)
    //     )
    // )"##;
    let c = RepoConfig::from_str(language).expect("available language");
    let language = if let RepoConfig::C = c {
        use hyperast_gen_ts_c as ts_gen;
        ts_gen::language()
    } else if let RepoConfig::Cpp = c {
        use hyperast_gen_ts_cpp as ts_gen;
        ts_gen::language()
    } else if let RepoConfig::Java = c {
        use hyperast_gen_ts_java as ts_gen;
        ts_gen::language()
    } else if let RepoConfig::Python = c {
        use hyperast_gen_ts_python as ts_gen;
        ts_gen::language()
    } else if let RepoConfig::Rust = c {
        use hyperast_gen_ts_rust as ts_gen;
        ts_gen::language()
    } else if let RepoConfig::Typescript = c {
        use hyperast_gen_ts_typescript as ts_gen;
        ts_gen::language()
    } else if let RepoConfig::Any = c {
        panic!()
    } else {
        todo!("unsupported language: {:?}", c)
    };

    let query = hyperast_tsquery::Query::new(query, language.clone()).unwrap();
    dbg!();

    let tree = tree_sitter_parse(text.as_bytes(), &language);
    if tree.root_node().has_error() {
        eprintln!("parsed tree has errors");
    }
    dbg!();
    println!("{}", tree.root_node().to_sexp());
    dbg!();

    let cursor = hyperast_tsquery::default_impls::TreeCursor::new(text.as_bytes(), tree.walk());
    let mut matches = query.matches(cursor);
    loop {
        let Some(m) = matches.next() else {
            break;
        };
        println!("# pattern {} matched", m.pattern_index);
        for capture in m.captures.iter() {
            let p = capture.node.start_position();
            println!(
                "## {}: ({}:{}:{})",
                query.capture_name(capture.index),
                path.to_string_lossy(),
                p.row + 1,
                p.column,
            );
            // let s = TextSerializer::new(&stores, node);
            // println!("```\n{}\n```", s);
        }
    }
}
