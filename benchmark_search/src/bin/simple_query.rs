use std::str::FromStr as _;

use hyperast::nodes::{SyntaxSerializer, TextSerializer};
use hyperast::position::PositionConverter;
use hyperast::position::position_accessors::SolvedPosition;
use hyperast::store::SimpleStores;
use hyperast::tree_gen;
use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;
use hyperast::types::{self, HyperAST, HyperType, WithChildren as _};
use hyperast_vcs_git::processing::RepoConfig;

fn main() {
    // env_logger::Builder::new()
    //     .filter(None, log::LevelFilter::Trace)
    //     .init();
    let args = std::env::args().collect::<Vec<_>>();

    // let path = args.get(1).expect("path");
    let path = (args.get(1).map(|p| std::path::PathBuf::from(p)))
        // .unwrap_or(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("queries/c/improper.c"));
        .unwrap_or(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("queries/rust/destructuring.rs"),
        );
    let text = std::fs::read_to_string(&path).unwrap();

    // let language = args.get(2).expect("language");
    let language = args.get(2).map(|p| p.as_str()).unwrap_or("Rust");
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

        // read from a file
        // let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        //         .join("queries/rust/destructuring.rs.scm");
        // &std::fs::read_to_string(&path).unwrap().to_string()
    };

    dbg!(&query);
    let name = path.to_string_lossy();
    let c = RepoConfig::from_str(language).expect("available language");
    if c == RepoConfig::C {
        use hyperast_gen_ts_c as ts_gen;
        let language = ts_gen::language();
        aux::<ts_gen::TStore>(&name, &text, language, query);
    } else if c == RepoConfig::Cpp {
        use hyperast_gen_ts_cpp as ts_gen;
        let language = ts_gen::language();
        aux::<ts_gen::TStore>(&name, &text, language, query);
    } else if c == RepoConfig::Java {
        use hyperast_gen_ts_java as ts_gen;
        let language = ts_gen::language();
        aux::<ts_gen::TStore>(&name, &text, language, query);
    } else if c == RepoConfig::Typescript {
        use hyperast_gen_ts_typescript as ts_gen;
        let language = ts_gen::language();
        aux::<ts_gen::TStore>(&name, &text, language, query);
    } else if c == RepoConfig::Python {
        use hyperast_gen_ts_python as ts_gen;
        let language = ts_gen::language();
        aux::<ts_gen::TStore>(&name, &text, language, query);
    } else if c == RepoConfig::Rust {
        use hyperast_gen_ts_rust as ts_gen;
        let language = ts_gen::language();
        aux::<ts_gen::TStore>(&name, &text, language, query);
    } else if c == RepoConfig::Any {
        panic!("Need to provide a language")
    } else {
        panic!("Unknown language {:?}", c)
    }
}

fn aux<TStore>(name: &str, text: &str, language: tree_sitter::Language, query: &str)
where
    // TODO simplify those bounds
    TStore: 'static + Default + tree_gen::TsEnabledTS + types::RoleStore,
    <TStore as types::ETypeStore>::Ty2: tree_gen::TsType + 'static,
    <TStore as types::RoleStore>::IdF: From<u16>,
    u16: From<<TStore as types::RoleStore>::IdF>,
{
    let query = match hyperast_tsquery::Query::new(query, language.clone()) {
        Ok(q) => q,
        Err(e) => {
            eprintln!("query error: {}", e);
            return;
        }
    };
    dbg!();

    let tree = hyperast::tree_gen::utils_ts::tree_sitter_parse(text.as_bytes(), &language);
    if tree.root_node().has_error() {
        eprintln!("parsed tree has errors");
    }
    dbg!();
    println!("{}", tree.root_node().to_sexp());
    dbg!();
    let mut stores = SimpleStores::<TStore>::default();
    let mut tgen = TsTreeGen::bare(&mut stores);

    let root = tgen.generate_file(name.as_bytes(), text.as_bytes(), tree.walk());
    let root = root.local.compressed_node;

    println!("{}", SyntaxSerializer::new(&stores, root));

    let pos = hyperast::position::StructuralPosition::new(root);
    let cursor = hyperast_tsquery::hyperast_cursor::TreeCursor::new(&stores, pos);

    let mut matches = query.matches(cursor);

    loop {
        let Some(m) = matches.next() else {
            break;
        };
        println!("# pattern {} matched", m.pattern_index);
        for capture in m.captures.iter() {
            let rc = PositionConverter::new(&capture.node.pos)
                .with_stores(&stores)
                .compute_pos_post_order::<_, hyperast::position::row_col::RowCol<usize>>();
            let p = capture.node.pos.make_file_line_range(&stores);
            let node = capture.node.pos.node();
            // let ty = stores.resolve_type(&node);
            println!(
                "## {}: ({}:{}:{})",
                query.capture_name(capture.index),
                p.0,
                p.1 + 1,
                rc.col() + 1
            );
            let s = TextSerializer::new(&stores, node);
            println!("```\n{}\n```", s);

            let n = stores.node_store.resolve(node);
            let Some(cs) = n.children() else {
                continue;
            };
            println!("children:");
            for id in cs {
                println!("- {}", stores.resolve_type(&id).as_static_str());
            }
        }
    }
}
