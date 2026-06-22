#[cfg(feature = "impact")]
pub mod direct_type_ref;
#[cfg(feature = "impact")]
#[cfg(test)]
pub mod extends_package_local;
pub mod obj_creation;

use crate::git::fetch_github_repository;
use crate::multi_preprocessed;
use crate::preprocessed::PreProcessedRepository;

#[cfg(feature = "impact")]
use std::env;

use hyperast::store::nodes::legion::RawHAST;
#[cfg(feature = "impact")]
use hyperast::utils::memusage;

#[cfg(feature = "impact")]
#[test]
fn example_main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
    let repo_name = &args
        .get(1)
        .expect("give an argument like openjdk/jdk or INRIA/spoon"); //"openjdk/jdk";//"INRIA/spoon";
    let before = &args.get(2).map_or("", |x| x);
    let after = &args.get(3).map_or("", |x| x);
    let dir_path = &args.get(4).map_or("", |x| x);

    let mut preprocessed = PreProcessedRepository::new(&repo_name);
    preprocessed.pre_process(
        &mut fetch_github_repository(&repo_name),
        before,
        after,
        dir_path,
    );

    find_refs_from_canonical_type(&mut preprocessed, before, after, dir_path);
}

#[cfg(feature = "impact")]
pub fn find_refs_from_canonical_type(
    preprocessed: &mut PreProcessedRepository,
    _before: &str,
    _after: &str,
    _dir_path: &str,
) {
    use hyperast_gen_ts_java::impact::{
        element::{IdentifierFormat, LabelPtr, RefsEnum},
        partial_analysis::PartialAnalysis,
    };
    {
        let mut ana = PartialAnalysis::default(); //&mut commits[0].meta_data.0;

        macro_rules! scoped {
            ( $o:expr, $i:expr ) => {{
                let o = $o;
                let i = $i;
                let f = IdentifierFormat::from(i);
                let i = preprocessed.processor.get_or_insert_label(i);
                let i = LabelPtr::new(i, f);
                ana.solver.intern(RefsEnum::ScopedIdentifier(o, i))
            }};
        }
        // let i = ana.solver.intern(RefsEnum::MaybeMissing);
        // // let i = scoped!(i, "ReferenceQueue");
        // let i = scoped!(i, "Reference");
        let i = ana.solver.intern(RefsEnum::Root);
        // let i = scoped!(scoped!(scoped!(i, "java"), "security"), "PrivilegedAction");
        // let i = scoped!(scoped!(scoped!(i, "java"), "util"), "Objects");
        // let i = scoped!(scoped!(scoped!(i, "java"), "util"), "Comparator");
        // let i = scoped!(scoped!(scoped!(i, "java"), "util"), "Arrays");
        // let i = scoped!(scoped!(scoped!(scoped!(i,"jdk"),"internal"),"misc"),"SharedSecrets");
        // let i = scoped!(scoped!(scoped!(scoped!(i,"java"),"util"),"concurrent"),"ThreadFactory");
        // let i = scoped!(scoped!(scoped!(scoped!(i,"java"),"nio"),"file"),"FilePermission");
        // let i = scoped!(scoped!(scoped!(scoped!(i, "java"), "nio"), "file"), "Files");
        let i = scoped!(
            scoped!(scoped!(scoped!(i, "java"), "nio"), "file"),
            "InvalidPathException"
        );
        let _ = i;
        // let i = scoped!(scoped!(scoped!(scoped!(i,"java"),"nio"),"file"),"Path");
        preprocessed.processor.print_refs(&ana);

        // println!("{}", java_tree_gen.stores.label_store);

        // let repository = fetch_github_repository(preprocessed.name());
        // let root = preprocessed
        //     .commits
        //     .get(&repository.refname_to_id(before).unwrap())
        //     .unwrap()
        //     .ast_root;
        // preprocessed.print_matched_references(&mut ana, i, root);
    }

    let mu = memusage();
    // drop(java_tree_gen);
    // drop(full_nodes);
    // drop(commits);
    drop(preprocessed);
    let mu = mu - memusage();
    println!("memory used {}", mu);
}

#[test]
fn example_process_make_cpp_project() {
    use std::io::Write;
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug"))
        .format(|buf, record| {
            if record.level().to_level_filter() > log::LevelFilter::Debug {
                writeln!(buf, "{}", record.args())
            } else {
                writeln!(
                    buf,
                    "[{} {}] {}",
                    buf.timestamp_millis(),
                    record.level(),
                    record.args()
                )
            }
        })
        .init();
    let name = &"official-stockfish/Stockfish";
    let mut preprocessed = PreProcessedRepository::new(name);
    let a = preprocessed.pre_process_make_project_with_limit(
        &mut fetch_github_repository(name),
        "",
        "74a0a73715322608332038f7c0151ddf0609a59a",
        // "f3bfce353168b03e4fedce515de1898c691f81ec",
        // "f97c5b6909d22277f28e3dea2f146e9314d634dc", // issue with operator[]('K') = KB;
        "src",
        2,
    );

    let id = preprocessed.commits.get(&a[0]).unwrap().ast_root;
    eprintln!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&preprocessed.processor.main_stores, id)
    );
}

#[test]
fn test_cpp_top_includes() {
    let mut preprocessed = multi_preprocessed::PreProcessedRepositories::default();
    let repo = crate::git::Forge::Github.repo("official-stockfish", "Stockfish");
    let config = crate::processing::RepoConfig::CppMake;
    let handle = &preprocessed.register_config(repo, config).fetch();
    let commits = preprocessed
        .pre_process_with_limit(
            handle,
            "",
            "587bc647d7d14b53d8625c4446006e23a4acd82a",
            // "f97c5b6909d22277f28e3dea2f146e9314d634dc", // issue with operator[]('K') = KB;
            // "src",
            2,
        )
        .unwrap();

    let commitid = commits[0];
    let id = preprocessed
        .get_commit(&handle.config, &commitid)
        .unwrap()
        .ast_root;
    // eprintln!(
    //     "{}",
    //     hyperast::nodes::SyntaxSerializer::new(&preprocessed.processor.main_stores, id)
    // );
    cpp_top_includes_aux(&preprocessed.processor.main_stores, id);
}

fn cpp_top_includes_aux(
    // stores: &crate::processors::cpp::SimpleStores,
    stores: &crate::SimpleStores,
    id: hyperast::store::defaults::NodeIdentifier,
) {
    use hyperast::types::HyperAST;
    use hyperast_tsquery::Query;

    dbg!();

    let lang = hyperast_gen_ts_cpp::language();
    let query = "(translation_unit)\n(translation_unit (preproc_include)@a)\n(preproc_include)@b";
    let precomp = [
        // "(_declarator/operator_name)",                      // TODO
        // "(parameter_declaration (_declarator/identifier))", // TODO
        // "(parameter_declaration declarator: (identifier))", // TODO
        // "(parameter_declaration type: (type_identifier) declarator: (identifier))", // TODO
        // "(identifier)",
        // "(reference_declarator (identifier))",
        "(type_identifier)",
        // "(parameter_declaration (type_identifier) (identifier))",
        // "(primitive_type)",
    ]
    .as_slice();

    let (_precomp, query) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));

    let mut counts = vec![0; query.enabled_pattern_count()];
    let mut types = std::collections::HashSet::<String>::default();

    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(id);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&stores, pos);
    let mut matches = query.matches(cursor);
    while let Some(m) = matches.next() {
        dbg!();
        let pid = query.enabled_pattern_index(m.pattern_index).unwrap();
        dbg!(pid);
        counts[pid as usize] += 1;
        // for c in m.captures.iter() {
        //     use hyperast::position::structural_pos::CursorHead;
        //     let n = c.node;
        //     let p = n.pos.parent();
        //     dbg!(c.index.to_string());
        //     println!("{}", stores.resolve_type(&p.unwrap()))
        // }
        let cid = query.capture_index_for_name("b").unwrap();
        for n in m.nodes_for_capture_index(cid) {
            use hyperast::position::structural_pos::CursorHead;
            let p = n.pos.parent();
            // dbg!(c.index.to_string());
            println!("{}", stores.resolve_type(&p.unwrap()));
            types.insert(stores.resolve_type(&p.unwrap()).to_string());
        }
    }
    dbg!(&counts);
    dbg!(&types);
    println!("example patters");
    for t in types {
        println!("({t} (preproc_include)@root)");
    }
    println!("(translation_unit)");
    println!("(_ (preproc_include))");
    println!("(preproc_include)");
}

#[test]
fn test_python() {
    let mut preprocessed = multi_preprocessed::PreProcessedRepositories::default();

    let precomp = ["(identifier)", "(integer)", "(module)"].as_slice();

    // let repo = crate::git::Forge::Github.repo("pallets", "click");
    let repo = crate::git::Forge::Github.repo("pallets", "flask");
    let config = crate::processing::RepoConfig::Python;
    // let handle = &preprocessed.register_config(repo, config);
    let handle = preprocessed.register_config_with_prequeries(repo, config, precomp);
    let handle = &handle.fetch();
    let commits = preprocessed
        .pre_process_with_limit(
            handle,
            "",
            "36e4a824f340fdee7ed50937ba8e7f6bc7d17f81", //flask
            // "8a1b1a33d739be05b7e91251e3c0dde77c5e152f", //click
            // "f97c5b6909d22277f28e3dea2f146e9314d634dc", // issue with operator[]('K') = KB;
            // "src",
            2,
        )
        .unwrap();

    let commitid = commits[0];
    let id = preprocessed
        .get_commit(&handle.config, &commitid)
        .unwrap()
        .ast_root;
    python_aux(&preprocessed.processor.main_stores, id, precomp);
}

fn python_aux(
    stores: &crate::SimpleStores,
    id: hyperast::store::defaults::NodeIdentifier,
    precomp: &[&str],
) {
    use hyperast_tsquery::Query;

    dbg!();

    let lang = hyperast_gen_ts_python::language();
    let query = "(module)\n(identifier)@a\n(primary_expression (integer))";

    let (_precomp, query) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));

    let mut counts = vec![0; query.enabled_pattern_count()];
    // let mut types = std::collections::HashSet::<String>::default();

    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(id);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&stores, pos);
    let mut matches = query.matches(cursor);
    while let Some(m) = matches.next() {
        dbg!();
        let pid = query.enabled_pattern_index(m.pattern_index).unwrap();
        dbg!(pid);
        counts[pid as usize] += 1;
        // let cid = query.capture_index_for_name("a").unwrap();
        // for n in m.nodes_for_capture_index(cid) {
        //     use hyperast::position::structural_pos::CursorHead;
        //     let p = n.pos.parent();
        //     println!("{}", stores.resolve_type(&p.unwrap()));
        //     types.insert(stores.resolve_type(&p.unwrap()).to_string());
        // }
    }
    dbg!(&counts);
    // dbg!(&types);
}

#[test]
fn test_tsg_incr_inner_classes() -> std::result::Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("trace"))
        .format(|buf, record| {
            use std::io::Write;
            if record.level().to_level_filter() > log::LevelFilter::Debug {
                writeln!(buf, "{}", record.args())
            } else {
                writeln!(
                    buf,
                    "[{} {}] {}",
                    buf.timestamp_millis(),
                    record.level(),
                    record.args()
                )
            }
        })
        .init();
    // let repo_spec = hyperast_vcs_git::git::Forge::Github.repo("INRIA", "spoon");
    // let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
    // let commit = "56e12a0c0e0e69ea70863011b4f4ca3305e0542b";
    // let language = "Java";
    let tsg = r#"
(class_declaration name:(_)@name)@class {
    node @class.decl
    attr (@class.decl) name = (source-text @name)
}
"#;
    use hyperast_gen_ts_java::TStore;
    use hyperast_gen_ts_java::legion_with_refs::Acc;
    let tsg = {
        type M<HAST, Acc> = hyperast_tsquery::QueryMatcher<HAST, Acc>;
        type ExtQ<HAST, Acc> =
            hyperast_tsquery::ExtendingStringQuery<M<HAST, Acc>, tree_sitter::Language>;

        let source: &str = tsg;
        let language = hyperast_gen_ts_java::language();

        let mut file = tree_sitter_graph::ast::File::<
            hyperast_tsquery::QueryMatcher<RawHAST<TStore>, &Acc>,
        >::new(language.clone());

        let query_source: ExtQ<RawHAST<TStore>, &Acc> = {
            let x: &[&str] = &[];
            ExtQ::new(language.clone(), Box::new(x), source.len())
        };
        tree_sitter_graph::parser::Parser::<ExtQ<RawHAST<TStore>, &Acc>>::with_ext(
            query_source,
            source,
        )
        .parse_into_file(&mut file)
        .unwrap();
        use tree_sitter_graph::GenQuery;

        M::check(&mut file).unwrap();
        file
    };
    let _t = INNER_CLASSES;
    let _spec: &tree_sitter_graph::ast::File<hyperast_tsquery::QueryMatcher<_, &Acc>> = &tsg;
    let _query: Option<&hyperast_tsquery::Query> = None;
    // let functions = tree_sitter_graph::functions::Functions::<
    // tree_sitter_graph::graph::Graph<
    //     hyperast_tsquery::Node<
    //         hyperast::store::SimpleStores<
    //             TStore,
    //             &hyperast::store::nodes::legion::NodeStoreInner,
    //             &hyperast::store::labels::LabelStore,
    //         >,
    //         &Acc,
    //     >,
    // >,
    // // tree_sitter_graph::graph::GraphErazing<
    // //     hyperast_tsquery::MyNodeErazing<
    // //         hyperast::store::SimpleStores<
    // //             TStore,
    // //             &hyperast::store::nodes::legion::NodeStoreInner,
    // //             &hyperast::store::labels::LabelStore,
    // //         >,
    // //         &Acc,
    // //     >,
    // // >,
    // >::default();
    todo!();
    // let functions = functions.as_any();
    // let more = hyperast_tsquery::PreparedOverlay {
    //     query,
    //     overlayer: spec,
    //     functions,
    // };
    // let mut stores = hyperast::store::SimpleStores::default();
    // let mut md_cache = std::collections::HashMap::default();
    // let line_break = "\n".as_bytes().to_vec();
    // let mut java_tree_gen = hyperast_gen_ts_java::legion_with_refs::JavaTreeGen::<
    //     hyperast_gen_ts_java::types::TStore,
    //     _,
    //     _,
    // >::with_preprocessing(&mut stores, &mut md_cache, more)
    // .with_line_break(line_break);
    // let r = crate::java::handle_java_file(&mut java_tree_gen, &b"".into(), t.as_bytes()).unwrap();
    // log::error!("height : {:3?}", r.local.metrics.height);
    // log::error!("{:?}", stores.node_store);
    // // ASSERT one node per class_declaration
    // // TODO make an automatic test once nodes can be accessed after the contruction
    // Ok(())
}

static INNER_CLASSES: &str = r#"package spoon.test.imports.testclasses;

import spoon.test.imports.testclasses.internal.ChildClass;

public class ClientClass extends ChildClass {
	private class InnerClass {}
	private class InnerClass2 {}
	private class InnerClass3a {}
}
"#;

#[test]
fn test_file_sys() {
    // env_logger::builder()
    //     .parse_env(
    //         ::test_log::env_logger::Env::default().default_filter_or("hyperast_tsquery=trace"),
    //     )
    //     // .target(::test_log::env_logger::Target::Stderr)
    //     .is_test(true)
    //     .try_init()
    //     .unwrap();

    let mut preprocessed = multi_preprocessed::PreProcessedRepositories::default();

    // https://github.com/malivinayak/Multi-Programming/commit/5debb0d755d71f9714d2fcd3e11fdac66d055b4a
    // https://github.com/numpy/numpy/commit/706b1035187baf72959059cd35ac1f5266e1932c
    let spec = crate::git::Forge::Github.repo("numpy", "numpy");
    dbg!();
    let repo = spec.fetch();
    dbg!();
    let config = preprocessed.processor.default_config();
    let repository = crate::processing::ConfiguredRepo2 { spec, repo, config };
    let commits = preprocessed
        .processor
        .pre_process_with_limit(
            &repository,
            "",
            "706b1035187baf72959059cd35ac1f5266e1932c",
            2,
        )
        .unwrap();

    let commitid = commits[0];
    let commit = preprocessed.get_commit(&config, &commitid).unwrap();
    dbg!(std::time::Duration::from_nanos(
        commit.processing_time as u64
    ));
    // let stores = &preprocessed.processor.main_stores;
    // let id = child_by_name(stores, id, "numpy").unwrap();
    // let id = child_by_name(stores, id, "core").unwrap();
    // let id = {
    //     let n = stores.node_store.resolve(id);
    //     use hyperast::types::LabelStore;
    //     use hyperast::types::WithChildren;
    //     dbg!(n.child_count());
    //     let default_label_identifier = stores.label_store.get("getlimits.py").unwrap();
    //     n.get_child_by_name(&default_label_identifier)
    // }
    // .unwrap();

    // TODO make it select only file_sys and python nodes
    // python_aux(stores, id, &[]);
    // TODO make it select only file_sys and cpp nodes
    // cpp_top_includes_aux(stores, id);
}
