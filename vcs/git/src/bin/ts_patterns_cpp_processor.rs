use std::time::Instant;

use hyperast::position::structural_pos::CursorWithPersistence;
use hyperast_tsquery::hyperast_opt::TreeCursor;
use hyperast_vcs_git::git::Forge;
use hyperast_vcs_git::multi_preprocessed::PreProcessedRepositories;
use hyperast_vcs_git::processing::RepoConfig;

fn main() {
    let mut preprocessed = PreProcessedRepositories::default();
    let now = Instant::now();
    let precomp = ["(preproc_include)"].as_slice();

    // let repo = Forge::Github.repo("pallets", "click");
    // let repo = Forge::Github.repo("pallets", "flask");
    let repo = Forge::Github.repo("numpy", "numpy");
    let config = RepoConfig::CppMake;
    let handle = preprocessed.register_config_with_prequeries(repo, config, precomp);
    // let handle = preprocessed.register_config(repo, config);
    let handle = &handle.fetch();
    let commits = preprocessed
        .pre_process_with_limit(
            handle,
            "",
            "706b1035187baf72959059cd35ac1f5266e1932c", // numpy
            // "36e4a824f340fdee7ed50937ba8e7f6bc7d17f81", //flask
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
    let build_time = now.elapsed();
    dbg!(build_time);
    aux(&preprocessed.processor.main_stores, id, precomp);
    let search_time = now.elapsed() - build_time;
    dbg!(search_time);
}

fn aux(
    stores: &hyperast_vcs_git::SimpleStores,
    id: hyperast::store::defaults::NodeIdentifier,
    precomp: &[&str],
) {
    use hyperast_tsquery::Query;

    dbg!();

    let lang = hyperast_gen_ts_cpp::language();
    let query = "(preproc_include path: (string_literal (string_content)))";

    let (_precomp, query) = Query::with_precomputed(query, lang, precomp) //
        .unwrap_or_else(|e| panic!("\n{e}"));

    let mut counts = vec![0; query.enabled_pattern_count()];
    // let mut types = std::collections::HashSet::<String>::default();

    let pos = CursorWithPersistence::new(id);
    let cursor = TreeCursor::new(&stores, pos);
    let mut matches = query.matches(cursor);
    while let Some(m) = matches.next() {
        let pid = query.enabled_pattern_index(m.pattern_index).unwrap();
        counts[pid as usize] += 1;
        // let cid = query.capture_index_for_name("a").unwrap();
        // for n in m.nodes_for_capture_index(cid) {
        //     use hyperast::position::structural_pos::CursorHead;
        //     let p = n.pos.parent();
        //     println!("{}", stores.resolve_type(&p.unwrap()));
        //     types.insert(stores.resolve_type(&p.unwrap()).to_string());
        // }
    }
    dbg!(&matches.status_count);
    dbg!(&matches.goto_count);
    dbg!(&counts);
    // dbg!(&types);
}
