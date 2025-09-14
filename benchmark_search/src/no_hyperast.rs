use git2::Oid;
use hyperast::compat::HashMap;
use std::{fmt::Debug, time::Duration};

/// Special bench approach for original tree-sitter (only file based). Also enable incrementality at file level using git.
/// NOTE might also be needed for the one using my executor but still original tree cursor
pub fn baseline(repository: git2::Repository, commit: &str, depth: usize, query: &str) {
    let start_inst = std::time::Instant::now();
    // compile query
    let language = tree_sitter::Language::new(tree_sitter_java::LANGUAGE);
    let query = tree_sitter::Query::new(&language, query).unwrap();
    let q_comp_dur = start_inst.elapsed();
    shared(
        &repository,
        commit,
        depth,
        &language,
        &query,
        |query, content, tree| {
            let mut query_cursor = tree_sitter::QueryCursor::new();
            let matches = query_cursor.matches(&query, tree.root_node(), content);
            matches.count()
        },
    );
    eprintln!("Query compilation duration: {:?}", q_comp_dur);
}

/// Same but with our query executor
pub fn baseline_our_executor(
    repository: git2::Repository,
    commit: &str,
    depth: usize,
    query: &str,
) {
    let start_inst = std::time::Instant::now();
    // compile query
    let language = tree_sitter::Language::new(tree_sitter_java::LANGUAGE);
    let query = hyperast_tsquery::Query::new(query, language.clone()).unwrap();
    let q_comp_dur = start_inst.elapsed();

    shared(
        &repository,
        commit,
        depth,
        &language,
        &query,
        |query, content, tree| {
            let tree_cursor = tree.walk();
            let cursor = hyperast_tsquery::default_impls::TreeCursor::new(content, tree_cursor);
            let matches = query.matches(cursor);
            matches.count()
        },
    );
    eprintln!("Query compilation duration: {:?}", q_comp_dur);
}

fn shared<Q, R: Debug>(
    repository: &git2::Repository,
    commit: &str,
    depth: usize,
    language: &tree_sitter::Language,
    query: &Q,
    executor: impl Fn(&Q, &[u8], tree_sitter::Tree) -> R,
) {
    shared0(
        &repository,
        commit,
        depth,
        |name| !name.ends_with(".java"),
        &query,
        |query, content| {
            // parse file
            let mut parser = tree_sitter::Parser::new();
            parser.set_language(&language).unwrap();
            let tree = parser.parse(content, None).unwrap();
            executor(query, content, tree)
        },
    );
}

fn shared0<Q, R: Debug>(
    repository: &git2::Repository,
    commit: &str,
    depth: usize,
    filter: impl Fn(&str) -> bool,
    query: &Q,
    executor: impl Fn(&Q, &[u8]) -> R,
) {
    let start_instant = std::time::Instant::now();

    use hyperast_vcs_git::git::retrieve_commit;
    let commit = retrieve_commit(&repository, commit).unwrap().id();
    let mut rw = repository.revwalk().unwrap();
    rw.push(commit).unwrap();
    rw.simplify_first_parent().unwrap();
    let rw = rw.take(depth);

    type FileId = Oid;
    let mut results = HashMap::<FileId, (R, Duration)>::new();
    let mut acc_commit_durations = Vec::<Duration>::new();

    // for each commit
    for commit in rw {
        let commit = commit.unwrap();
        // eprintln!("Commit: {}", commit);
        let commit = repository.find_commit(commit).unwrap();
        let root = commit.tree().unwrap();

        //   for each file only if never seen before
        root.walk(git2::TreeWalkMode::PreOrder, |_name, entry| {
            if entry.kind() != Some(git2::ObjectType::Blob) {
                return git2::TreeWalkResult::Ok;
            }
            let name = entry.name().unwrap();
            if filter(name) {
                return git2::TreeWalkResult::Ok;
            }
            let oid = entry.id();
            let file_id = oid;
            if !results.contains_key(&file_id) {
                let blob = repository.find_blob(oid).unwrap();
                let content = blob.content();

                // parse code and execute query
                let res = executor(query, content);

                let duration = start_instant.elapsed();
                // eprintln!("File {}: {} matches in {:?}", file_id, count, duration);
                results.insert(file_id, (res, duration));
            }
            // TODO also skip directories already seen
            git2::TreeWalkResult::Ok
        })
        .unwrap();
        let commit_duration = start_instant.elapsed();
        acc_commit_durations.push(commit_duration);
    }

    // for (file_id, (count, duration)) in &result {
    //     println!("File {}: {} matches in {:?}", file_id, count, duration);
    // }
    for (index, acc_commit_duration) in acc_commit_durations.iter().enumerate() {
        println!("duration {}: {:?}", index, acc_commit_duration);
    }
    eprintln!("Finished in {:?}", start_instant.elapsed());
}
