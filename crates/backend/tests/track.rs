use backend::piece_of_code::LocalPieceOfCode;
use backend::{AppState, track::*};
use hyperast_vcs_git::git::Forge;
use hyperast_vcs_git::processing::RepoConfig;

#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test]
// slow test, more of an integration test, benefits from being run in release mode
fn test_track_at_file_pos() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("backend=debug")
        .try_init()
        .unwrap();
    let state: std::sync::Arc<AppState> = AppState::default().into();
    state
        .repositories
        .write()
        .unwrap()
        .register_config(Forge::Github.repo("INRIA", "spoon"), RepoConfig::JavaMaven);
    let path = TrackingParam {
        user: "INRIA".to_string(),
        name: "spoon".to_string(),
        commit: "5f250ead2df52d7fe26a3ed2bdd7a38355f764b1".parse().unwrap(),
        file: "src/main/java/spoon/SpoonModelBuilder.java".to_string(),
    };
    let mut flags = Flags::default();
    flags.upd = true;
    let query = TrackingQuery {
        start: Some(10),
        end: Some(200),
        before: Some("8cafc796a3afdda4d52e90f3d17f12c09735be02".to_string()),
        flags,
    };
    match track_code(state, path, query) {
        Ok(x) => {
            let s = serde_json::to_string_pretty(&x);
            eprintln!("{}", s.unwrap());
        }
        Err(x) => {
            dbg!(x.message);
            panic!()
        }
    }
    Ok(())
}

/// http://127.0.0.1:8888/track_at_path_with_changes/github/official-stockfish/Stockfish/7f2eb10e93879bc569c7ddf6fb51d6f812cc477c
/// /0/8?upd=false&child=false&parent=false&exact_child=false&exact_parent=false&sim_child=false&sim_parent=false&meth=false&typ=false&top=false&file=false&pack=false&dependency=false&dependent=false&references=false&declaration=false
/// http://127.0.0.1:8888/track_at_path_with_changes/github/official-stockfish/Stockfish/5cffc032da8402df79cf6cba4238b83ff5d29f7e
/// /0/8?upd=false&child=false&parent=false&exact_child=false&exact_parent=false&sim_child=false&sim_parent=false&meth=false&typ=false&top=false&file=false&pack=false&dependency=false&dependent=false&references=false&declaration=false
#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test]
// slow test, more of an integration test, benefits from being run in release mode
fn test_track_at_path() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("backend=debug")
        .try_init()
        .unwrap();
    let state: std::sync::Arc<AppState> = AppState::default().into();
    let repo_handle = state.repositories.write().unwrap().register_config(
        Forge::Github.repo("official-stockfish", "Stockfish"),
        RepoConfig::CppMake,
    );
    let target_oid = "7f2eb10e93879bc569c7ddf6fb51d6f812cc477c".parse().unwrap();
    let path = TrackingAtPathParam {
        user: "official-stockfish".to_string(),
        name: "Stockfish".to_string(),
        commit: target_oid,
        path: vec![0, 8],
    };
    let flags = Flags::default();
    let query = TrackingQuery {
        start: None,
        end: None,
        before: None,
        flags,
    };
    let r = match track_code_at_path(state.clone(), path, query) {
        Ok(x) => {
            let s = serde_json::to_string_pretty(&x);
            eprintln!("{}", s.unwrap());
            x
        }
        Err(x) => {
            dbg!(x.message);
            panic!()
        }
    };
    let matched_oid = r.matched[0].commit.clone();
    let path = TrackingAtPathParam {
        user: "official-stockfish".to_string(),
        name: "Stockfish".to_string(),
        commit: matched_oid,
        path: r.matched[0].path.clone(),
    };
    let flags = Flags::default();
    let query = TrackingQuery {
        start: None,
        end: None,
        before: None,
        flags,
    };
    let r2 = match track_code_at_path(state.clone(), path, query) {
        Ok(x) => x,
        Err(x) => {
            dbg!(x.message);
            panic!()
        }
    };
    let matched2_oid = r2.matched[0].commit.clone();

    let s = serde_json::to_string_pretty(&r);
    eprintln!("{}", s.unwrap());
    let s = serde_json::to_string_pretty(&r2);
    eprintln!("{}", s.unwrap());
    dbg!(&r.src.path_ids.last());
    dbg!(&r.matched[0].path_ids.last());
    dbg!(&r2.src.path_ids.last());
    dbg!(&r2.matched[0].path_ids.last());

    let repositories = state.repositories.read().unwrap();
    let target_tr = get_commit_root(&repositories, &repo_handle.config, target_oid)?;
    dbg!(target_oid, target_tr);
    let matched_tr = get_commit_root(&repositories, &repo_handle.config, matched_oid)?;
    dbg!(matched_oid, matched_tr, repo_handle.config);
    let matched_tr2 = get_commit_root(&repositories, &repo_handle.config, matched2_oid)?;
    dbg!(matched2_oid, matched_tr2);

    let matched_poc = LocalPieceOfCode::<_, u16>::from_root_and_offsets(
        &repositories.processor.main_stores,
        matched_tr,
        &r.matched[0].path,
    );
    dbg!(matched_oid);
    dbg!(&matched_poc);
    let next_src_poc = LocalPieceOfCode::<_, u16>::from_root_and_offsets(
        &repositories.processor.main_stores,
        matched_tr,
        &r.src.path,
    );
    dbg!(&next_src_poc);
    assert_eq!(&r2.src.path_ids, &next_src_poc.path_ids);
    assert_eq!(&r.matched[0].path_ids, &matched_poc.path_ids);
    assert_eq!(r.matched[0], r2.src);

    Ok(())
}
