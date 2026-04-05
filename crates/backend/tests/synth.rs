use backend::{AppState, smells};

#[ignore] // ignore (from normal cargo test) for now, later make a feature
#[test_log::test]
/// Testing tsq pattern generation (synth + simp)
///
/// I have issues upgrading from tree-sitter 0.25 to 0.26.
///
///
/// slow test, more of an integration test, try using release
///
fn test_pattern_generation() -> Result<(), Box<dyn std::error::Error>> {
    let user = "Marcono1234";
    let name = "gson";
    let repo_spec = hyperast_vcs_git::git::Forge::Github.repo(user, name);
    let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
    let commit = "3d241ca0a6435cbf1fa1cdaed2af8480b99fecde";

    let state = AppState::default();
    (state.repositories.write())
        .unwrap()
        .register_config(repo_spec.clone(), config);
    let state = std::sync::Arc::new(state);

    let path = serde_json::json!({
        "user": user,
        "name": name,
        "commit": commit,
        "len": 1
    });

    let path: smells::Path = serde_json::from_value(path).unwrap();

    let examples = smells::smells_ex_from_diffs(state.clone(), path.clone())?;

    assert_eq!(examples.examples.len(), 38);
    assert_eq!(examples.moves.len(), 146);

    let meta_gen = smells::META_GEN.to_string();

    let meta_simp = smells::META_SIMP.to_string();
    let examples = smells::Examples::new(meta_gen, meta_simp, examples.examples);
    let more = serde_json::from_value(serde_json::json!({})).unwrap();

    let smells = smells::smells(examples, state, path, more)?;

    assert_eq!(smells.graphs.len(), 4);
    Ok(())
}
