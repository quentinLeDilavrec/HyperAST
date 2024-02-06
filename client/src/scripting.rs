mod estimate;
mod finalize;
mod fs_container;
mod max;
mod mean;
mod min;
mod named_container;
mod parse_tsquery;
mod quantile;
mod refs;
mod stats;

use crate::{scripting::max::Max, SharedState};
use average::Merge;
use axum::Json;
use hyper_ast::{
    store::defaults::NodeIdentifier,
    types::{
        HyperType, IterableChildren, LabelStore, Labeled, TypeStore, Typed, WithChildren, WithStats,
    },
};
use nom::Parser;
use num::ToPrimitive;
use rhai::{
    packages::{BasicArrayPackage, CorePackage, Package},
    Array, Dynamic, Engine, Instant, Scope,
};
use serde::{Deserialize, Serialize};
use std::{
    fmt::{format, Display},
    ops::DerefMut,
    sync::atomic::{AtomicU16, AtomicUsize},
    time::Duration,
};

#[derive(Deserialize, Clone)]
pub struct ScriptingParam {
    user: String,
    name: String,
    commit: String,
}

#[derive(Deserialize, Clone)]
pub struct ScriptContentDepth {
    #[serde(flatten)]
    inner: ScriptContent,
    commits: usize,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct ScriptContent {
    pub init: String,
    pub accumulate: String,
    pub filter: String,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct ScriptTsQueryContent {
    pub stranza_list: Vec<StranzaRaw>,
    commits: usize,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct StranzaRaw {
    pub query: String,
    pub compute: String,
}

#[derive(Clone)]
pub struct Stranza<K> {
    pub root_kind: Option<K>, //hyper_ast::types::AnyType
    pub query: parse_tsquery::Pattern,
    pub compute: Option<u32>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum ScriptingError {
    AtCompilation(String),
    AtEvaluation(String),
    Other(String),
}

#[derive(Deserialize, Serialize)]
pub struct ComputeResult {
    pub compute_time: f64,
    pub result: Dynamic,
}

#[derive(Deserialize, Serialize)]
pub struct ComputeResultIdentified {
    pub commit: String,
    #[serde(flatten)]
    pub inner: ComputeResult,
}

impl Display for ComputeResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Deserialize, Serialize)]
pub struct ComputeResults {
    pub prepare_time: f64,
    pub results: Vec<Result<ComputeResultIdentified, String>>,
}

impl Display for ComputeResults {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub fn simple(
    script: ScriptContent,
    state: SharedState,
    path: ScriptingParam,
) -> Result<Json<ComputeResult>, ScriptingError> {
    let now = Instant::now();
    let (commit, engine, init_script, accumulate_script, filter_script, mut repo) =
        simple_prepare(path, script, &state)?;
    let commits = state
        .repositories
        .write()
        .unwrap()
        .pre_process_with_limit(&mut repo, "", &commit, 2)
        .unwrap();
    log::info!("done construction of {commits:?} in  {}", repo.spec);

    let commit_oid = &commits[0];
    simple_aux(
        state,
        &mut repo,
        commit_oid,
        &engine,
        &init_script,
        &filter_script,
        &accumulate_script,
        now,
    )
    .map(|r| Json(r))
}

pub fn simple_depth(
    script: ScriptContentDepth,
    state: SharedState,
    path: ScriptingParam,
) -> Result<Json<ComputeResults>, ScriptingError> {
    let ScriptContentDepth {
        inner: script,
        commits,
    } = script;
    let now = Instant::now();
    let ScriptingParam { user, name, commit } = path.clone();
    let mut engine = Engine::new();
    engine.disable_symbol("/");
    add_utils(&mut engine);
    let init_script = engine.compile(script.init.clone()).map_err(|x| {
        ScriptingError::AtCompilation(format!("Init: {}, {}", x, script.init.clone()))
    })?;
    let filter_script = engine.compile(script.filter.clone()).map_err(|x| {
        ScriptingError::AtCompilation(format!("Filter: {}, {}", x, script.filter.clone()))
    })?;
    let accumulate_script = engine.compile(script.accumulate.clone()).map_err(|x| {
        ScriptingError::AtCompilation(format!("Acc: {}, {}", x, script.accumulate.clone()))
    })?;
    let repo_spec = hyper_ast_cvs_git::git::Forge::Github.repo(user, name);
    let repo = state
        .repositories
        .write()
        .unwrap()
        .get_config(repo_spec.clone());
    let repo = match repo {
        Some(repo) => repo,
        None => {
            let configs = &mut state.repositories.write().unwrap();
            configs.register_config(
                repo_spec.clone(),
                hyper_ast_cvs_git::processing::RepoConfig::JavaMaven,
            );
            log::error!("missing config for {}", repo_spec);
            configs.get_config(repo_spec.clone()).unwrap()
        }
    };
    // .ok_or_else(|| ScriptingError::Other("missing config for repository".to_string()))?;
    let mut repo = repo.fetch();
    log::warn!("done cloning {}", &repo.spec);
    let commits = state
        .repositories
        .write()
        .unwrap()
        .pre_process_with_limit(&mut repo, "", &commit, commits)
        .unwrap();
    let prepare_time = now.elapsed().as_secs_f64();
    let mut results = vec![];
    for commit_oid in &commits {
        let now = Instant::now();
        let r = simple_aux(
            state.clone(),
            &mut repo,
            commit_oid,
            &engine,
            &init_script,
            &filter_script,
            &accumulate_script,
            now,
        );
        match r {
            Ok(r) => results.push(Ok(ComputeResultIdentified {
                commit: commit_oid.to_string(),
                inner: r,
            })),
            Err(ScriptingError::AtEvaluation(e)) => results.push(Err(e)),
            Err(e) => return Err(e),
        }
    }
    let r = ComputeResults {
        prepare_time,
        results,
    };
    Ok(Json(r))
}

fn simple_prepare(
    path: ScriptingParam,
    script: ScriptContent,
    state: &rhai::Shared<crate::AppState>,
) -> Result<
    (
        String,
        Engine,
        rhai::AST,
        rhai::AST,
        rhai::AST,
        hyper_ast_cvs_git::processing::ConfiguredRepo2,
    ),
    ScriptingError,
> {
    let ScriptingParam { user, name, commit } = path.clone();
    let mut engine = Engine::new();
    engine.disable_symbol("/");
    add_utils(&mut engine);
    let init_script = engine.compile(script.init.clone()).map_err(|x| {
        ScriptingError::AtCompilation(format!("Init: {}, {}", x, script.init.clone()))
    })?;
    let filter_script = engine.compile(script.filter.clone()).map_err(|x| {
        ScriptingError::AtCompilation(format!("Filter: {}, {}", x, script.filter.clone()))
    })?;
    let accumulate_script = engine.compile(script.accumulate.clone()).map_err(|x| {
        ScriptingError::AtCompilation(format!("Acc: {}, {}", x, script.accumulate.clone()))
    })?;
    let repo_spec = hyper_ast_cvs_git::git::Forge::Github.repo(user, name);
    let repo = state
        .repositories
        .write()
        .unwrap()
        .get_config(repo_spec)
        .ok_or_else(|| ScriptingError::Other("missing config for repository".to_string()))?;
    let repo = repo.fetch();
    log::warn!("done cloning {}", &repo.spec);
    Ok((
        commit,
        engine,
        init_script,
        accumulate_script,
        filter_script,
        repo,
    ))
}

fn simple_aux(
    state: rhai::Shared<crate::AppState>,
    repo: &mut hyper_ast_cvs_git::processing::ConfiguredRepo2,
    commit_oid: &hyper_ast_cvs_git::git::Oid,
    engine: &Engine,
    init_script: &rhai::AST,
    filter_script: &rhai::AST,
    accumulate_script: &rhai::AST,
    now: Instant,
) -> Result<ComputeResult, ScriptingError> {
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories.get_commit(&repo.config, commit_oid).unwrap();
    let src_tr = commit_src.ast_root;
    let node_store = &repositories.processor.main_stores.node_store;
    // let size = node_store.resolve(src_tr).size();
    drop(repositories);
    macro_rules! ns {
        ($s:expr) => {
            $s.repositories
                .read()
                .unwrap()
                .processor
                .main_stores
                .node_store
        };
    }
    macro_rules! stores {
        ($s:expr) => {
            $s.repositories.read().unwrap().processor.main_stores
        };
    }
    #[derive(Debug)]
    struct Acc {
        sid: NodeIdentifier,
        value: Option<Dynamic>,
        parent: usize,
        pending_cs: isize,
    }
    let init: Dynamic = engine
        .eval_ast(&init_script)
        .map_err(|x| ScriptingError::AtEvaluation(x.to_string()))?;
    let mut stack: Vec<Acc> = vec![];
    stack.push(Acc {
        sid: src_tr,
        value: Some(init),
        parent: 0,
        pending_cs: -1,
    });
    let mut acc_engine = Engine::new_raw();
    acc_engine.on_print(|text| println!("{text}"));
    let package = CorePackage::new();
    package.register_into_engine(&mut acc_engine);
    let package = BasicArrayPackage::new();
    package.register_into_engine(&mut acc_engine);
    let mut filter_engine = Engine::new_raw();
    filter_engine.on_print(|text| println!("{text}"));
    let package = CorePackage::new();
    package.register_into_engine(&mut filter_engine);
    let package = BasicArrayPackage::new();
    package.register_into_engine(&mut filter_engine);
    // let s = state.clone().read().unwrap();
    let result: Dynamic = loop {
        let Some(mut acc) = stack.pop() else {
            unreachable!()
        };

        let stack_len = stack.len();

        if acc.pending_cs < 0 {
            let mut scope = Scope::new();
            scope.push("s", acc.value.clone().unwrap());
            filter_engine.disable_symbol("/");
            let current = acc.sid;
            let s = state.clone();
            filter_engine.register_fn("type", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let type_store = &stores.type_store;
                let n = node_store.resolve(current);
                let t = type_store.resolve_type(&n);
                t.to_string()
            });
            let s = state.clone();
            filter_engine.register_fn("is_directory", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let type_store = &stores.type_store;
                let n = node_store.resolve(current);
                let t = type_store.resolve_type(&n);
                t.is_directory()
            });
            let s = state.clone();
            filter_engine.register_fn("is_type_decl", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let type_store = &stores.type_store;
                let n = node_store.resolve(current);
                let t = type_store.resolve_type(&n);
                let s = t.as_shared();
                s == hyper_ast::types::Shared::TypeDeclaration
            });
            let s = state.clone();
            filter_engine.register_fn("is_file", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let type_store = &stores.type_store;
                let n = node_store.resolve(current);
                let t = type_store.resolve_type(&n);
                t.is_file()
            });
            let s = state.clone();
            filter_engine.register_fn("children", move || {
                let node_store = &ns!(s);
                node_store
                    .resolve(current)
                    .children()
                    .map_or(Default::default(), |v| {
                        v.0.iter().map(|x| Dynamic::from(*x)).collect::<Array>()
                    })
            });
            let s = state.clone();
            filter_engine.register_fn("is_java_file", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let type_store = &stores.type_store;
                let n = node_store.resolve(current);
                let t = type_store.resolve_type(&n);
                t.is_file()
                    && stores
                        .label_store
                        .resolve(n.get_label_unchecked())
                        .ends_with(".java")
            });
            let s = state.clone();
            filter_engine.register_fn("file_name", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let type_store = &stores.type_store;
                let n = node_store.resolve(current);
                let t = type_store.resolve_type(&n);
                if t.is_file() || t.is_directory() {
                    Ok(stores
                        .label_store
                        .resolve(n.get_label_unchecked())
                        .to_string())
                } else {
                    Err(Box::<rhai::EvalAltResult>::from(
                        "file_name() should be called on a file or a directory",
                    ))
                }
            });
            let s = state.clone();
            filter_engine.register_fn("is_maven_module", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let n = node_store.resolve(current);
                use enumset::EnumSet;
                use hyper_ast_cvs_git::maven::SemFlags;
                n.get_component::<EnumSet<SemFlags>>()
                    .map_or(false, |x| x.contains(SemFlags::IsMavenModule))
            });
            let s = state.clone();
            filter_engine.register_fn("hold_maven_submodule", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let n = node_store.resolve(current);
                use enumset::EnumSet;
                use hyper_ast_cvs_git::maven::SemFlags;
                n.get_component::<EnumSet<SemFlags>>()
                    .map_or(false, |x| x.contains(SemFlags::HoldMavenSubModule))
            });
            let s = state.clone();
            filter_engine.register_fn("hold_java_folder", move || {
                let stores = &stores!(s);
                let node_store = &stores.node_store;
                let n = node_store.resolve(current);
                use enumset::EnumSet;
                use hyper_ast_cvs_git::maven::SemFlags;
                n.get_component::<EnumSet<SemFlags>>().map_or(false, |x| {
                    x.contains(SemFlags::HoldMainFolder) || x.contains(SemFlags::HoldTestFolder)
                })
            });
            add_utils(&mut filter_engine);
            let prepared: Dynamic = filter_engine
                .eval_ast_with_scope(&mut scope, &filter_script)
                .map_err(|x| ScriptingError::AtEvaluation(x.to_string()))?;
            acc.value = Some(scope.get_value("s").unwrap());
            if let Some(prepared) = prepared.try_cast::<Vec<Dynamic>>() {
                stack.push(Acc {
                    pending_cs: prepared.len() as isize,
                    ..acc
                });
                stack.extend(prepared.into_iter().map(|x| x.cast()).map(|x: Array| {
                    let mut it = x.into_iter();
                    Acc {
                        sid: it.next().unwrap().cast(),
                        value: Some(it.next().unwrap()),
                        parent: stack_len,
                        pending_cs: -1,
                    }
                }));
            }
            continue;
        }
        if stack.is_empty() {
            assert_eq!(acc.parent, 0);
            break acc.value.unwrap();
        }

        let mut scope = Scope::new();
        scope.push("s", acc.value.take().unwrap());
        scope.push("p", stack[acc.parent].value.take().unwrap());
        acc_engine.disable_symbol("/");
        let current = acc.sid;
        let s = state.clone();
        acc_engine.register_fn("size", move || {
            let node_store = &ns!(s);
            node_store.resolve(current).size() as i64
        });
        let s = state.clone();
        acc_engine.register_fn("type", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let type_store = &stores.type_store;
            let n = node_store.resolve(current);
            let t = type_store.resolve_type(&n);
            t.to_string()
        });
        let s = state.clone();
        acc_engine.register_fn("is_type_decl", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let type_store = &stores.type_store;
            let n = node_store.resolve(current);
            let t = type_store.resolve_type(&n);
            let s = t.as_shared();
            s == hyper_ast::types::Shared::TypeDeclaration
        });
        let s = state.clone();
        acc_engine.register_fn("is_directory", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let type_store = &stores.type_store;
            let n = node_store.resolve(current);
            let t = type_store.resolve_type(&n);
            t.is_directory()
        });
        let s = state.clone();
        acc_engine.register_fn("is_file", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let type_store = &stores.type_store;
            let n = node_store.resolve(current);
            let t = type_store.resolve_type(&n);
            t.is_file()
        });
        let s = state.clone();
        acc_engine.register_fn("is_java_file", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let type_store = &stores.type_store;
            let n = node_store.resolve(current);
            let t = type_store.resolve_type(&n);
            t.is_file()
                && stores
                    .label_store
                    .resolve(n.get_label_unchecked())
                    .ends_with(".java")
        });
        let s = state.clone();
        acc_engine.register_fn("file_name", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let type_store = &stores.type_store;
            let n = node_store.resolve(current);
            let t = type_store.resolve_type(&n);
            if t.is_file() || t.is_directory() {
                Ok(stores
                    .label_store
                    .resolve(n.get_label_unchecked())
                    .to_string())
            } else {
                Err(Box::<rhai::EvalAltResult>::from(
                    "file_name() should be called on a file or a directory",
                ))
            }
        });
        let s = state.clone();
        acc_engine.register_fn("is_maven_module", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let n = node_store.resolve(current);
            use enumset::EnumSet;
            use hyper_ast_cvs_git::maven::SemFlags;
            n.get_component::<EnumSet<SemFlags>>()
                .map_or(false, |x| x.contains(SemFlags::IsMavenModule))
        });
        let s = state.clone();
        acc_engine.register_fn("hold_maven_submodule", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let n = node_store.resolve(current);
            use enumset::EnumSet;
            use hyper_ast_cvs_git::maven::SemFlags;
            n.get_component::<EnumSet<SemFlags>>()
                .map_or(false, |x| x.contains(SemFlags::HoldMavenSubModule))
        });
        let s = state.clone();
        acc_engine.register_fn("hold_java_folder", move || {
            let stores = &stores!(s);
            let node_store = &stores.node_store;
            let n = node_store.resolve(current);
            use enumset::EnumSet;
            use hyper_ast_cvs_git::maven::SemFlags;
            n.get_component::<EnumSet<SemFlags>>().map_or(false, |x| {
                x.contains(SemFlags::HoldMainFolder) || x.contains(SemFlags::HoldTestFolder)
            })
        });
        let s = state.clone();
        acc_engine.register_fn("references", move |sig: String, p_ref: String| {
            let stores = &stores!(s);
            refs::find_refs(stores, current, p_ref, sig).map_or(0, |x| x as i64)
        });

        let s = state.clone();
        acc_engine.register_fn(
            "pp",
            |s: refs::QPath,
             node: NodeIdentifier,
             i: i64|
             -> Result<refs::Pos, Box<rhai::EvalAltResult>> {
                // let stores = &stores!(s);
                // let it = s.0;
                // let position_converter =
                //     &hyper_ast::position::PositionConverter::new(&it).with_stores(stores);
                // let p = position_converter
                //     .compute_pos_post_order::<_, hyper_ast::position::Position, _>();
                // Ok(refs::Pos::from(p))
                todo!("need to choose a convenient path, try to exploit param overloading")
            },
        );
        add_utils(&mut acc_engine);
        acc_engine
            .eval_ast_with_scope(&mut scope, &accumulate_script)
            .map_err(|x| ScriptingError::AtEvaluation(x.to_string()))?;
        stack[acc.parent].value = Some(scope.get_value("p").unwrap());
    };
    let result = result.finalize();
    let compute_time = now.elapsed().as_secs_f64();
    let r = ComputeResult {
        compute_time,
        result,
    };
    Ok(r)
}

use self::{mean::Mean, min::Min, quantile::Quantile, stats::Stats};
use finalize::Finalize;

fn add_utils(engine: &mut Engine) {
    engine
        .register_type_with_name::<Mean>("Mean")
        .register_fn("Mean", Mean::default)
        .register_fn("+=", |x: &mut Mean, y: Mean| {
            x.merge(&y);
        })
        .register_fn("+=", |m: &mut Mean, x: i64| m.add_i64(x));

    engine
        .register_type_with_name::<Max>("Max")
        .register_fn("Max", Max::default)
        .register_fn("+=", |x: &mut Max, y: Max| {
            x.merge(&y);
        })
        .register_fn("+=", |m: &mut Max, x: i64| m.add_i64(x));

    engine
        .register_type_with_name::<Min>("Min")
        .register_fn("Min", Min::default)
        .register_fn("+=", |x: &mut Min, y: Min| {
            x.merge(&y);
        })
        .register_fn("+=", |m: &mut Min, x: i64| m.add_i64(x));

    engine
        .register_type_with_name::<Quantile>("Quantile")
        .register_fn("Quantile", Quantile::new)
        .register_fn("Median", || Quantile::new(0.5))
        .register_fn("+=", |x: &mut Quantile, y: Quantile| {
            x.merge(&y);
        })
        .register_fn("+=", |m: &mut Quantile, x: i64| m.add_i64(x));

    engine
        .register_type_with_name::<Stats>("Stats")
        .register_fn("Stats", Stats::new)
        .register_fn("+=", |x: &mut Stats, y: Stats| {
            x.merge(&y);
        })
        .register_fn("+=", |m: &mut Stats, x: i64| m.add_i64(x));

    use named_container::NamedContainer;
    engine
        .register_type_with_name::<NamedContainer<Dynamic>>("NamedCont")
        .register_fn("NamedCont", NamedContainer::<Dynamic>::new)
        .register_fn(
            "+=",
            |context: rhai::NativeCallContext,
             x: &mut NamedContainer<Dynamic>,
             y: Dynamic|
             -> Result<(), Box<rhai::EvalAltResult>> {
                let cont = std::mem::replace(&mut x.content, Dynamic::UNIT);
                context.call_native_fn("+", (cont, y)).map(|y| {
                    x.content = y;
                })
            },
        )
        .register_fn(
            "is_empty",
            |context: rhai::NativeCallContext,
             x: &mut NamedContainer<Dynamic>|
             -> Result<bool, Box<rhai::EvalAltResult>> {
                context
                    .call_native_fn("is_empty", (x.content.clone(),))
                    .map(|r| r)
            },
        );
    use fs_container::FsContainer;
    engine
        .register_type_with_name::<FsContainer<Dynamic>>("FsCont")
        .register_fn("FsCont", FsContainer::<Dynamic>::new)
        .register_fn(
            "+=",
            |context: rhai::NativeCallContext,
             x: &mut FsContainer<Dynamic>,
             mut y: Dynamic|
             -> Result<(), Box<rhai::EvalAltResult>> {
                // let mut cont = std::mem::replace(&mut x.content, Dynamic::UNIT);
                // dbg!(&cont, &y);
                // context.call_native_fn("+=", (cont, y))
                // .map(|y|{
                //     x.content = y;
                // })
                let cont = &mut x.content;
                context
                    .call_native_fn_raw("+=", true, &mut [cont, &mut y])
                    .and_then(|y| y.as_unit().map_err(|e| e.into()))
            },
        )
        .register_fn(
            "is_empty",
            |context: rhai::NativeCallContext,
             x: &mut FsContainer<Dynamic>|
             -> Result<bool, Box<rhai::EvalAltResult>> {
                context
                    .call_native_fn("is_empty", (x.content.clone(),))
                    .map(|r| r)
            },
        );
    use refs::QPath;
    engine
        .register_type_with_name::<QPath>("Path")
        .register_fn("Path", QPath::new)
        .register_fn(
            "goto",
            |s: &mut QPath, node: NodeIdentifier, i: i64| -> Result<(), Box<rhai::EvalAltResult>> {
                let i = i.to_u16().ok_or(concat!(
                    "given child offset is too big,",
                    "you most likely made an error,",
                    "otherwise change the configured offset size"
                ))?;
                Ok(s.goto(node, i))
            },
        );
}

pub fn ts_query(
    script: ScriptTsQueryContent,
    state: SharedState,
    path: ScriptingParam,
) -> Result<Json<ComputeResults>, ScriptingError> {
    let ScriptTsQueryContent {
        stranza_list,
        commits,
    } = script;
    let now = Instant::now();
    let ScriptingParam { user, name, commit } = path.clone();
    let mut engine = Engine::new();
    engine.disable_symbol("/");
    add_utils(&mut engine);

    let mut stranza_list: Vec<_> = stranza_list
        .into_iter()
        .map(|StranzaRaw { query, compute }| {
            let q = parse_tsquery::parse_tsquery(&query)
                .map_err(|e| ScriptingError::AtCompilation(format!("ill formed query: {:?}", e)))?;
            let root_kind;
            let query;
            match &q {
                parse_tsquery::Pattern::NamedNode { k: Some(k), .. } => {
                    root_kind = hyper_ast_gen_ts_cpp::types::Type::from_str(&k)
                        .unwrap()
                        .into();
                }
                parse_tsquery::Pattern::NamedNode { k: None, .. } => {
                    root_kind = hyper_ast_gen_ts_cpp::types::Type::End.into();
                }
                x => {
                    return Err(ScriptingError::AtCompilation(format!(
                        "bad root pattern: {:?}",
                        x
                    )))
                }
            }
            query = q;

            let compute: i32 = compute.parse().map_err(|x: std::num::ParseIntError| {
                ScriptingError::AtCompilation(x.to_string())
            })?;
            let compute = Some(compute as u32);
            Ok(Stranza {
                root_kind,
                query,
                compute,
            })
        })
        .try_collect()?;
    let repo_spec = hyper_ast_cvs_git::git::Forge::Github.repo(user, name);
    let repo = state
        .repositories
        .write()
        .unwrap()
        .get_config(repo_spec.clone());
    let repo = match repo {
        Some(repo) => repo,
        None => {
            let configs = &mut state.repositories.write().unwrap();
            configs.register_config(
                repo_spec.clone(),
                hyper_ast_cvs_git::processing::RepoConfig::JavaMaven,
            );
            log::error!("missing config for {}", repo_spec);
            configs.get_config(repo_spec.clone()).unwrap()
        }
    };
    // .ok_or_else(|| ScriptingError::Other("missing config for repository".to_string()))?;
    let mut repo = repo.fetch();
    log::warn!("done cloning {}", &repo.spec);
    let commits = state
        .repositories
        .write()
        .unwrap()
        .pre_process_with_limit(&mut repo, "", &commit, commits)
        .unwrap();
    let prepare_time = now.elapsed().as_secs_f64();
    let mut results = vec![];
    for commit_oid in &commits {
        let now = Instant::now();
        let r = query_aux(
            state.clone(),
            &mut repo,
            commit_oid,
            &engine,
            &stranza_list,
            now,
        );
        match r {
            Ok(r) => results.push(Ok(ComputeResultIdentified {
                commit: commit_oid.to_string(),
                inner: r,
            })),
            Err(ScriptingError::AtEvaluation(e)) => results.push(Err(e)),
            Err(e) => return Err(e),
        }
    }
    let r = ComputeResults {
        prepare_time,
        results,
    };
    Ok(Json(r))
}

fn query_aux(
    state: rhai::Shared<crate::AppState>,
    repo: &mut hyper_ast_cvs_git::processing::ConfiguredRepo2,
    commit_oid: &hyper_ast_cvs_git::git::Oid,
    engine: &Engine,
    stranza_list: &[Stranza<hyper_ast_gen_ts_cpp::types::Type>],
    now: Instant,
) -> Result<ComputeResult, ScriptingError> {
    let repositories = state.repositories.read().unwrap();
    let commit_src = repositories.get_commit(&repo.config, commit_oid).unwrap();
    let src_tr = commit_src.ast_root;
    let node_store = &repositories.processor.main_stores.node_store;
    // let size = node_store.resolve(src_tr).size();
    drop(repositories);
    macro_rules! ns {
        ($s:expr) => {
            $s.repositories
                .read()
                .unwrap()
                .processor
                .main_stores
                .node_store
        };
    }
    macro_rules! stores {
        ($s:expr) => {
            $s.repositories.read().unwrap().processor.main_stores
        };
    }
    let stranza = stranza_list[0].clone();
    let result: usize = if false {
        let root_k = stranza.root_kind.unwrap();
        let root_k = hyper_ast_gen_ts_cpp::types::as_any(&root_k);
        struct Todos {
            remaining: AtomicU16,
            id: Option<NodeIdentifier>,
            parent: Option<std::sync::Arc<Todos>>,
            result: AtomicUsize,
        }
        let root_todo = std::sync::Arc::new(Todos {
            remaining: 1.into(),
            id: None,
            parent: None,
            result: 0.into(),
        });
        let mut stack: Vec<std::sync::Arc<Todos>> = vec![root_todo.clone()];
        stack.push(std::sync::Arc::new(Todos {
            remaining: ns!(state).resolve(src_tr).child_count().into(),
            id: Some(src_tr),
            parent: Some(root_todo.clone()),
            result: 0.into(),
        }));
        // let to_finish: Vec<std::sync::Arc<Todos>> = vec![];

        while stack.len() > 1 {
            let Some(curr) = stack.pop() else {
                unreachable!()
            };
            let node_store = &ns!(state);
            let n = node_store.resolve(curr.id.unwrap());
            let k = n.get_type();
            if k == root_k {
                curr.parent
                    .as_ref()
                    .unwrap()
                    .result
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
            todo!()
        }

        root_todo.result.load(std::sync::atomic::Ordering::Relaxed)
    } else {
        let root_k = stranza.root_kind.unwrap();
        let root_k = hyper_ast_gen_ts_cpp::types::as_any(&root_k);
        struct Todos {
            remaining: u16,
            id: Option<NodeIdentifier>,
            parent: usize,
            result: usize,
        }
        let root_todo = Todos {
            remaining: 1,
            id: None,
            parent: 0,
            result: 0,
        };
        let mut stack: Vec<Todos> = vec![root_todo];
        stack.push(Todos {
            remaining: 1,
            id: Some(src_tr),
            parent: 0,
            result: 0,
        });
        // let to_finish: Vec<std::sync::Arc<Todos>> = vec![];

        while stack.len() > 1 {
            let curr_i = stack.len() - 1;
            let (id, parent, rem) = {
                let Some(curr) = stack.last() else {
                    unreachable!()
                };

                (curr.id.unwrap(), curr.parent, curr.remaining)
            };

            let stores = &stores!(state);
            let n = stores.node_store.resolve(id);
            use hyper_ast::types::LangRef;
            if rem > 0 {
                if let Some(cs) = n.children() {
                    // dbg!(curr_i, parent, rem);
                    let cs = cs.iter_children();
                    stack[curr_i].remaining = ns!(state).resolve(id).child_count();
                    for &c in cs {
                        stack.push(Todos {
                            remaining: 1,
                            id: Some(c),
                            parent: curr_i,
                            result: 0,
                        });
                    }
                } else {
                    // dbg!(curr_i, parent, rem, root_k.to_string());
                    let k = stores.type_store.resolve_type(&n);
                    if k == root_k {
                        dbg!(
                            k.generic_eq(&hyper_ast_gen_ts_cpp::types::Type::TranslationUnit),
                            hyper_ast_gen_ts_cpp::types::Type::TranslationUnit
                                .generic_eq(&hyper_ast_gen_ts_cpp::types::Type::TranslationUnit)
                        );
                        dbg!(
                            root_k,
                            k,
                            root_k.is_file(),
                            k.generic_eq(&k),
                            k == k,
                            root_k.generic_eq(&root_k),
                            root_k == root_k,
                            k.generic_eq(&root_k),
                            k == root_k
                        );
                        // if k == root_k {
                        stack[parent].result += 1;
                    }
                    stack[parent].result += stack[curr_i].result;
                    stack[parent].remaining -= 1;
                    stack.pop().unwrap();
                }
            } else {
                // dbg!(curr_i, parent, rem);
                let k = stores.type_store.resolve_type(&n);
                if k == root_k {
                    dbg!(
                        k.generic_eq(&hyper_ast_gen_ts_cpp::types::Type::TranslationUnit),
                        hyper_ast_gen_ts_cpp::types::Type::TranslationUnit
                            .generic_eq(&hyper_ast_gen_ts_cpp::types::Type::TranslationUnit),
                        hyper_ast_gen_ts_cpp::types::Type::TranslationUnit.generic_eq(&k)
                    );
                    dbg!(
                        root_k,
                        k,
                        root_k.is_file(),
                        k.generic_eq(&k),
                        k == k,
                        root_k.generic_eq(&root_k),
                        root_k == root_k,
                        k.generic_eq(&root_k),
                        k == root_k
                    );
                    stack[parent].result += 1;
                }
                stack[parent].result += stack[curr_i].result;
                stack[parent].remaining -= 1;
                stack.pop().unwrap();
            }
        }
        stack.pop().unwrap().result
    };

    // let rt  = tokio::runtime::Runtime::new().unwrap();
    // rt.block_on(async {
    //     tokio::spawn(async move {
    //         // do some job
    //     }).await.unwrap();

    // });
    // let pool = tokio::task::::new(5)
    //     .with_spawn_timeout(Duration::from_secs(5))
    //     .with_run_timeout(Duration::from_secs(10));

    let result = Dynamic::from_int(result as i64);
    // let result = result.finalize();
    let compute_time = now.elapsed().as_secs_f64();
    let r = ComputeResult {
        compute_time,
        result,
    };
    Ok(r)
}
