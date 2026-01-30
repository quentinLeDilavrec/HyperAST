use std::fmt::{Debug, Display};
use std::path::{Path, PathBuf};
use std::time::Duration;
use std::{fs, process};

pub use git2::{ErrorCode, Oid, Repository};
use git2::{RemoteCallbacks, Revwalk, TreeEntry};

use hyperast::{position::Position, utils::Url};

use crate::processing::ObjectName;

pub struct Builder<'a>(Revwalk<'a>, &'a Repository, bool);

impl<'a> Builder<'a> {
    pub fn new(repository: &'a Repository) -> Result<Self, git2::Error> {
        let mut rw = repository.revwalk()?;
        rw.set_sorting(git2::Sort::TOPOLOGICAL & git2::Sort::TIME)?;
        Ok(Self(rw, repository, false))
    }
    pub fn before(mut self, before: &str) -> Result<Self, git2::Error> {
        if before.is_empty() {
            return Ok(self);
        }
        let c = retrieve_commit(self.1, before)?;
        for c in c.parents() {
            self.0.hide(c.id())?;
        }
        Ok(self)
    }

    pub fn after(mut self, after: &str) -> Result<Self, git2::Error> {
        if after.is_empty() {
            return Ok(self);
        }
        let c = retrieve_commit(self.1, after)?;
        self.0.push(c.id())?;
        self.2 = true;
        Ok(self)
    }

    pub fn first_parents(mut self) -> Result<Self, git2::Error> {
        self.0.simplify_first_parent()?;
        Ok(self)
    }

    pub fn walk(mut self) -> Result<Revwalk<'a>, git2::Error> {
        if !self.2 {
            self.0.push_head()?;
        }
        Ok(self.0)
    }
}

/// Initialize a [git2::revwalk::Revwalk] to explore commits between before and after.
///
/// # Arguments
///
/// * `repository` - The repository where the walk is done
/// * `before` - The the parent commit
/// * `after` - The the child commit
///
/// # Property
///
/// if `after` is not a descendant of before then only walk `before`
///
/// # Errors
///
/// This function just lets errors from [git2] bubble up.
pub(crate) fn all_commits_between<'a>(
    repository: &'a Repository,
    befor: &str,
    after: &str,
) -> Result<Revwalk<'a>, git2::Error> {
    let b = if befor.is_empty() { &[][..] } else { &[befor] };
    let a = if after.is_empty() { &[][..] } else { &[after] };
    all_commits_between_multi(repository, b, a)
}

pub(crate) fn all_commits_between_multi<'a>(
    repository: &'a Repository,
    before: &[impl AsRef<str>],
    after: &[impl AsRef<str>],
) -> Result<Revwalk<'a>, git2::Error> {
    use git2::*;
    let mut rw = repository.revwalk()?;
    for before in before {
        let before = before.as_ref();
        if before.is_empty() {
            return Err(git2::Error::from_str("one `before` is empty"));
        }
        let c = retrieve_commit(repository, before)?;
        for c in c.parents() {
            rw.hide(c.id())?;
        }
    }
    if after.is_empty() {
        rw.push_head()?;
    } else {
        for after in after {
            let after = after.as_ref();
            if after.is_empty() {
                return Err(git2::Error::from_str("one `after` is empty"));
            }
            let c = retrieve_commit(repository, after)?;
            rw.push(c.id())?;
        }
    }
    rw.set_sorting(Sort::TOPOLOGICAL)?;
    Ok(rw)
}

pub(crate) fn all_first_parents_between<'a>(
    repository: &'a Repository,
    before: &str,
    after: &str,
) -> Result<Revwalk<'a>, git2::Error> {
    let mut rw = all_commits_between(repository, before, after)?;
    rw.simplify_first_parent()?;
    Ok(rw)
}

pub fn retrieve_commit<'a>(
    repository: &'a Repository,
    s: &str,
) -> Result<git2::Commit<'a>, git2::Error> {
    // TODO make a more advanced search with helpful error msgs
    match repository.find_reference(&format!("refs/tags/{}", s)) {
        Ok(c) => match c.peel_to_commit() {
            Ok(c) => Ok(c),
            Err(err) => {
                log::warn!("{}", err);
                repository.find_commit(Oid::from_str(s)?)
            }
        },
        Err(err) => {
            let oid = Oid::from_str(s).map_err(|e| {
                log::warn!("{}", e);
                err
            })?;
            repository.find_commit(oid)
        }
    }
}

pub fn all_commits_from_head(repository: &Repository) -> Revwalk<'_> {
    use git2::*;
    // let REMOTE_REFS_PREFIX = "refs/remotes/origin/";
    // let branch: Option<&str> = None;
    // let currentRemoteRefs:Vec<Object> = vec![];
    let mut rw = repository.revwalk().unwrap();
    rw.push_head().unwrap();
    rw.set_sorting(Sort::TOPOLOGICAL).unwrap();
    rw
    // Revwalk::
    // for reff in repository.references().expect("") {
    //     let reff = reff.unwrap();
    // 	let refName = reff.name().unwrap();
    // 	if refName.starts_with(REMOTE_REFS_PREFIX) {
    // 		if branch.is_none() || refName.ends_with(&("/".to_owned() + branch.unwrap())) {
    // 			currentRemoteRefs.push(reff.);
    // 		}
    // 	}
    // }

    // RevWalk walk = new RevWalk(repository);
    // for (ObjectId newRef : currentRemoteRefs) {
    // 	walk.markStart(walk.parseCommit(newRef));
    // }
    // walk.setRevFilter(commitsFilter);
    // return walk;
}

pub fn fetch_repository<T: TryInto<Url>, U: Into<PathBuf>>(url: T, path: U) -> Repository
where
    <T as TryInto<Url>>::Error: std::fmt::Debug,
{
    let url: Url = url.try_into().unwrap();
    let mut path: PathBuf = path.into();
    path.push(url.path.clone());
    // let url = &format!("{}{}", "https://github.com/", repo_name);
    // let path = &format!("{}{}", "/tmp/hyperastgitresources/repo/", repo_name);
    let mut callbacks = RemoteCallbacks::new();

    let mut time = std::time::Instant::now();
    callbacks.transfer_progress(|x| {
        if time.elapsed() > Duration::from_secs(2) {
            log::debug!("transfer {}/{}", x.received_objects(), x.total_objects());
            time = std::time::Instant::now();
        }
        true
    });

    let mut fo = git2::FetchOptions::new();

    fo.remote_callbacks(callbacks);

    let repository = up_to_date_repo(&path, Some(fo), url.clone());
    if let Ok(repository) = repository {
        return repository;
    }

    if let Err(err) = process::Command::new("git")
        .arg("clone")
        .arg(url.to_string())
        .current_dir(&*path.to_string_lossy())
        .spawn()
    {
        log::error!("tryed to use the git executable, but failed. {}", err);
    }

    log::info!("loading repo at {}", path.display());
    let repository = up_to_date_repo(&path, None, url);
    repository.unwrap()
    // nofetch_repository(url, path)
}

pub fn nofetch_repository<T: TryInto<Url>, U: Into<PathBuf>>(url: T, path: U) -> Repository
where
    <T as TryInto<Url>>::Error: std::fmt::Debug,
{
    let url: Url = url.try_into().unwrap();
    let mut path: PathBuf = path.into();
    path.push(url.path.clone());

    log::info!("loading repo at {}", path.display());
    let repository = up_to_date_repo(&path, None, url);
    repository.unwrap()
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub enum Forge {
    Github,
    Gitlab,
    GitlabInria,
}

impl std::str::FromStr for Forge {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "github.com" => Self::Github,
            "gitlab.com" => Self::Gitlab,
            "gitlab.inria.fr" => Self::GitlabInria,
            x => return Err(format!("'{}' is not an authorize forge", x)),
        })
    }
}

impl Forge {
    fn url(&self) -> &str {
        match self {
            Forge::Github => "https://github.com/",
            Forge::Gitlab => "https://gitlab.com/",
            Forge::GitlabInria => "https://gitlab.inria.fr/",
        }
    }

    /// panics in case `user`` or `name`` contain '/' '#'
    pub fn repo(self, user: impl Into<String>, name: impl Into<String>) -> Repo {
        self.try_repo(user, name).unwrap()
    }

    pub fn try_repo(
        self,
        user: impl Into<String>,
        name: impl Into<String>,
    ) -> Result<Repo, String> {
        let user = user.into();
        if user.contains("#") || user.contains("/") {
            return Err("attempting to inject stuff!".to_string());
        }
        let name = name.into();
        if name.contains("#") || name.contains("/") {
            return Err("attempting to inject stuff!".to_string());
        }
        Ok(Repo {
            forge: self,
            user,
            name,
        })
    }
}

// TODO use `&'static str`s to derive with Copy
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Repo {
    forge: Forge,
    user: String,
    name: String,
}

impl Repo {
    pub fn url(&self) -> String {
        format!("{}{}/{}", self.forge.url(), self.user, self.name)
    }
    pub fn fetch(&self) -> Repository {
        let url = self.url();
        let path = "/tmp/hyperastgitresources/repo/".to_string();
        fetch_repository(url, path)
    }
    pub fn nofetch(&self) -> Repository {
        let url = self.url();
        let path = "/tmp/hyperastgitresources/repo/".to_string();
        nofetch_repository(url, path)
    }

    pub fn fetch_to(&self, path: impl Into<PathBuf>) -> Repository {
        let url = self.url();
        let path = path.into();
        fetch_repository(url, path)
    }

    pub fn nofetch_to(&self, path: impl Into<PathBuf>) -> Repository {
        let url = self.url();
        let path = path.into();
        nofetch_repository(url, path)
    }

    pub fn forge(&self) -> Forge {
        self.forge
    }
    pub fn user(&self) -> &str {
        &self.user
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Display for Repo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}{}/{}", self.forge.url(), self.user, self.name)
    }
}

impl std::str::FromStr for Repo {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (forge, repo) = s
            .split_once("/")
            .ok_or("give a valid repository address without 'https://' and '.git'")?;
        let (user, name) = repo
            .split_once("/")
            .ok_or("give a valid repository address without 'https://' and '.git'")?;
        let forge: Forge = forge.parse()?;
        if name.contains("/") {
            return Err(format!(
                "{} should not contain anymore '/' give a valid repository address",
                name
            ));
        }
        forge.try_repo(user, name)
    }
}

pub fn fetch_github_repository(repo_name: &str) -> Repository {
    let url = format!("{}{}", "https://github.com/", repo_name);
    let path = "/tmp/hyperastgitresources/repo/".to_string();
    fetch_repository(url, path)
}

pub fn fetch_remote(mut x: git2::Remote, head: &str) -> Result<(), git2::Error> {
    let name = x.name().unwrap_or("").to_string();
    let mut time = std::time::Instant::now();
    let mut callbacks = RemoteCallbacks::new();
    callbacks.transfer_progress(move |x| {
        if time.elapsed() > Duration::from_secs(2) {
            log::debug!(
                "{name}/{head} transfer {}/{}",
                x.received_objects(),
                x.total_objects()
            );
            time = std::time::Instant::now();
        }
        true
    });

    let mut fo = git2::FetchOptions::new();

    fo.remote_callbacks(callbacks);
    x.fetch(&[head], Some(&mut fo), None)
}

/// avoid mixing providers
pub fn up_to_date_repo(
    path: &Path,
    fo: Option<git2::FetchOptions>,
    url: Url,
) -> Result<Repository, git2::Error> {
    let Some(fo) = fo else {
        return Repository::open(path);
    };
    best_effort(path, &url, BestEffortState::Init { fo })
}

enum BestEffortState<'cb> {
    Init {
        fo: git2::FetchOptions<'cb>,
    },
    /// trying to fetch
    Updating {
        repo: Repository,
        fo: git2::FetchOptions<'cb>,
        attempts: u8,
    },
    /// Cloning a repository
    ///
    /// We could init then fetch, but direct cloning is faster if we want everything. see clone local
    Cloning {
        builder: git2::build::RepoBuilder<'cb>,
        attempts: u8,
    },
}

impl<'cb> BestEffortState<'cb> {
    fn cloning(fo: git2::FetchOptions<'cb>) -> Self {
        let mut builder = git2::build::RepoBuilder::new();
        builder.bare(true).fetch_options(fo);
        BestEffortState::Cloning {
            builder,
            attempts: 0,
        }
    }
}

fn best_effort<'cb>(
    path: &Path,
    url: &Url,
    state: BestEffortState<'cb>,
) -> Result<Repository, git2::Error> {
    let state = match state {
        BestEffortState::Init { fo } if path.join(".git").exists() => {
            let open = Repository::open(path);
            match open {
                Ok(repo) => {
                    log::info!("opened repo at {}", path.display());
                    BestEffortState::Updating {
                        repo,
                        fo,
                        attempts: 0,
                    }
                }
                Err(e) if e.code() == git2::ErrorCode::NotFound && path.starts_with("/tmp") => {
                    log::warn!("removing {:?}", path.join(".git"));
                    if let Err(e) = fs::remove_dir_all(path.join(".git")) {
                        log::error!("failed to remove corrupted clone: {}", e);
                        return Err(git2::Error::from_str("failed to remove corrupted clone"));
                    }
                    BestEffortState::cloning(fo)
                }
                Err(e) => return Err(e),
            }
        }
        BestEffortState::Init { fo } => {
            if let Ok(repo) = Repository::open(path) {
                BestEffortState::Updating {
                    repo,
                    fo,
                    attempts: 0,
                }
            } else {
                log::warn!("no .git/ in {:?}", path);
                let mut builder = git2::build::RepoBuilder::new();
                builder.bare(true).fetch_options(fo);
                BestEffortState::Cloning {
                    builder,
                    attempts: 0,
                }
            }
        }
        BestEffortState::Updating {
            repo,
            mut fo,
            mut attempts,
        } => {
            let mut remote = repo.find_remote("origin")?;
            let r = try_connect_and_download(path, &mut fo, &mut attempts, &mut remote);
            if let Err(e) = remote.disconnect() {
                log::error!("failed to disconnect remote: {}", e);
            }
            drop(remote);
            match r {
                Ok(_) if attempts == 0 => return Ok(repo),
                Ok(_) => BestEffortState::Updating { repo, fo, attempts },
                Err(e) => return Err(e),
            }
        }
        BestEffortState::Cloning {
            mut builder,
            attempts,
        } => {
            log::debug!("cloning {}", url.path);
            let repo = builder.clone(&url.to_string(), path.join(".git").as_path());
            match repo {
                Ok(repo) => {
                    log::info!("{} cloned", url.path);
                    return Ok(repo);
                }
                Err(e) => {
                    let attempts = attempts + 1;
                    if attempts > 2 {
                        log::error!("failed to clone after 2 attempts");
                        return Err(e);
                    }
                    log::warn!("failed to clone {}: {}", url.path, e);
                    BestEffortState::Cloning { builder, attempts }
                }
            }
        }
    };
    best_effort(path, url, state)
}

fn try_connect_and_download(
    path: &Path,
    fo: &mut git2::FetchOptions<'_>,
    attempts: &mut u8,
    remote: &mut git2::Remote<'_>,
) -> Result<(), git2::Error> {
    let connect = remote.connect(git2::Direction::Fetch);
    if let Err(e) = connect {
        *attempts += 1;
        if *attempts > 2 {
            log::error!("failed to fetch after 2 attempts");
            return Err(e);
        }
        log::warn!("cannot fetch, {}", e);
        return Ok(());
    }
    // let specs = remote
    //     .default_branch()
    //     .inspect_err(|e| log::error!("{}", e))
    //     .ok();
    // let specs = specs.as_ref().and_then(|s| s.as_str()).unwrap_or("coucou");
    // dbg!(specs);
    log::info!("download: {:?}", path);
    let specs: &[&str] = &[];
    if let Err(e) = remote.download(specs, Some(fo)) {
        *attempts += 1;
        if *attempts > 3 {
            log::error!("failed to download after 3 attempts");
            return Err(e);
        }
        log::error!("failed to download: {}", e);
        return Ok(());
    }
    *attempts = 0;
    return Ok(());
}

pub(crate) enum BasicGitObject {
    Blob(Oid, ObjectName),
    Tree(Oid, ObjectName),
}

impl<'a> TryFrom<TreeEntry<'a>> for BasicGitObject {
    type Error = TreeEntry<'a>;

    fn try_from(x: TreeEntry<'a>) -> Result<Self, Self::Error> {
        if x.kind().unwrap().eq(&git2::ObjectType::Tree) {
            Ok(Self::Tree(x.id(), x.name_bytes().into()))
        } else if x.kind().unwrap().eq(&git2::ObjectType::Blob) {
            Ok(Self::Blob(x.id(), x.name_bytes().into()))
        } else {
            Err(x)
        }
    }
}

pub trait NamedObject {
    fn name(&self) -> &ObjectName;
}

impl NamedObject for BasicGitObject {
    fn name(&self) -> &ObjectName {
        match self {
            BasicGitObject::Blob(_, n) => n,
            BasicGitObject::Tree(_, n) => n,
        }
    }
}
impl TypedObject for BasicGitObject {
    fn r#type(&self) -> ObjectType {
        match self {
            BasicGitObject::Blob(..) => ObjectType::File,
            BasicGitObject::Tree(..) => ObjectType::Dir,
        }
    }
}
impl UniqueObject for BasicGitObject {
    type Id = Oid;
    fn id(&self) -> &Oid {
        match self {
            BasicGitObject::Tree { 0: id, .. } => id,
            BasicGitObject::Blob { 0: id, .. } => id,
        }
    }
}

pub enum ObjectType {
    File,
    Dir,
}

pub trait TypedObject {
    fn r#type(&self) -> ObjectType;
}
pub trait UniqueObject {
    type Id: Clone;
    fn id(&self) -> &Self::Id;
}

// enum File<'a, 'b, Id> {
//     File(Id, Vec<u8>, &'a [u8]),
//     Dir(Id, Vec<u8>, &'b [Id]),
// }
// impl<'a, 'b, Id> NamedObject for File<'a, 'b, Id> {
//     fn name(&self) -> &[u8] {
//         match self {
//             File::Dir { 1: name, .. } => name,
//             File::File { 1: name, .. } => name,
//         }
//     }
// }
// impl<'a, 'b, Id: Clone> UniqueObject for File<'a, 'b, Id> {
//     type Id = Id;
//     fn id(&self) -> &Id {
//         match self {
//             File::Dir { 0: id, .. } => id,
//             File::File { 0: id, .. } => id,
//         }
//     }
// }
// impl<'a, 'b, Id> TypedObject for File<'a, 'b, Id> {
//     fn r#type(&self) -> ObjectType {
//         match self {
//             File::Dir(..) => ObjectType::Dir,
//             File::File(..) => ObjectType::File,
//         }
//     }
// }

pub fn read_position(
    repo: &Repository,
    commit: &str,
    position: &Position,
) -> Result<String, git2::Error> {
    read_position_floating(repo, commit, position, 0).map(|x| x.1)
}

// let mut before = 0;
// for _ in 0..border_lines {
//     let x = text[before..]
//         .find(|x: char| x == '\n')
//         .unwrap_or(text.len() - before);
//     before = before + x + 1;
//     before = before.min(text.len() - 1)
// }
// let mut after = text.len();
// for _ in 0..border_lines {
//     let x = text[..after].rfind(|x: char| x == '\n').unwrap_or_default();
//     after = x;
// }
pub fn read_position_floating_lines(
    repo: &Repository,
    commit: &str,
    position: &Position,
    lines: usize,
) -> Result<(String, String, String), git2::Error> {
    let blob = blob_position(repo, Oid::from_str(commit)?, &position)?;
    compute_range_floating(
        blob.content(),
        position,
        |r| {
            if r.is_empty() {
                return r;
            }
            let mut i = r.len();
            for _ in 0..=lines {
                i = r[..i].iter().rposition(|x| *x == b'\n').unwrap_or_default();
            }
            &r[i..]
        },
        |r| {
            if r.is_empty() {
                return r;
            }
            let mut i = 0;
            for _ in 0..=lines {
                let x = r[i..]
                    .iter()
                    .position(|x| *x == b'\n')
                    .unwrap_or(r.len() - i);
                i = i + x + 1;
                i = i.min(r.len() - 1)
            }
            &r[..i]
        },
    )
    .map_err(|err| {
        git2::Error::new(
            err.code(),
            err.class(),
            position.file().to_str().unwrap().to_string() + err.message(),
        )
    })
}

pub fn read_position_floating(
    repo: &Repository,
    commit: &str,
    position: &Position,
    radius: usize,
) -> Result<(String, String, String), git2::Error> {
    let blob = blob_position(repo, Oid::from_str(commit)?, &position)?;
    compute_range_floating(
        blob.content(),
        position,
        |r| {
            let x = r.len().saturating_sub(radius);
            &r[x..]
        },
        |r| {
            let x = radius.min(r.len());
            &r[..x]
        },
    )
    .map_err(|err| {
        git2::Error::new(
            err.code(),
            err.class(),
            position.file().to_str().unwrap().to_string() + err.message(),
        )
    })
}
fn compute_range_floating<F, G>(
    text: &[u8],
    position: &Position,
    f_start: F,
    f_end: G,
) -> Result<(String, String, String), git2::Error>
where
    F: Fn(&[u8]) -> &[u8],
    G: Fn(&[u8]) -> &[u8],
{
    let range = position.range();
    let before = f_start(&text.get(..range.start).ok_or_else(|| {
        git2::Error::from_str(&format!(
            "range {:?} out of text ({}) {:?}",
            range,
            text.len(),
            std::str::from_utf8(text)
        ))
    })?);
    let after = f_end(&text.get(range.end..).ok_or_else(|| {
        git2::Error::from_str(&format!(
            "range {:?} out of text ({}) {:?}",
            range,
            text.len(),
            std::str::from_utf8(text)
        ))
    })?);
    Ok((own(before)?, own(&text[range])?, own(after)?))
}
fn blob_position<'a>(
    repo: &'a Repository,
    commit: Oid,
    position: &Position,
) -> Result<git2::Blob<'a>, git2::Error> {
    let commit = repo.find_commit(commit)?;
    let tree = commit.tree()?;
    let file = tree.get_path(position.file())?;
    let obj = file.to_object(repo)?;
    let blob = obj.into_blob();
    blob.map_err(|_| git2::Error::from_str("file path in position should be a valid file"))
}

fn own(r: &[u8]) -> Result<String, git2::Error> {
    let r = std::str::from_utf8(r);
    let r = r.map_err(|x| git2::Error::from_str(&x.to_string()));
    r.map(|s| s.to_string())
}
