use re_ui::UiExt;
use std::{collections::HashSet, hash::Hash, ops::Range, str::FromStr, u8};

use egui_addon::code_editor;

use hyperast::store::nodes::fetched::NodeIdentifier;

type Cpp = hyperast_gen_ts_cpp::types::Type;
type Java = hyperast_gen_ts_java::types::Type;

#[derive(Hash, PartialEq, Eq, Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Repo {
    pub(crate) user: String,
    pub(crate) name: String,
}

pub type CommitId = Oid;

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct Oid(#[doc(hidden)] pub [u8; 20]);

impl Oid {
    // take the first x char of the oid.
    // if x > 20 just return the full oid, equivalent to as_str
    pub fn prefix(&self, x: usize) -> String {
        // std::str::from_utf8(&self.0[..x.min(20)]).unwrap()
        assert!(x % 2 == 0);
        self.0
            .into_iter()
            .take(x / 2)
            .map(|x| format!("{x:02x}"))
            .collect()
    }
    pub fn as_str(&self) -> String {
        self.to_string()
    }
    // pub fn as_str(&self) -> &str {
    //     std::str::from_utf8(&self.0).unwrap()
    // }
    pub(crate) fn tb<'a>(&'a mut self) -> OidTextBuffer<'a> {
        OidTextBuffer(self.to_string(), self)
    }
}

pub(crate) struct OidTextBuffer<'a>(String, &'a mut Oid);
impl egui::TextBuffer for OidTextBuffer<'_> {
    fn is_mutable(&self) -> bool {
        true
    }

    fn as_str(&self) -> &str {
        self.0.as_str()
    }

    fn insert_text(&mut self, text: &str, char_index: usize) -> usize {
        let r = self.0.insert_text(text, char_index);
        *self.1 = self.0.as_str().into();
        r
    }

    fn delete_char_range(&mut self, char_range: Range<usize>) {
        self.0.delete_char_range(char_range);
        *self.1 = self.0.as_str().into();
    }

    fn type_id(&self) -> std::any::TypeId {
        std::any::TypeId::of::<Oid>()
    }
}

impl std::fmt::Debug for Oid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CommitId").field(&self.0).finish()
    }
}

impl std::fmt::Display for Oid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self)
    }
}

impl std::fmt::LowerHex for Oid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for x in self.0 {
            write!(f, "{x:02x}")?;
        }
        Ok(())
    }
}

#[test]
fn feature() {
    let x = "2731bbaf6b4bed23abaae8de5c1fa9f373e30e57";
    assert_eq!(x, &Oid::from(x).to_string());
}

impl From<String> for Oid {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}

impl From<&str> for Oid {
    fn from(value: &str) -> Self {
        match value.parse() {
            Ok(s) => s,
            Err((s, _)) => s,
        }
    }
}

impl FromStr for Oid {
    type Err = (Self, &'static str);

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = s.bytes().map(|c| {
            Some(match c as char {
                '0'..='9' => c as u32 - '0' as u32,
                'a'..='f' => c as u32 - 'a' as u32 + 10,
                _ => return None,
            } as u8)
        });
        let mut bytes = [0; 20];
        for i in 0..20 {
            let Some(c1) = s.next() else {
                return Err((Self(bytes), "no c2"));
            };
            let Some(c1) = c1 else {
                return Err((Self(bytes), "unrecognized oid char"));
            };
            let Some(c2) = s.next() else {
                return Err((Self(bytes), "no c2"));
            };
            let Some(c2) = c2 else {
                return Err((Self(bytes), "unrecognized oid char"));
            };
            bytes[i] = c1 << 4 | c2;
        }
        if s.next().is_some() {
            return Err((Self(bytes), "too many char in oid"));
        }
        Ok(Self(bytes))
    }
}

impl serde::Serialize for Oid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let s = self.to_string();
        serializer.serialize_str(&s)
    }
}
impl<'de> serde::Deserialize<'de> for Oid {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        <&str>::deserialize(deserializer).map(From::from)
    }
}

#[derive(serde::Deserialize, serde::Serialize, Default)]
#[serde(default)]
pub(crate) struct ComputeConfigMulti {
    pub(crate) list: Vec<Commit>,
}

#[derive(serde::Deserialize, serde::Serialize)]
pub(crate) struct ComputeConfigDiff {
    pub(crate) repo: Repo,
    pub(crate) before: CommitId,
    pub(crate) after: CommitId,
}

impl Default for ComputeConfigDiff {
    fn default() -> Self {
        Self {
            repo: Default::default(),
            before: "".into(),
            after: "".into(),
        }
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(default)]
#[derive(Default)]
pub struct CodeRange {
    #[serde(flatten)]
    pub(crate) file: FileIdentifier,
    #[serde(flatten)]
    pub(crate) range: Option<Range<usize>>,
    pub(crate) path: Vec<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) path_ids: Vec<NodeIdentifier>,
}

impl PartialEq for CodeRange {
    fn eq(&self, other: &Self) -> bool {
        let r = self.file.commit == other.file.commit
            // && self.range == other.range
            && self.path == other.path
        && self.path_ids == other.path_ids;
        if r && !(self.range == other.range && self.path_ids == other.path_ids) {
            wasm_rs_dbg::dbg!(
                &self.file.commit,
                &self.range,
                &other.range,
                &self.path_ids,
                &other.path_ids,
            );
        }
        r
    }
}

impl Eq for CodeRange {}

impl Hash for CodeRange {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.file.commit.hash(state);
        // self.range.hash(state);
        self.path.hash(state);
        // self.path_ids.hash(state);
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize, Default)]
#[serde(default)]
pub(crate) struct ComputeConfigTracking {
    pub(crate) target: CodeRange,
}

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(crate) struct ComputeConfigAspectViews {
    pub(super) commit: Commit,
    pub(super) path: String,
    pub(super) hightlight: String,
    pub(super) cst: bool,
    pub(super) spacing: bool,
    pub(super) syntax: bool,
    pub(super) ast: bool,
    pub(super) type_decls: bool,
    pub(super) licence: bool,
    pub(super) doc: bool,
    // pub(super) ser_opt_cpp_text: String,
    // pub(super) ser_opt_java_text: String,
    // TODO use an enum set btw...
    #[serde(skip)]
    pub(super) ser_opt_cpp: TySet<Cpp>,
    #[serde(skip)]
    pub(super) ser_opt_java: TySet<Java>,
    #[serde(skip)]
    pub(super) hide_opt_cpp: TySet<Cpp>,
    #[serde(skip)]
    pub(super) hide_opt_java: TySet<Java>,
}

pub struct TySet<Ty>(HashSet<Ty>);

impl<Ty> std::ops::Deref for TySet<Ty> {
    type Target = HashSet<Ty>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Ty> std::ops::DerefMut for TySet<Ty> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<Ty> Default for TySet<Ty> {
    fn default() -> Self {
        Self(HashSet::default())
    }
}

impl<Ty: Eq + Hash + Copy + 'static> TySet<Ty> {
    pub fn toggle(&mut self, k: &dyn hyperast::types::HyperType) {
        let k = &k.as_any();
        if let Some(k) = k.downcast_ref::<Ty>() {
            self._toggle(k);
        }
    }
}
impl<Ty: Eq + Hash + Copy> TySet<Ty> {
    fn _toggle(&mut self, k: &Ty) {
        if self.0.contains(k) {
            self.0.remove(k);
        } else {
            self.0.insert(k.to_owned());
        }
    }
}

impl Default for ComputeConfigAspectViews {
    fn default() -> Self {
        let mut ser_opt_cpp = TySet::<Cpp>::default();
        ser_opt_cpp.insert(Cpp::FunctionDeclarator);
        let mut ser_opt_java = TySet::<Java>::default();
        ser_opt_java.insert(Java::MethodDeclaration);
        let hide_opt_cpp = TySet::<Cpp>::default();
        let hide_opt_java = TySet::<Java>::default();
        Self {
            commit: Default::default(),
            path: "".into(),
            hightlight: "0".into(),
            cst: true,
            spacing: false,
            syntax: false,
            ast: false,
            type_decls: false,
            licence: false,
            doc: false,
            ser_opt_cpp,
            ser_opt_java,
            hide_opt_cpp,
            hide_opt_java,
            // commit: "4acedc53a13a727be3640fe234f7e261d2609d58".into(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub(crate) struct FileIdentifier {
    #[serde(flatten)]
    pub(crate) commit: Commit,
    #[serde(rename = "file")]
    pub(crate) file_path: String,
}

impl Default for FileIdentifier {
    fn default() -> Self {
        Self {
            commit: Default::default(),
            file_path: "src/main/java/spoon/Launcher.java".to_string(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct Commit {
    #[serde(flatten)]
    pub(crate) repo: Repo,
    #[serde(rename = "commit")]
    #[serde(alias = "id")]
    pub(crate) id: CommitId,
}

impl Repo {
    pub fn with(self, id: impl Into<CommitId>) -> Commit {
        Commit {
            repo: self,
            id: id.into(),
        }
    }
    pub(crate) fn url(&self) -> String {
        format!("https://github.com/{}/{}", self.user, self.name)
    }
}

impl Commit {
    pub(crate) fn url(&self) -> String {
        let url = self.repo.url();
        let id = &self.id;
        format!("{url}/commit/{id}")
    }
    pub(crate) fn commit_url_to_clipboard(
        &self,
        ctx: &egui::Context,
        notification_ui: &mut re_ui::notifications::NotificationUi,
    ) {
        let url = self.url();
        let text = format!("Copied address of github commit to clipboard\n{url}",);
        ctx.copy_text(url);
        notification_ui.success(text);
    }
}

impl Default for Commit {
    fn default() -> Self {
        Repo::default().with("7f2eb10e93879bc569c7ddf6fb51d6f812cc477c")
        // id: "cd339e2c5f0e5c1e42c66b890f02bc282c3a0ea1".into(), // 61074989324d20e7d9cd387cee830a31a7e68aca // 4acedc53a13a727be3640fe234f7e261d2609d58
        // id: "7f2eb10e93879bc569c7ddf6fb51d6f812cc477c".into(),
        // # stockfish
        // * long 7f2eb10e93879bc569c7ddf6fb51d6f812cc477c
        // * more in past "587bc647d7d14b53d8625c4446006e23a4acd82a".into()
        // * close to first b8e487ff9caffb5061f680b1919ab2fe442bc0a1
    }
}

#[derive(
    serde::Deserialize,
    serde::Serialize,
    Default,
    PartialEq,
    Eq,
    Clone,
    Copy,
    strum_macros::EnumIter,
)]
pub enum SelectedConfig {
    #[default]
    Querying,
    Aspects,
    LongTracking,
    Single,
    Tsg,
    Smells,
    Diff,
    Tracking,
    Multi,
}

impl SelectedConfig {
    pub const fn title(&self) -> impl Into<String> + AsRef<str> + use<> {
        match self {
            SelectedConfig::Single => "Stats",
            SelectedConfig::Querying => "Querying",
            SelectedConfig::Tsg => "TSG",
            SelectedConfig::Smells => "Smells", //ℹ //🗖
            SelectedConfig::Multi => "Multi Repository",
            SelectedConfig::Diff => "Tree Diff",
            SelectedConfig::Tracking => "Immediate Tracking",
            SelectedConfig::LongTracking => "Tracking",
            SelectedConfig::Aspects => "Aspects",
        }
    }

    pub(crate) const fn enabled(&self) -> bool {
        match self {
            SelectedConfig::Single => true,
            SelectedConfig::Querying => true,
            SelectedConfig::Tsg => true,
            SelectedConfig::Smells => true,
            SelectedConfig::Multi => false,
            SelectedConfig::Diff => false,
            SelectedConfig::Tracking => false,
            SelectedConfig::LongTracking => true,
            SelectedConfig::Aspects => true,
        }
    }

    pub(crate) fn on_hover_show(&self, ui: &mut egui::Ui) {
        ui.markdown_ui(self.descriptions())
    }

    pub(crate) fn descriptions(&self) -> &str {
        match self {
            SelectedConfig::Single => "Show code metrics over commits of a given repository",
            SelectedConfig::Querying => r#"Search code evolutions through pattern queries."#,
            SelectedConfig::Smells => {
                "Search for problematic code patterns, by focussing on code removals (specialization of the querying)."
            }
            SelectedConfig::Tsg => {
                r#"Graph computed using the [tree-sitter-graph DSL](https://docs.rs/tree-sitter-graph/latest/tree_sitter_graph/reference/index.html)."#
            }
            SelectedConfig::Multi => "Show code metrics over commits of a multiple repository.",
            SelectedConfig::Diff => "Tree diffs between a pair of commits.",
            SelectedConfig::Tracking => "Code tracking between a pair of commits.",
            SelectedConfig::LongTracking => {
                "Code tracking through a development history, handles moves and can stop on modifications to the tracked code but also to its parents."
            }
            SelectedConfig::Aspects => {
                "Explore how code can be rendered in this GUI. Once you save a render, you can configure other views with it."
            }
        }
    }
}

impl Default for Repo {
    fn default() -> Self {
        Self {
            // user: "INRIA".to_string(),
            // name: "spoon".to_string(),
            user: "official-stockfish".to_string(),
            name: "Stockfish".to_string(),
        }
    }
}

impl From<[&str; 2]> for Repo {
    fn from(value: [&str; 2]) -> Self {
        let [user, name] = value.map(|s| s.to_string());
        Self { user, name }
    }
}

impl From<&ComputeConfigAspectViews> for SelectedConfig {
    fn from(_: &ComputeConfigAspectViews) -> Self {
        Self::Aspects
    }
}

#[derive(Default, Clone)]
pub struct Languages;

impl egui_addon::Languages for Languages {
    fn get(&self, lang: &str) -> Option<egui_addon::Lang> {
        match lang {
            #[cfg(not(target_arch = "wasm32"))]
            "JavaScript" => Some(egui_addon::Lang {
                name: "JavaScript".into(),
                #[cfg(feature = "ts_highlight")]
                lang: {
                    let l: tree_sitter::Language = tree_sitter_javascript::LANGUAGE.into();
                    l
                },
            }),
            _ => None,
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum QueriedLang {
    Cpp,
    Java,
}
impl QueriedLang {
    #[allow(unused)]
    pub fn as_str(&self) -> &str {
        match self {
            QueriedLang::Cpp => "Cpp",
            QueriedLang::Java => "Java",
        }
    }
}

pub(crate) trait WithDesc<T> {
    fn desc(&self) -> &T;
}

#[derive(
    serde::Deserialize, serde::Serialize, autosurgeon::Hydrate, autosurgeon::Reconcile, Clone, Debug,
)]
#[serde(default)]
pub(crate) struct CodeEditors<T = code_editor::CodeEditor<Languages>> {
    pub(crate) description: T,
    pub(crate) init: T,
    pub(crate) filter: T,
    pub(crate) accumulate: T,
}

#[derive(
    serde::Deserialize, serde::Serialize, autosurgeon::Hydrate, autosurgeon::Reconcile, Clone, Debug,
)]
#[serde(default)]
pub(crate) struct QueryEditor<T = code_editor::CodeEditor<Languages>> {
    pub(crate) description: T,
    pub(crate) query: T,
}

pub trait EditorHolder {
    type Item;
    fn iter_editors_mut(&mut self) -> impl Iterator<Item = &mut Self::Item>;
}

#[derive(
    serde::Deserialize, serde::Serialize, autosurgeon::Hydrate, autosurgeon::Reconcile, Clone, Debug,
)]
#[serde(default)]
pub(crate) struct TsgEditor<T = code_editor::CodeEditor<Languages>> {
    pub(crate) description: T,
    pub(crate) query: T,
}

#[derive(serde::Deserialize, serde::Serialize, Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Config {
    Any,
    MavenJava,
    MakeCpp,
}

impl Config {
    #[allow(unused)]
    pub fn language(&self) -> &'static str {
        match self {
            Config::Any => "",
            Config::MavenJava => "Java",
            Config::MakeCpp => "Cpp",
        }
    }
}
impl Config {
    pub(crate) fn show_combo_box(
        &mut self,
        ui: &mut egui::Ui,
        label: impl Into<egui::WidgetText>,
    ) -> egui::InnerResponse<std::option::Option<()>> {
        egui::ComboBox::from_label(label)
            .selected_text(format!("{:?}", self))
            .show_ui(ui, |ui| {
                ui.selectable_value(self, super::types::Config::Any, "Any");
                ui.selectable_value(self, super::types::Config::MavenJava, "Java");
                ui.selectable_value(self, super::types::Config::MakeCpp, "Cpp");
            })
    }
}
