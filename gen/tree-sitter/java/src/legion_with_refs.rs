//! fully compress all subtrees from a Java CST
use num::ToPrimitive;
use std::fmt::Debug;
use std::marker::PhantomData;

use hyperast::cyclomatic::Mcc;
use hyperast::full::FullNode;
use hyperast::hashed::{HashedNode, IndexingHashBuilder, MetaDataHashsBuilder};
use hyperast::hashed::{SyntaxNodeHashs, SyntaxNodeHashsKinds};
use hyperast::nodes::Space;
use hyperast::store::SimpleStores;
use hyperast::store::defaults::LabelIdentifier;
use hyperast::store::nodes::legion::DedupMap;
use hyperast::store::nodes::legion::HashedNodeRef;
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::store::nodes::legion::{eq_node, subtree_builder};
use hyperast::tree_gen;
use hyperast::tree_gen::TsType;
use hyperast::tree_gen::parser::{Node, TreeCursor};
use hyperast::tree_gen::utils_ts::TTreeCursor;
use hyperast::tree_gen::{AccIndentation, Accumulator, WithByteRange};
use hyperast::tree_gen::{BasicAccumulator, RoleAcc, SubTreeMetrics};
use hyperast::tree_gen::{Parents, PreResult};
use hyperast::tree_gen::{SpacedGlobalData, StatsGlobalData, TextedGlobalData};
use hyperast::tree_gen::{TreeGen, ZippedTreeGen};
use hyperast::tree_gen::{compute_indentation, get_spacing};
use hyperast::types::LabelStore as LabelStoreTrait;
use hyperast::types::{AnyType, NodeStoreExt, TypeTrait, WithHashs, WithStats};
use hyperast::types::{Role, RoleStore};

use crate::TNode;
use crate::TStore;
use crate::Type;
use crate::types::impls::JavaEnabledTypeStore;

#[cfg(feature = "impact")]
mod reference_analysis;
// use reference_analysis::build_ana;
#[cfg(feature = "impact")]
pub use reference_analysis::add_md_ref_ana;

#[cfg(feature = "impact")]
pub use crate::impact::partial_analysis::PartialAnalysis;
#[cfg(not(feature = "impact"))]
#[derive(Debug, Clone)]
pub struct PartialAnalysis;
impl PartialAnalysis {
    pub fn init<F: FnMut(&str) -> LabelIdentifier>(
        _kind: &Type,
        _label: Option<&str>,
        mut _intern_label: F,
    ) -> Self {
        Self
    }
}

pub type FNode = FullNode<StatsGlobalData, Local>;

// TODO try to use a const generic for spaceless generation ?
// SPC: consider spaces ie. add them to the HyperAST,
// NOTE there is a big issue with the byteLen of subtree then.
// just provide a view abstracting spaces (see attempt in hyper_diff)
pub struct JavaTreeGen<
    'stores,
    'cache,
    TS = TStore,
    S = SimpleStores<TS>,
    More = (),
    const HIDDEN_NODES: bool = true,
> {
    // TODO replace with Arc<[u8]>
    pub line_break: Vec<u8>,
    pub dedup: Option<&'stores mut DedupMap>,
    pub stores: &'stores mut S,
    pub md_cache: &'cache mut MDCache,
    pub more: More,
    pub _p: PhantomData<TS>,
}

pub type MDCache = hashbrown::HashMap<NodeIdentifier, MD>;

// NOTE only keep compute intensive metadata (where space/time trade off is worth storing)
// eg. decls refs, maybe hashes but not size and height
// * metadata: computation results from concrete code of node and its children
// they can be qualitative metadata .eg a hash or they can be quantitative .eg lines of code
pub struct MD {
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    #[cfg(feature = "impact")]
    pub ana: Option<PartialAnalysis>,
    pub mcc: Mcc,
    pub precomp_queries: PrecompQueries,
}

impl MD {
    pub fn local(&self, compressed_node: NodeIdentifier) -> Local {
        Local {
            compressed_node,
            metrics: self.metrics,
            #[cfg(feature = "impact")]
            ana: self.ana.clone(),
            mcc: self.mcc.clone(),
            role: None,
            precomp_queries: self.precomp_queries,
            stmt_count: 0,
            member_import_count: 0,
        }
    }
}

// Enables static reference analysis
#[cfg(feature = "impact")]
const ANA: bool = false;

impl From<Local> for MD {
    fn from(x: Local) -> Self {
        MD {
            metrics: x.metrics,
            #[cfg(feature = "impact")]
            ana: x.ana,
            mcc: x.mcc,
            precomp_queries: x.precomp_queries,
        }
    }
}

pub type Global<'a> = SpacedGlobalData<'a, StatsGlobalData>;

type PrecompQueries = u16;

#[derive(Debug, Clone)]
pub struct Local {
    pub compressed_node: NodeIdentifier,
    // * metadata: computation results from concrete code of node and its children
    // they can be qualitative metadata, e.g. a hash or they can be quantitative e.g. lines of code
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    #[cfg(feature = "impact")]
    pub ana: Option<PartialAnalysis>,
    pub mcc: Mcc,
    pub role: Option<Role>,
    pub precomp_queries: PrecompQueries,
    pub stmt_count: u8,
    pub member_import_count: u8,
}

impl Local {
    fn acc<Scope>(self, acc: &mut Acc<Scope>) {
        if self.metrics.size_no_spaces > 0 {
            acc.no_space.push(self.compressed_node)
        }
        let o = acc.simple.children.len();
        if let Some(role) = self.role {
            acc.role.acc(role, o);
        }
        acc.simple.push(self.compressed_node);
        acc.metrics.acc(self.metrics);
        acc.precomp_queries |= self.precomp_queries;
        acc.stmt_count = acc.stmt_count.saturating_add(self.stmt_count);
        acc.member_import_count = acc
            .member_import_count
            .saturating_add(self.member_import_count);

        #[cfg(feature = "impact")]
        if let Some(s) = self.ana {
            // TODO use to simplify when stabilized
            // s.acc(&acc.simple.kind,acc.ana.get_or_insert_default());
            if let Some(aaa) = &mut acc.ana {
                s.acc(&acc.simple.kind, aaa);
            } else {
                let mut aaa = Default::default();
                s.acc(&acc.simple.kind, &mut aaa);
                acc.ana = Some(aaa);
            }
        }
        self.mcc.acc(&mut acc.mcc)
    }
}

pub struct Acc<Scope = hyperast::scripting::Acc> {
    simple: BasicAccumulator<Type, NodeIdentifier>,
    no_space: Vec<NodeIdentifier>,
    labeled: bool,
    start_byte: usize,
    end_byte: usize,
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    #[cfg(feature = "impact")]
    ana: Option<PartialAnalysis>,
    mcc: Mcc,
    padding_start: usize,
    indentation: tree_gen::Spaces,
    role: RoleAcc<crate::types::Role>,
    precomp_queries: PrecompQueries,
    prepro: Option<Scope>,
    stmt_count: u8,
    member_import_count: u8,
}

impl<Scope> Accumulator for Acc<Scope> {
    type Node = FNode;
    fn push(&mut self, full_node: Self::Node) {
        full_node.local.acc(self);
    }
}

impl<Scope> AccIndentation for Acc<Scope> {
    fn indentation(&self) -> &tree_gen::Spaces {
        &self.indentation
    }
}

impl<Scope> WithByteRange for Acc<Scope> {
    fn has_children(&self) -> bool {
        !self.simple.children.is_empty()
    }

    fn begin_byte(&self) -> usize {
        self.start_byte
    }

    fn end_byte(&self) -> usize {
        self.end_byte
    }
}

impl hyperast::types::Typed for Acc {
    type Type = Type;

    fn get_type(&self) -> Self::Type {
        self.simple.kind
    }
}

impl tree_gen::WithChildren<NodeIdentifier> for Acc {
    fn children(&self) -> &[NodeIdentifier] {
        &self.simple.children
    }
}

impl tree_gen::WithRole<Role> for Acc {
    fn role_at(&self, o: usize) -> Option<Role> {
        self.role
            .offsets
            .iter()
            .position(|x| *x as usize == o)
            .and_then(|x| self.role.roles.get(x))
            .cloned()
    }
}

impl<'acc> tree_gen::WithLabel for &'acc Acc {
    type L = &'acc str;
}

impl<S> Debug for Acc<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("Acc");
        f.field("simple", &self.simple)
            .field("no_space", &self.no_space)
            .field("labeled", &self.labeled)
            .field("start_byte", &self.start_byte)
            .field("end_byte", &self.end_byte)
            .field("metrics", &self.metrics)
            .field("mcc", &self.mcc)
            .field("padding_start", &self.padding_start)
            .field("indentation", &self.indentation);
        #[cfg(feature = "impact")]
        f.field("ana", &self.ana);
        f.finish()
    }
}

/// Implements [`ZippedTreeGen`] to offer a visitor for Java generation
impl<TS, More, const HIDDEN_NODES: bool> ZippedTreeGen
    for JavaTreeGen<'_, '_, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS: JavaEnabledTypeStore + 'static + RoleStore<Role = Role, IdF = u16>,
    More: tree_gen::Prepro<SimpleStores<TS>>
        + tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc<More::Scope>>,
{
    type Stores = SimpleStores<TS>;
    type Text = [u8];
    type Node<'b> = TNode<'b>;
    type TreeCursor<'b> = TTreeCursor<'b, HIDDEN_NODES>;

    fn stores(&mut self) -> &mut Self::Stores {
        self.stores
    }

    fn init_val(&mut self, text: &[u8], node: &Self::Node<'_>) -> Self::Acc {
        let kind = TS::obtain_type(node);
        let parent_indentation = Space::try_format_indentation(&self.line_break)
            .unwrap_or_else(|| vec![Space::Space; self.line_break.len()]);
        let indent = compute_indentation(
            &self.line_break,
            text,
            node.start_byte(),
            0,
            &parent_indentation,
        );
        let labeled = node.has_label();
        #[cfg(feature = "impact")]
        let ana = self.build_ana(&kind);
        let mcc = Mcc::new(&kind);
        let prepro = if More::USING {
            Some(self.more.preprocessing(kind).unwrap())
        } else {
            None
        };
        Acc {
            simple: BasicAccumulator::new(kind),
            no_space: vec![],
            labeled,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            #[cfg(feature = "impact")]
            ana,
            mcc,
            padding_start: 0,
            indentation: indent,
            role: Default::default(),
            precomp_queries: Default::default(),
            prepro,
            stmt_count: kind.is_statement() as u8,
            member_import_count: (kind == Type::Import || kind.is_member()) as u8,
        }
    }

    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> PreResult<Self::Acc> {
        let node = cursor.node();
        let Some(kind) = TS::try_obtain_type(&node) else {
            return PreResult::Skip;
        };
        if !HIDDEN_NODES {
            if kind == Type::_UnannotatedType
                || kind == Type::_VariableDeclaratorList
                || kind == Type::_VariableDeclaratorId
            {
                return PreResult::Ignore;
            } else if kind.is_repeat() {
                // if stack.parent().unwrap().simple.children.len() < 256 {
                return PreResult::Ignore;
                // }
            } else if kind == Type::_Literal {
                // return PreResult::Ignore;
            } else if !kind.is_supertype() && kind.is_hidden() {
                return PreResult::Ignore;
            }
        }
        if (!HIDDEN_NODES && kind.is_hidden()) || kind.is_repeat() {
            return PreResult::Ignore;
        }
        if node.0.is_missing() {
            log::trace!(
                "Missing node: {:?} {}-{}",
                kind,
                node.start_byte(),
                node.end_byte()
            );

            // must skip missing nodes, i.e., leafs added by tree-sitter to fix CST,
            // needed to avoid breaking invariant, as the node has no span:
            // `is_parent_hidden && parent.end_byte() <= acc.begin_byte()`
            return PreResult::Skip;
        }
        if kind.is_hidden() && node.start_byte() == node.end_byte() {
            log::trace!(
                "Ignoring empty hidden node: {:?} {}-{}",
                kind,
                node.start_byte(),
                node.end_byte()
            );
            return PreResult::Ignore;
        }
        let mut acc = self.pre(text, &node, stack, global);
        // TODO replace with wrapper
        if !stack.parent().is_some_and(|a| a.simple.kind.is_supertype()) {
            if let Some(r) = cursor.0.field_name() {
                if let Ok(r) = r.try_into() {
                    acc.role.current = Some(r);
                } else {
                    log::error!("cannot convert role: {}", r)
                }
            }
        }
        if kind.is_leaf() {
            acc.labeled = true;
            return PreResult::SkipChildren(acc);
        }
        PreResult::Ok(acc)
    }

    fn pre(
        &mut self,
        text: &[u8],
        node: &Self::Node<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> Self::Acc {
        let parent_indentation = &stack.parent().unwrap().indentation();
        let kind = TS::obtain_type(node);
        assert!(
            global.sum_byte_length() <= node.start_byte(),
            "{}: {} <= {}",
            kind,
            global.sum_byte_length(),
            node.start_byte()
        );
        let indent = compute_indentation(
            &self.line_break,
            text,
            node.start_byte(),
            global.sum_byte_length(),
            parent_indentation,
        );
        let prepro = if More::USING {
            Some(self.more.preprocessing(kind).unwrap())
        } else {
            None
        };
        Acc {
            labeled: node.has_label(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            #[cfg(feature = "impact")]
            ana: self.build_ana(&kind),
            mcc: Mcc::new(&kind),
            padding_start: global.sum_byte_length(),
            indentation: indent,
            simple: BasicAccumulator::new(kind),
            no_space: vec![],
            role: Default::default(),
            precomp_queries: Default::default(),
            prepro,
            stmt_count: kind.is_statement() as u8,
            member_import_count: (kind == Type::Import || kind.is_member()) as u8,
        }
    }

    fn acc(&mut self, parent: &mut Self::Acc, full_node: <Self::Acc as Accumulator>::Node) {
        let id = full_node.local().compressed_node;
        let ty = parent.simple.kind;
        parent.push(full_node);
        if let Some(p) = &mut parent.prepro {
            // SAFETY: this side should be fine, issue when unerasing
            let store = unsafe { self.stores.erase_ts_unchecked() };
            let child: hyperast::scripting::SubtreeHandle<crate::types::TType> = id.into();
            use hyperast::scripting::Accumulable;
            p.acc(self.more.scripts(), store, ty, child).unwrap();
        }
    }

    fn _post(
        &mut self,
        parent: &mut Self::Acc,
        global: &mut Self::Global,
        text: &[u8],
        acc: Self::Acc,
    ) {
        let spacing = get_spacing(acc.padding_start, acc.start_byte, text);
        if let Some(spacing) = spacing {
            self.make_space(global, &spacing, parent);
        }
        let node = self.post(|n| parent.push(n), global, text, acc);
        parent.push(node);
    }

    fn post(
        &mut self,
        _acc_node: impl FnMut(<Self::Acc as Accumulator>::Node),
        global: &mut Self::Global,
        text: &Self::Text,
        acc: Self::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let label = if acc.labeled {
            std::str::from_utf8(&text[acc.start_byte..acc.end_byte])
                .ok()
                .map(|x| x.to_string())
        } else {
            None
        };
        self.make(global, acc, label)
    }
}

pub fn tree_sitter_parse(text: &[u8]) -> tree_sitter::Tree {
    tree_gen::utils_ts::tree_sitter_parse(text, &crate::language())
}

impl<'stores, 'cache, TS: JavaEnabledTypeStore, X>
    JavaTreeGen<'stores, 'cache, TS, SimpleStores<TS>, tree_gen::NoOpMore<X, Acc>, true>
{
    pub fn new(stores: &'stores mut SimpleStores<TS>, md_cache: &'cache mut MDCache) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: None,
            stores,
            md_cache,
            more: Default::default(),
            _p: Default::default(),
        }
    }
}

impl<'stores, 'cache, TS, More> JavaTreeGen<'stores, 'cache, TS, SimpleStores<TS>, More, true> {
    pub fn without_hidden_nodes(
        self,
    ) -> JavaTreeGen<'stores, 'cache, TS, SimpleStores<TS>, More, false> {
        JavaTreeGen {
            line_break: self.line_break,
            dedup: self.dedup,
            stores: self.stores,
            md_cache: self.md_cache,
            more: self.more,
            _p: self._p,
        }
    }
}

impl<'stores, 'cache, TS: JavaEnabledTypeStore + 'static, More>
    JavaTreeGen<'stores, 'cache, TS, SimpleStores<TS>, More, true>
{
    pub fn with_preprocessing(
        stores: &'stores mut SimpleStores<TS>,
        md_cache: &'cache mut MDCache,
        more: More,
    ) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: None,
            stores,
            md_cache,
            more,
            _p: Default::default(),
        }
    }
}
impl<'stores, 'cache, TS: JavaEnabledTypeStore + 'static, More>
    JavaTreeGen<'stores, 'cache, TS, SimpleStores<TS>, More, true>
{
    /// Replaces the default dedup map when deriving different data.
    /// Be cautious when replacing the default dedup map,
    /// as it breaks the referential equlity being equivalent to the structural equality.
    /// Otherwise, everything else is great !
    /// In the future use multiple dedup maps when we can guarantee valid nesting,
    ///   e.g., additional Derived Data on files can reuse subtrees of things inside files, but directories and files must be added without merging with the others
    ///    (it should also be possible to compute markers to provide similar guarantees, e.g. a DD only on classes can reuse children that do not contain classes).
    /// Could also make the eq consider the derived data, but to avoid breaking the incrementality we would still need some kind of marker for each context
    pub fn with_preprocessing_and_dedup(
        stores: &'stores mut SimpleStores<TS>,
        dedup: &'stores mut DedupMap,
        md_cache: &'cache mut MDCache,
        more: More,
    ) -> Self {
        Self {
            line_break: "\n".as_bytes().to_vec(),
            dedup: Some(dedup),
            stores,
            md_cache,
            more,
            _p: Default::default(),
        }
    }
}

impl<'stores, 'cache, TS: JavaEnabledTypeStore + 'static, More, const HIDDEN_NODES: bool>
    JavaTreeGen<'stores, 'cache, TS, SimpleStores<TS>, More, HIDDEN_NODES>
{
    pub fn set_line_break(self, line_break: Vec<u8>) -> Self {
        JavaTreeGen {
            line_break,
            dedup: self.dedup,
            stores: self.stores,
            md_cache: self.md_cache,
            more: self.more,
            _p: self._p,
        }
    }
}

impl<'stores, TS, More, const HIDDEN_NODES: bool>
    JavaTreeGen<'stores, '_, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS: JavaEnabledTypeStore<Ty2 = Type> + 'static + RoleStore<Role = Role, IdF = u16>,
    More: tree_gen::Prepro<SimpleStores<TS>>
        + tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc<More::Scope>>,
{
    pub(crate) fn make_space(
        &mut self,
        global: &<Self as TreeGen>::Global,
        spacing: &[u8],
        acc: &mut <Self as TreeGen>::Acc,
    ) {
        let full_node = FullNode {
            global: global.simple(),
            local: self.make_spacing(spacing),
        };
        self.acc(acc, full_node);
    }

    fn make_spacing(&mut self, spacing: &[u8]) -> Local {
        let kind = Type::Spaces;
        let interned_kind = TS::intern(kind);
        debug_assert_eq!(kind, TS::resolve(interned_kind));

        let spacing = std::str::from_utf8(&spacing).unwrap().to_string();

        let dedup = &mut self.stores.node_store.dedup;
        let dedup = self.dedup.as_mut().map_or(dedup, |x| &mut x.0);
        let node_store = &mut self.stores.node_store.inner;
        let label_store = &mut self.stores.label_store;
        let line_break = &self.line_break;
        let more = |dyn_builder: &mut _| {
            if More::USING {
                let prepro = self.more.preprocessing(Type::Spaces).unwrap();
                let subtr = hyperast::scripting::Subtr(kind, &dyn_builder);
                use hyperast::scripting::Finishable;
                let scripts = self.more.scripts();
                let ss = prepro.finish_with_label(scripts, &subtr, &spacing).unwrap();
                dyn_builder.add(ss);
            };
        };
        let (compressed_node, metrics) = tree_gen::utils_ts::make_leaf::<TS>(
            node_store,
            label_store,
            dedup,
            line_break,
            interned_kind,
            &spacing,
            more,
        );
        debug_assert!(
            hyperast::store::nodes::PolyglotHolder::lang_id(
                &self.stores.node_store.resolve(compressed_node)
            )
            .is::<crate::types::Lang>(),
            "wrong lang id for node kind: {}",
            kind
        );
        Local {
            compressed_node,
            metrics,
            #[cfg(feature = "impact")]
            ana: Default::default(),
            mcc: Mcc::new(&kind),
            role: None,
            precomp_queries: Default::default(),
            stmt_count: 0,
            member_import_count: 0,
        }
    }

    pub fn tree_sitter_parse(text: &[u8]) -> Result<tree_sitter::Tree, tree_sitter::Tree> {
        let mut parser = tree_sitter::Parser::new();
        let language = crate::language();
        parser.set_language(&language).unwrap();
        let tree = parser.parse(text, None).unwrap();
        if tree.root_node().has_error() {
            Err(tree)
        } else {
            Ok(tree)
        }
    }

    pub fn generate_file(
        &mut self,
        name: &[u8],
        text: &'stores [u8],
        cursor: tree_sitter::TreeCursor,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let mut global = Global::from(TextedGlobalData::new(Default::default(), text));
        let init = self.init_val(text, &TNode(cursor.node()));
        let xx = TTreeCursor(cursor);
        use hyperast::tree_gen::_handle_file_bounds;
        let mut acc = _handle_file_bounds(self, text, xx, &mut global, init, |slf, g, s, acc| {
            if tree_gen::validate_spacing(s) {
                Self::make_space(slf, g, s, acc)
            } else {
                log::warn!("invalid spacing: {:?}", s);
                Self::make_space(slf, g, s, acc)
            }
        });

        let label = Some(std::str::from_utf8(name).unwrap().to_owned());

        use hyperast::types::HyperType;
        if !acc.simple.kind.is_file() {
            log::warn!("ignoring parsing error at the root of the file");
            acc.simple.kind = Type::Program;
        }

        let full_node = self.make(&mut global, acc, label);

        #[cfg(feature = "impact")]
        match full_node.local.ana.as_ref() {
            Some(x) => {
                log::debug!("refs in file:",);
                for x in x.display_refs(&self.stores.label_store) {
                    log::debug!("    {}", x);
                }
                log::debug!("decls in file:",);
                for x in x.display_decls(&self.stores.label_store) {
                    log::debug!("    {}", x);
                }
            }
            None => log::debug!("None"),
        };

        full_node
    }

    #[cfg(feature = "impact")]
    fn build_ana(&mut self, _kind: &Type) -> Option<PartialAnalysis> {
        if !ANA {
            return None;
        }
        let _label_store = &mut self.stores.label_store;
        #[cfg(feature = "impact")]
        {
            build_ana(kind, label_store)
        }
        #[cfg(not(feature = "impact"))]
        {
            None
        }
    }
}

impl<'stores, TS, More, const HIDDEN_NODES: bool> TreeGen
    for JavaTreeGen<'stores, '_, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS: JavaEnabledTypeStore + 'static + RoleStore<Role = Role, IdF = u16>,
    More: tree_gen::Prepro<SimpleStores<TS>>
        + tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc<More::Scope>>,
{
    type Acc = Acc<More::Scope>;
    type Global = Global<'stores>;
    fn make(
        &mut self,
        global: &mut Self::Global,
        mut acc: Self::Acc,
        label: Option<String>,
    ) -> <Self::Acc as Accumulator>::Node {
        let stores = &mut self.stores;
        let more = &mut self.more;
        let kind = acc.simple.kind;
        let interned_kind = TS::intern(kind);
        let metrics = acc.metrics.finalize(&interned_kind, &label);

        let hashable = &metrics.hashs.most_discriminating();

        let label_id = label.as_deref().map(|label| {
            // Some notable type can contain very different labels,
            // they might benefit from a particular storing (like a blob storage, even using git's object database )
            // eg. acc.simple.kind == Type::Comment and acc.simple.kind.is_literal()
            stores.label_store.get_or_insert(label)
        });
        let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

        let node_store = &mut stores.node_store;
        #[cfg(feature = "subtree-stats")]
        (node_store.inner.stats()).add_height_non_dedup(metrics.height);

        let dedup = self.dedup.as_mut();
        let dedup = dedup.map_or(&mut node_store.dedup, |x| &mut x.0);
        let insertion = node_store.inner.prepare_insertion(dedup, hashable, eq);

        let local = if let Some(compressed_node) = insertion.occupied_id() {
            let md = self.md_cache.get(&compressed_node).unwrap();
            Local {
                role: acc.role.current,
                stmt_count: acc.stmt_count,
                member_import_count: acc.member_import_count,
                ..md.local(compressed_node)
            }
        } else {
            use hyperast::store::nodes::compo;
            #[cfg(feature = "impact")]
            reference_analysis::make_partial_ana(
                acc.simple.kind,
                &mut acc.ana,
                &label,
                &acc.simple.children,
                &mut stores.label_store,
                &insertion,
            );
            let mut metrics = metrics.map_hashs(|h| h.build());
            let own_line_count = tree_gen::newline_count(&label);
            metrics.line_count += own_line_count;

            let byte_len = (acc.end_byte - acc.start_byte).try_into().unwrap();
            let bytes_len = compo::BytesLen(byte_len);
            let vacant = insertion.vacant();
            let node_store: &_ = vacant.1.1;
            let stores = SimpleStores {
                type_store: stores.type_store,
                label_store: &stores.label_store,
                node_store,
            };
            let children_is_empty = acc.simple.children.is_empty();

            let mut dyn_builder = subtree_builder::<TS>(interned_kind);
            dyn_builder.add(bytes_len);

            if More::ENABLED {
                acc.precomp_queries |= more.match_precomp_queries(stores, &acc, label.as_deref());
                tree_gen::add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries);
            }
            if More::GRAPHING {
                more.compute_tsg(stores, &acc, label.as_deref()).unwrap();
            }

            if acc.stmt_count > 0 {
                dyn_builder.add(compo::StmtCount(acc.stmt_count));
            }
            if acc.member_import_count > 0 {
                dyn_builder.add(compo::MemberImportCount(acc.member_import_count));
            }

            let current_role = Option::take(&mut acc.role.current);
            acc.role.add_md(&mut dyn_builder);

            if Mcc::persist(&acc.simple.kind) {
                dyn_builder.add(acc.mcc.clone());
            }
            #[cfg(feature = "impact")]
            reference_analysis::add_md_ref_ana(
                &mut dyn_builder,
                children_is_empty,
                acc.ana.as_ref(),
            );
            #[cfg(feature = "subtree-stats")]
            (vacant.1.1.stats()).add_height_dedup(metrics.height, metrics.hashs);
            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            if acc.simple.children.len() != acc.no_space.len() {
                let children = acc.no_space;
                tree_gen::add_cs_no_spaces(&mut dyn_builder, children);
            }

            acc.simple
                .add_primary(&mut dyn_builder, interned_kind, label_id);

            if More::USING {
                let subtr = hyperast::scripting::Subtr(kind, &dyn_builder);
                use hyperast::scripting::Finishable;
                let ss = if let Some(label) = &label {
                    acc.prepro
                        .unwrap()
                        .finish_with_label(more.scripts(), &subtr, label)
                        .unwrap()
                } else {
                    acc.prepro.unwrap().finish(more.scripts(), &subtr).unwrap()
                };
                dyn_builder.add(ss);
            }

            let compressed_node = vacant.insert_built(dyn_builder.build());

            // TODO see if possible to only keep md in md_cache, but would need a generational cache I think
            self.md_cache.insert(
                compressed_node,
                MD {
                    metrics,
                    #[cfg(feature = "impact")]
                    ana: acc.ana.clone(),
                    mcc: acc.mcc.clone(),
                    precomp_queries: acc.precomp_queries,
                },
            );
            Local {
                compressed_node,
                metrics,
                #[cfg(feature = "impact")]
                ana: acc.ana,
                mcc: acc.mcc,
                role: current_role,
                precomp_queries: acc.precomp_queries,
                stmt_count: acc.stmt_count,
                member_import_count: acc.member_import_count,
            }
        };

        FullNode {
            global: global.simple(),
            local,
        }
    }
}

impl<
    TS: JavaEnabledTypeStore + 'static + RoleStore<Role = Role, IdF = u16>,
    More: tree_gen::Prepro<SimpleStores<TS>, Scope = hyperast::scripting::Acc>
        + tree_gen::PreproTSG<SimpleStores<TS>, Acc = Acc<More::Scope>>,
    const HIDDEN_NODES: bool,
> NodeStoreExt<HashedNode> for JavaTreeGen<'_, '_, TS, SimpleStores<TS>, More, HIDDEN_NODES>
where
    TS::Ty: TypeTrait,
{
    #[allow(unused)]
    fn build_then_insert(
        &mut self,
        i: <HashedNode as hyperast::types::Stored>::TreeId,
        t: AnyType, //<HashedNode as types::Typed>::Type,
        l: Option<<HashedNode as hyperast::types::Labeled>::Label>,
        cs: Vec<<HashedNode as hyperast::types::Stored>::TreeId>,
    ) -> <HashedNode as hyperast::types::Stored>::TreeId {
        todo!();
        // if t.is_spaces() {
        //     //     // TODO improve ergonomics
        //     //     // should ge spaces as label then reconstruct spaces and insert as done with every other nodes
        //     //     // WARN it wont work for new spaces (l parameter is not used, and label do not return spacing)
        //     let spacing = self
        //         .stores
        //         .label_store
        //         .resolve(&l.unwrap())
        //         .as_bytes()
        //         .to_vec();
        //     self.make_spacing(spacing);
        //     return i;
        // }
        let mut acc: Acc = {
            let kind = t;
            let kind = todo!();
            let prepro = todo!(); //self.more.preprocessing(&*self.stores,kind).unwrap();
            Acc {
                labeled: l.is_some(),
                start_byte: 0,
                end_byte: 0,
                metrics: Default::default(),
                #[cfg(feature = "impact")]
                ana: None,
                mcc: Mcc::new(&kind),
                padding_start: 0,
                indentation: vec![],
                simple: BasicAccumulator {
                    kind,
                    children: vec![],
                },
                no_space: vec![],
                role: Default::default(),
                precomp_queries: Default::default(),
                prepro,
                stmt_count: 0,
                member_import_count: 0,
            }
        };
        for c in cs {
            let local = {
                // print_tree_syntax(&self.stores.node_store, &self.stores.label_store, &c);
                // println!();
                let md = self.md_cache.get(&c);
                #[cfg(feature = "impact")]
                panic!("does not support impact analysis in and code modifications");
                let (metrics, mcc) = if let Some(md) = md {
                    let metrics = md.metrics;
                    let mcc = md.mcc.clone();
                    (metrics, mcc)
                } else {
                    let node: HashedNodeRef<_> = self.stores.node_store.resolve(c);
                    let hashs = SyntaxNodeHashs {
                        structt: WithHashs::hash(&node, SyntaxNodeHashsKinds::Struct),
                        label: WithHashs::hash(&node, SyntaxNodeHashsKinds::Label),
                        syntax: WithHashs::hash(&node, SyntaxNodeHashsKinds::Syntax),
                    };
                    let kind: TS::Ty = todo!(); //node.get_type();
                    let metrics = SubTreeMetrics {
                        size: node.size().to_u32().unwrap(),
                        height: node.height().to_u32().unwrap(),
                        size_no_spaces: node.size_no_spaces().to_u32().unwrap(),
                        hashs,
                        line_count: node.line_count().to_u32().unwrap(),
                    };
                    let mcc = node
                        .get_component::<Mcc>()
                        .map_or(Mcc::new(&kind), |x| x.clone());
                    (metrics, mcc)
                };
                Local {
                    compressed_node: c,
                    metrics,
                    #[cfg(feature = "impact")]
                    ana: None,
                    mcc,
                    role: acc.role.current,
                    precomp_queries: todo!(),
                    stmt_count: acc.stmt_count,
                    member_import_count: acc.member_import_count,
                }
            };
            let global = StatsGlobalData::default();
            let full_node = FullNode { global, local };
            acc.push(full_node);
        }
        let post = {
            let more = &mut self.more;
            let node_store = &mut self.stores.node_store.inner;
            let label_store = &mut self.stores.label_store;
            let label_id = l;
            let label = label_id.map(|l| label_store.resolve(&l));

            let interned_kind = TS::intern(acc.simple.kind);
            let metrics = acc.metrics.finalize(&interned_kind, &label);

            let hsyntax = metrics.hashs.most_discriminating();
            let hashable = &hsyntax;

            let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

            let dedup = &mut self.stores.node_store.dedup;
            let dedup = self.dedup.as_mut().map_or(dedup, |x| &mut x.0);
            let insertion = node_store.prepare_insertion(dedup, &hashable, eq);

            if let Some(id) = insertion.occupied_id() {
                let md = self.md_cache.get(&id).unwrap();
                Local {
                    compressed_node: id,
                    metrics: md.metrics,
                    #[cfg(feature = "impact")]
                    ana: md.ana.clone(),
                    mcc: md.mcc.clone(),
                    role: acc.role.current,
                    precomp_queries: md.precomp_queries,
                    stmt_count: acc.stmt_count,
                    member_import_count: acc.member_import_count,
                }
            } else {
                use hyperast::store::nodes::compo;
                let mut metrics = metrics.map_hashs(|h| h.build());
                let own_line_count = tree_gen::newline_count(&label);
                metrics.line_count += own_line_count;
                let bytes_len = compo::BytesLen((acc.end_byte - acc.start_byte) as u32);

                let vacant = insertion.vacant();
                let node_store: &_ = vacant.1.1;
                let stores = SimpleStores {
                    type_store: self.stores.type_store,
                    node_store,
                    label_store: &self.stores.label_store,
                };

                let children_is_empty = acc.simple.children.is_empty();

                let mut dyn_builder = subtree_builder::<TS>(interned_kind);
                dyn_builder.add(bytes_len);

                let current_role = Option::take(&mut acc.role.current);
                acc.role.add_md(&mut dyn_builder);
                if Mcc::persist(&acc.simple.kind) {
                    dyn_builder.add(acc.mcc.clone());
                }
                if let Some(label_id) = label_id {
                    dyn_builder.add(label_id);
                }
                if More::ENABLED {
                    acc.precomp_queries |= more.match_precomp_queries(stores, &acc, label);
                    tree_gen::add_md_precomp_queries(&mut dyn_builder, acc.precomp_queries);
                }
                if More::GRAPHING {
                    more.compute_tsg(stores, &acc, label.as_deref()).unwrap();
                }
                #[cfg(feature = "impact")]
                reference_analysis::add_md_ref_ana(
                    &mut dyn_builder,
                    children_is_empty,
                    acc.ana.as_ref(),
                );
                let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
                hashs.persist(&mut dyn_builder);
                if !children_is_empty && acc.simple.children.len() != acc.no_space.len() {
                    let children = acc.no_space;
                    tree_gen::add_cs_no_spaces(&mut dyn_builder, children);
                }
                acc.simple
                    .add_primary(&mut dyn_builder, interned_kind, label_id);
                let compressed_node = vacant.insert_built(dyn_builder.build());

                // TODO see if possible to only keep md in md_cache, but would need a generational cache I think
                self.md_cache.insert(
                    compressed_node,
                    MD {
                        metrics,
                        #[cfg(feature = "impact")]
                        ana: acc.ana.clone(),
                        mcc: acc.mcc.clone(),
                        precomp_queries: acc.precomp_queries,
                    },
                );
                Local {
                    compressed_node,
                    metrics,
                    #[cfg(feature = "impact")]
                    ana: acc.ana,
                    mcc: acc.mcc,
                    role: current_role,
                    precomp_queries: todo!(),
                    stmt_count: acc.stmt_count,
                    member_import_count: acc.member_import_count,
                }
            }
        };
        post.compressed_node
    }
}
