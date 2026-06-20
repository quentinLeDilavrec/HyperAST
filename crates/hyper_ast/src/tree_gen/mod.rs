//! # Tree Generators
//!
//! This module contains facilities to help you build an HyperAST.
//!
//! With the simplest abstraction (wrapping the next one):
//! - [`zipped_ts_extra::TsTreeGen`] wraps the next abstraction,
//!     such that, you only have to care about defining,
//!   - the node types of your syntax tree, and
//!   - your extra metrics by implementing `Extra`.
//!     - [`extra_chain::ChainedExtra`] provides chaining of extras,
//!     - [`extra_pattern_precomp::PatternPrecompExtra`] provides pattern precomputing
//! With the more advanced abstraction:
//! - [`TreeGen::make`] is where a subtree is pushed in the HyperAST
//!   - You should also use [`crate::store::nodes::legion::NodeStore::prepare_insertion`]
//!     to insert subtrees in the HyperAST while deduplicating identical ones
//! - To visit parsing output with a zipper/cursor interface you should implement [`ZippedTreeGen`]
//!   - [`crate::parser::TreeCursor`] should be implemented to wrap you parser's interface
//!
//!
//! ## Important Note
//! To make code analysis incremental in the HyperAST,
//! we locally persist locally derived values, we call them metadata.
//! To save memory, we also deduplicate identical nodes using the type, label and children of a subtree.
//! In other word, in the HyperAST, you store Metadata (derived values) along subtrees of the HyperAST,
//! and deduplicate subtree using identifying data.
//! To ensure derived data are unique per subtree,
//! metadata should only be derived from local identifying values.

pub mod parser;

use std::fmt::Debug;

use crate::hashed::NodeHashs;
use crate::nodes::Space;
use crate::store::nodes::GatherAttrErazed; // TODO rename the base trait
use crate::types::{ETypeStore, HyperAST, HyperASTShared, StoreRefAssoc};

#[cfg(feature = "ts")]
use self::parser::Visibility;

pub type Spaces = Vec<Space>;

/// Builder of a node for the hyperAST
pub trait Accumulator {
    type Node;
    fn push(&mut self, full_node: Self::Node);
}

pub trait WithByteRange {
    fn has_children(&self) -> bool;
    fn begin_byte(&self) -> usize;
    fn end_byte(&self) -> usize;
}

// TODO merge with other node traits?
pub trait WithChildren<Id: Clone> {
    fn children(&self) -> &[Id];
    fn child_count(&self) -> usize {
        let cs = self.children();
        cs.len()
    }
    fn child(&self, idx: usize) -> Option<Id> {
        let cs = self.children();
        cs.get(idx).cloned()
    }
}
// TODO merge with other node traits?
pub trait WithRole<R> {
    fn role_at(&self, idx: usize) -> Option<R>;
    fn role(&self) -> Option<R> {
        todo!()
    }
}

pub trait WithLabel {
    type L: Clone + AsRef<str>;
}

pub struct BasicAccumulator<T, Id> {
    pub kind: T,
    pub children: Vec<Id>,
}

impl<T, IdN> BasicAccumulator<T, IdN> {
    pub fn new(kind: T) -> Self {
        Self {
            kind,
            children: vec![],
        }
    }

    pub fn add_primary<L, K, EB: GatherAttrErazed>(
        self,
        dyn_builder: &mut EB,
        interned_kind: K,
        label_id: Option<L>,
    ) where
        K: crate::store::nodes::Compo,
        L: crate::store::nodes::Compo,
        IdN: 'static + Send + Sync,
    {
        use crate::store::nodes::compo;
        // TODO better handle the interneds
        // TODO the "static" interning should be hanled more specifically
        dyn_builder.add(interned_kind);
        if let Some(label_id) = label_id {
            dyn_builder.add(label_id);
        }

        let children = self.children;
        if children.len() == 1 {
            let Ok(cs) = children.try_into() else {
                unreachable!();
            };
            dyn_builder.add(compo::CS0::<_, 1>(cs));
        } else if children.len() == 2 {
            let Ok(cs) = children.try_into() else {
                unreachable!();
            };
            dyn_builder.add(compo::CS0::<_, 2>(cs));
        } else if !children.is_empty() {
            // TODO make global components, at least for primaries.
            dyn_builder.add(compo::CS(children.into_boxed_slice()));
        }
    }
}

pub fn add_cs_no_spaces<IdN: 'static + Send + Sync>(
    dyn_builder: &mut impl GatherAttrErazed,
    children: Vec<IdN>,
) {
    use crate::store::nodes::compo;
    if children.len() == 1 {
        let Ok(cs) = children.try_into() else {
            unreachable!();
        };
        dyn_builder.add(compo::NoSpacesCS0::<_, 1>(cs));
    } else if children.len() == 2 {
        let Ok(cs) = children.try_into() else {
            unreachable!();
        };
        dyn_builder.add(compo::NoSpacesCS0::<_, 2>(cs));
    } else if !children.is_empty() {
        // TODO make global components, at least for primaries.
        dyn_builder.add(compo::NoSpacesCS(children.into_boxed_slice()));
    }
}

impl<T: Debug, Id> Debug for BasicAccumulator<T, Id> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BasicAccumulator")
            .field("kind", &self.kind)
            .field("children", &self.children.len())
            .finish()
    }
}

impl<T, Id> Accumulator for BasicAccumulator<T, Id> {
    type Node = Id;
    fn push(&mut self, node: Self::Node) {
        self.children.push(node);
    }
}

/// Builder of a node aware of its indentation for the hyperAST
pub trait AccIndentation: Accumulator {
    fn indentation(&self) -> &Spaces;
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SubTreeMetrics<U> {
    pub hashs: U,
    pub size: u32,
    pub height: u32,
    pub size_no_spaces: u32,
    // pub byte_len: u32,
    /// count newlines inside labels
    pub line_count: u32, // TODO u16 is definitely not enough at the directory level e.g. 1.6MLoCs for Hadoop
}

impl<U: NodeHashs> SubTreeMetrics<U> {
    pub fn acc(&mut self, other: Self) {
        self.height = self.height.max(other.height);
        self.size += other.size;
        self.size_no_spaces += other.size_no_spaces;
        self.hashs.acc(&other.hashs);
        self.line_count = self.line_count.saturating_add(other.line_count);
    }
}

impl<U: NodeHashs> std::ops::AddAssign for SubTreeMetrics<U> {
    fn add_assign(&mut self, rhs: Self) {
        self.acc(rhs);
    }
}

impl<U> SubTreeMetrics<U> {
    pub fn map_hashs<V>(self, f: impl Fn(U) -> V) -> SubTreeMetrics<V> {
        SubTreeMetrics {
            hashs: f(self.hashs),
            size: self.size,
            height: self.height,
            size_no_spaces: self.size_no_spaces,
            line_count: self.line_count,
        }
    }

    #[must_use]
    pub fn add_md_metrics(
        self,
        dyn_builder: &mut impl GatherAttrErazed,
        children_is_empty: bool,
    ) -> U {
        use crate::store::nodes::compo;
        if !children_is_empty {
            dyn_builder.add(compo::Size(self.size));
            dyn_builder.add(compo::SizeNoSpaces(self.size_no_spaces));
            dyn_builder.add(compo::Height(self.height));
        }

        if self.line_count > 0 {
            dyn_builder.add(compo::LineCount(self.line_count));
        }

        self.hashs
    }
}

impl<U: NodeHashs> From<SubTreeMetrics<crate::hashed::HashesBuilder<U>>> for SubTreeMetrics<U>
where
    crate::hashed::HashesBuilder<U>: crate::hashed::MetaDataHashsBuilder<U>,
{
    fn from(value: SubTreeMetrics<crate::hashed::HashesBuilder<U>>) -> Self {
        use crate::hashed::MetaDataHashsBuilder;
        value.map_hashs(|h| h.build())
    }
}

impl<U: crate::hashed::ComputableNodeHashs> SubTreeMetrics<U>
where
    U::Hash: std::hash::Hash + Copy,
{
    pub fn finalize<K: ?Sized + std::hash::Hash, L: ?Sized + std::hash::Hash>(
        self,
        k: &K,
        l: &L,
    ) -> SubTreeMetrics<crate::hashed::HashesBuilder<U>> {
        let size_no_spaces = self.size_no_spaces + 1;
        use crate::hashed::IndexingHashBuilder;
        let hashs = crate::hashed::HashesBuilder::new(self.hashs, k, l, size_no_spaces);
        SubTreeMetrics {
            hashs,
            size: self.size + 1,
            height: self.height + 1,
            size_no_spaces,
            line_count: self.line_count,
        }
    }
}

pub trait GlobalData {
    fn up(&mut self);
    fn right(&mut self);
    fn down(&mut self);
}

#[derive(Debug, Clone, Copy)]
pub struct BasicGlobalData {
    depth: usize,
    /// preorder position
    position: usize,
}

impl Default for BasicGlobalData {
    fn default() -> Self {
        Self {
            depth: 1,
            position: 0,
        }
    }
}

impl GlobalData for BasicGlobalData {
    fn up(&mut self) {
        self.depth -= 1;
        // TODO fix, there are issues the depth count is too big, I am probably missing a up somewhere
    }

    fn right(&mut self) {
        self.position += 1;
        // self.depth -= 1;
    }

    /// goto the first children
    fn down(&mut self) {
        self.position += 1;
        self.depth += 1;
    }
}
pub trait TotalBytesGlobalData {
    fn set_sum_byte_length(&mut self, sum_byte_length: usize);
    fn sum_byte_length(&self) -> usize;
}

#[derive(Debug, Clone, Copy)]
pub struct TextedGlobalData<'a, GD = BasicGlobalData> {
    text: &'a [u8],
    inner: GD,
}

impl<'a, GD> TextedGlobalData<'a, GD> {
    pub fn new(inner: GD, text: &'a [u8]) -> Self {
        Self { text, inner }
    }
    pub fn text(self) -> &'a [u8] {
        self.text
    }
}

impl<GD: GlobalData> GlobalData for TextedGlobalData<'_, GD> {
    fn up(&mut self) {
        self.inner.up();
    }

    fn right(&mut self) {
        self.inner.right();
    }

    /// goto the first children
    fn down(&mut self) {
        self.inner.down();
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpacedGlobalData<'a, GD = BasicGlobalData> {
    sum_byte_length: usize,
    inner: TextedGlobalData<'a, GD>,
}

impl<GD> std::ops::Deref for SpacedGlobalData<'_, GD> {
    type Target = GD;

    fn deref(&self) -> &Self::Target {
        &self.inner.inner
    }
}
impl<GD> std::ops::DerefMut for SpacedGlobalData<'_, GD> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner.inner
    }
}

impl<'a, GD> From<TextedGlobalData<'a, GD>> for SpacedGlobalData<'a, GD> {
    fn from(inner: TextedGlobalData<'a, GD>) -> Self {
        Self {
            sum_byte_length: 0,
            inner,
        }
    }
}
impl<GD: Clone> SpacedGlobalData<'_, GD> {
    pub fn simple(&self) -> GD {
        self.inner.inner.clone()
    }
}

impl<GD: Clone> TextedGlobalData<'_, GD> {
    pub fn simple(&self) -> GD {
        self.inner.clone()
    }
}

impl<GD> SpacedGlobalData<'_, GD> {
    pub fn sum_byte_length(&self) -> usize {
        self.sum_byte_length
    }
}

impl<GD> TotalBytesGlobalData for SpacedGlobalData<'_, GD> {
    fn set_sum_byte_length(&mut self, sum_byte_length: usize) {
        // assert!(self.sum_byte_length <= sum_byte_length);
        assert!(
            self.sum_byte_length <= sum_byte_length,
            "new byte offset is smaller: {} > {}",
            self.sum_byte_length,
            sum_byte_length
        );
        self.sum_byte_length = sum_byte_length;
    }
    fn sum_byte_length(&self) -> usize {
        self.sum_byte_length
    }
}

impl<GD: GlobalData> GlobalData for SpacedGlobalData<'_, GD> {
    fn up(&mut self) {
        self.inner.up();
    }

    fn right(&mut self) {
        self.inner.right();
    }

    /// goto the first children
    fn down(&mut self) {
        self.inner.down();
    }
}

mod global_stats {
    use super::*;
    #[derive(Debug, Clone)]
    pub struct StatsGlobalData<GD = BasicGlobalData> {
        #[cfg(feature = "subtree-stats")]
        pub height_counts: Vec<u32>,
        inner: GD,
    }

    impl<GD: Default> Default for StatsGlobalData<GD> {
        fn default() -> Self {
            Self::new(Default::default())
        }
    }

    impl<GD: TotalBytesGlobalData> TotalBytesGlobalData for StatsGlobalData<GD> {
        fn set_sum_byte_length(&mut self, sum_byte_length: usize) {
            self.inner.set_sum_byte_length(sum_byte_length)
        }
        fn sum_byte_length(&self) -> usize {
            self.inner.sum_byte_length()
        }
    }

    impl<GD> std::ops::Deref for StatsGlobalData<GD> {
        type Target = GD;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }

    impl<GD: GlobalData> GlobalData for StatsGlobalData<GD> {
        fn up(&mut self) {
            self.inner.up();
        }

        fn right(&mut self) {
            self.inner.right();
        }

        fn down(&mut self) {
            self.inner.down();
        }
    }

    impl<GD> StatsGlobalData<GD> {
        pub fn new(inner: GD) -> Self {
            Self {
                #[cfg(feature = "subtree-stats")]
                height_counts: Vec::with_capacity(30),
                inner,
            }
        }
    }

    impl StatsGlobalData<SpacedGlobalData<'_>> {
        pub fn sum_byte_length(&self) -> usize {
            self.inner.sum_byte_length()
        }
    }
}

pub use global_stats::StatsGlobalData;

/// Primary trait to implement to generate AST.
pub trait TreeGen {
    /// Container holding data waiting to be added to the HyperAST
    /// Note: needs WithByteRange to handle hidden node properly, it allows to go back up without using the cursor. When Treesitter is "fixed" change that
    type Acc: Accumulator + WithByteRange;

    /// Container holding global data used during generation.
    ///
    /// Useful for transient data needed during generation,
    /// this way you avoid cluttering [TreeGen::Acc].
    ///
    /// WARN make sure it does not leaks contextual data in subtrees.
    type Global: GlobalData;
    fn make(
        &mut self,
        global: &mut Self::Global,
        acc: Self::Acc,
        label: Option<String>,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node;
}

#[cfg(feature = "ts")]
#[derive(Debug)]
pub struct Parents<Acc>(Vec<P<Acc>>);

#[cfg(feature = "ts")]
impl<Acc> From<Acc> for Parents<Acc> {
    fn from(value: Acc) -> Self {
        Self::new(P::Visible(value))
    }
}

#[cfg(feature = "ts")]
#[derive(Debug)]
#[allow(unused)]
enum P<Acc> {
    ManualyHidden,
    BothHidden,
    Hidden(Acc),
    Visible(Acc),
}

#[cfg(feature = "ts")]
impl<Acc> P<Acc> {
    fn unwrap(self) -> Acc {
        match self {
            P::ManualyHidden => panic!(),
            P::BothHidden => panic!(),
            P::Hidden(p) => p,
            P::Visible(p) => p,
        }
    }
    fn as_ref(&self) -> P<&Acc> {
        match self {
            P::ManualyHidden => P::ManualyHidden,
            P::BothHidden => P::BothHidden,
            P::Hidden(t) => P::Hidden(t),
            P::Visible(t) => P::Visible(t),
        }
    }
    fn as_mut(&mut self) -> P<&mut Acc> {
        match self {
            P::ManualyHidden => P::ManualyHidden,
            P::BothHidden => P::BothHidden,
            P::Hidden(t) => P::Hidden(t),
            P::Visible(t) => P::Visible(t),
        }
    }
}

#[cfg(feature = "ts")]
impl<Acc> P<Acc> {
    fn ok(self) -> Option<Acc> {
        match self {
            P::ManualyHidden => None,
            P::BothHidden => None,
            P::Hidden(p) => Some(p),
            P::Visible(p) => Some(p),
        }
    }
    fn visibility(self) -> Option<(Visibility, Acc)> {
        match self {
            P::ManualyHidden => None,
            P::BothHidden => None,
            P::Hidden(a) => Some((Visibility::Hidden, a)),
            P::Visible(a) => Some((Visibility::Visible, a)),
        }
    }
}

#[cfg(feature = "ts")]
impl<Acc> Parents<Acc> {
    fn new(value: P<Acc>) -> Self {
        Self(vec![value])
    }
    pub fn finalize(mut self) -> Acc {
        assert_eq!(self.0.len(), 1);
        self.0.pop().unwrap().unwrap()
    }
    fn push(&mut self, value: P<Acc>) {
        self.0.push(value)
    }
    fn pop(&mut self) -> Option<P<Acc>> {
        self.0.pop()
    }
    pub fn parent(&self) -> Option<&Acc> {
        self.0.iter().rev().find_map(|x| x.as_ref().ok())
    }
    // fn parent_mut(&mut self) -> Option<&mut Acc> {
    //     self.0.iter_mut().rev().find_map(|x| x.as_mut().ok())
    // }
    fn parent_mut_with_vis(&mut self) -> Option<(Visibility, &mut Acc)> {
        self.0
            .iter_mut()
            .rev()
            .find_map(|x| x.as_mut().visibility())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug)]
pub struct RoleAcc<R> {
    pub current: Option<R>,
    pub roles: Vec<R>,
    pub offsets: Vec<u8>,
}

impl<R> Default for RoleAcc<R> {
    fn default() -> Self {
        Self {
            current: None,
            roles: Default::default(),
            offsets: Default::default(),
        }
    }
}

impl<R> RoleAcc<R> {
    pub fn acc(&mut self, role: R, o: usize) {
        use num::ToPrimitive;
        if let Some(o) = o.to_u8() {
            self.roles.push(role);
            self.offsets.push(o);
        } else {
            log::warn!("overflowed 255 offseted role...");
            debug_assert!(false);
            // TODO could increase to u16,
            // at least on some variants.
            // TODO could also use the repeat nodes to break down nodes with way to many children...
        }
    }

    pub fn add_md<EB: GatherAttrErazed>(self, dyn_builder: &mut EB)
    where
        R: 'static + std::marker::Send + std::marker::Sync,
    {
        use crate::store::nodes::compo;
        debug_assert!(self.current.is_none());
        if !self.roles.is_empty() {
            dyn_builder.add(compo::Roles(self.roles.into_boxed_slice()));
            dyn_builder.add(compo::RoleOffsets(self.offsets.into_boxed_slice()));
        }
    }
}

pub fn add_md_precomp_queries<EB: GatherAttrErazed>(
    dyn_builder: &mut EB,
    precomp_queries: PrecompQueries,
) {
    use crate::store::nodes::compo;
    if precomp_queries > 0 {
        dyn_builder.add(compo::Precomp(precomp_queries));
    } else {
        dyn_builder.add(compo::PrecompFlag);
    }
}

#[cfg(feature = "ts")]
pub mod zipped;
#[cfg(feature = "ts")]
pub use zipped::PreResult;
#[cfg(feature = "ts")]
pub use zipped::ZippedTreeGen;

#[cfg(feature = "ts_type")]
pub trait TsEnableTS: crate::types::ETypeStore
where
    Self::Ty2: TsType,
{
    const ERROR: u16 = u16::MAX;
    const _ERROR: u16 = u16::MAX - 1;
    const SPACES: u16 = u16::MAX - 2;
    const DIRECTORY: u16 = u16::MAX - 3;
    const META_DIR: u16 = u16::MAX - 4;
    const LOWEST_RESERVED: u16 = Self::META_DIR;
    fn try_obtain_type<N: crate::tree_gen::parser::NodeWithU16TypeId>(n: &N) -> Option<Self::Ty2>;
    fn obtain_type<N: crate::tree_gen::parser::NodeWithU16TypeId>(n: &N) -> Self::Ty2;
}

#[cfg(feature = "ts_type")]
pub trait TsType: crate::types::HyperType + Copy {
    fn spaces() -> Self;
    fn is_repeat(&self) -> bool;
    /// returns true if current node should be treated as a leaf node,
    /// i.e. we skip generating children and set the label with the content of the span
    fn is_leaf(self) -> bool;
}

/// utils for generating code with tree-sitter
#[cfg(feature = "ts")]
pub mod utils_ts;

#[cfg(all(feature = "ts", feature = "legion"))]
pub trait TsExtra<HAST: StoreRefAssoc>:
    Extra<HAST, zipped_ts_extra::Acc<<HAST::TS as types::ETypeStore>::Ty2>>
where
    HAST::TS: types::ETypeStore,
{
}

#[cfg(all(feature = "ts", feature = "legion"))]
impl<T, HAST: StoreRefAssoc> TsExtra<HAST> for T
where
    T: Extra<HAST, zipped_ts_extra::Acc<<HAST::TS as types::ETypeStore>::Ty2>>,
    HAST::TS: types::ETypeStore,
{
}

/// Allow to define extra derived data
///
/// Can be chained with [`extra_chain::ChainedExtra`] when independent
pub trait Extra<HAST: StoreRefAssoc, Acc: Accumulator> {
    type Acc: std::ops::Deref<Target = Acc>
        + std::ops::DerefMut
        + Accumulator<Node = Self::Node>
        + WithByteRange
        + WithExtra
        + From<Acc> // for consumers of Extra
        // # used to implement `Extra` on `ChainedExtra`
        + From<(Acc, <Self::Acc as WithExtra>::Extra)>
        + Into<(Acc, <Self::Acc as WithExtra>::Extra)>;
    type Node: From<Acc::Node>
        // # used to implement `Extra` on `ChainedExtra`
        + From<(<Acc as Accumulator>::Node, <Self::Acc as WithExtra>::Extra)>
        + Into<(<Acc as Accumulator>::Node, <Self::Acc as WithExtra>::Extra)>;

    #[doc(hidden)] // TODO extract to other trait. Used to implement `Extra` on `ChainedExtra`
    fn _from_cache(&mut self, _id: HAST::IdN) -> Option<<Self::Acc as WithExtra>::Extra> {
        None
    }

    fn from_cache(
        &mut self,
        id: HAST::IdN,
        or_else: impl FnOnce() -> <Acc as Accumulator>::Node,
    ) -> Self::Node;

    fn extra(
        &mut self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        entity: &mut impl GatherAttrErazed,
        acc: Self::Acc,
        label: Option<&str>,
    ) -> Self::Acc;

    #[doc(hidden)] // TODO extract to other trait. Used to implement `Extra` on `ChainedExtra`
    fn _to_cache(
        &mut self,
        _id: HAST::IdN,
        extra: <Self::Acc as WithExtra>::Extra,
    ) -> <Self::Acc as WithExtra>::Extra {
        extra
    }

    fn to_cache(
        &mut self,
        id: HAST::IdN,
        node: <Acc as Accumulator>::Node,
        acc: Self::Acc,
    ) -> Self::Node;
}

#[cfg(all(feature = "ts", feature = "legion"))]
pub mod zipped_ts_extra;

#[cfg(all(feature = "ts", feature = "legion"))]
pub mod zipped_ts;

pub(crate) fn things_after_last_lb<'b>(lb: &[u8], spaces: &'b [u8]) -> Option<&'b [u8]> {
    spaces
        .windows(lb.len())
        .rev()
        .position(|window| window == lb)
        .map(|i| &spaces[spaces.len() - i - 1..])
}

pub fn compute_indentation<'a>(
    line_break: &Vec<u8>,
    text: &'a [u8],
    pos: usize,
    padding_start: usize,
    parent_indentation: &'a [Space],
) -> Vec<Space> {
    let spaces = { &text[padding_start..pos] };
    // let spaces = text.get(padding_start.min(text.len()-1)..pos.min(text.len()));
    // let Some(spaces) = spaces else {
    //     return parent_indentation.to_vec()
    // };
    let spaces_after_lb = things_after_last_lb(line_break, spaces);
    match spaces_after_lb {
        Some(s) => Space::format_indentation(s),
        None => parent_indentation.to_vec(),
    }
}

pub fn try_compute_indentation<'a>(
    line_break: &Vec<u8>,
    text: &'a [u8],
    pos: usize,
    padding_start: usize,
    parent_indentation: &'a [Space],
) -> Vec<Space> {
    let spaces = { &text[padding_start..pos] };
    let spaces_after_lb = things_after_last_lb(line_break, spaces);
    match spaces_after_lb {
        Some(s) => Space::try_format_indentation(s).unwrap_or(parent_indentation.to_vec()),
        None => parent_indentation.to_vec(),
    }
}

// TODO return a slice
pub fn get_spacing(padding_start: usize, pos: usize, text: &[u8]) -> Option<Vec<u8>> {
    // TODO change debug assert to assert if you want to strictly enforce spaces, issues with other char leaking is often caused by "bad" grammar.
    if padding_start != pos {
        let spaces = &text[padding_start..pos];
        // let spaces = Space::format_indentation(spaces);
        let mut bslash = false;
        spaces.iter().for_each(|x| {
            if bslash && (*x == b'\n' || *x == b'\r') {
                bslash = false
            } else if *x == b'\\' {
                debug_assert!(!bslash);
                bslash = true
            } else {
                debug_assert!(
                    *x == b' ' || *x == b'\n' || *x == b'\t' || *x == b'\r',
                    "{} {} {:?}",
                    x,
                    padding_start,
                    std::str::from_utf8(spaces).unwrap()
                )
            }
        });
        debug_assert!(
            !bslash,
            "{}",
            std::str::from_utf8(&text[padding_start.saturating_sub(100)..pos + 50]).unwrap()
        );
        let spaces = spaces.to_vec();
        // let spaces = Space::replace_indentation(parent_indentation, &spaces);
        // TODO put back the relativisation later, can pose issues when computing len of a subtree (contextually if we make the optimisation)
        Some(spaces)
    } else {
        None
    }
}

pub fn try_get_spacing(padding_start: usize, pos: usize, text: &[u8]) -> Option<Vec<u8>> {
    // ) -> Option<Spaces> {
    if padding_start != pos {
        let spaces = &text[padding_start..pos];
        // println!("{:?}",std::str::from_utf8(spaces).unwrap());
        if spaces
            .iter()
            .any(|x| *x != b' ' && *x != b'\n' && *x != b'\t' && *x != b'\r')
        {
            return None;
        }
        let spaces = spaces.to_vec();

        // let spaces = Space::try_format_indentation(spaces)?;
        // let spaces = Space::replace_indentation(parent_indentation, &spaces);
        // TODO put back the relativisation later, can pose issues when computing len of a subtree (contextually if we make the optimisation)
        Some(spaces)
    } else {
        None
    }
}

pub fn has_final_space(depth: &usize, sum_byte_length: usize, text: &[u8]) -> bool {
    // TODO not sure about depth
    *depth == 0 && sum_byte_length < text.len()
}

pub fn newline_count(label: &Option<impl AsRef<str>>) -> u32 {
    let Some(label) = label.as_ref() else {
        return 0;
    };
    let r = label.as_ref().matches("\n").count();
    num::ToPrimitive::to_u32(&r).expect("too many newlines")
}

#[cfg(feature = "ts")]
pub fn handle_file_bounds<G>(
    g: &mut G,
    text: &[u8],
    xx: <G as ZippedTreeGen>::TreeCursor<'_>,
    global: &mut <G as TreeGen>::Global,
    init: <G as TreeGen>::Acc,
    mut make_space: impl FnMut(
        &mut G,
        &<G as TreeGen>::Global,
        &[u8],
    ) -> <<G as TreeGen>::Acc as Accumulator>::Node,
) -> <G as TreeGen>::Acc
where
    G: ZippedTreeGen<Text = [u8]>,
    G::Global: TotalBytesGlobalData,
    <G as TreeGen>::Acc: Accumulator + WithByteRange,
{
    _handle_file_bounds(g, text, xx, global, init, |g, global, text, acc| {
        let node = make_space(g, global, text);
        acc.push(node);
    })
}

#[cfg(feature = "ts")]
#[doc(hidden)]
pub fn _handle_file_bounds<G>(
    g: &mut G,
    text: &[u8],
    mut xx: <G as ZippedTreeGen>::TreeCursor<'_>,
    global: &mut <G as TreeGen>::Global,
    mut init: <G as TreeGen>::Acc,
    mut make_space: impl FnMut(&mut G, &<G as TreeGen>::Global, &[u8], &mut <G as TreeGen>::Acc),
) -> <G as TreeGen>::Acc
where
    G: ZippedTreeGen<Text = [u8]>,
    G::Global: TotalBytesGlobalData,
    <G as TreeGen>::Acc: Accumulator + WithByteRange,
{
    let spacing = get_spacing(global.sum_byte_length(), init.begin_byte(), text);
    if let Some(spacing) = spacing {
        global.down();
        global.set_sum_byte_length(init.begin_byte());
        make_space(g, global, &spacing, &mut init);
        global.right();
    }
    let mut stack = init.into();
    g.r#gen(text, &mut stack, &mut xx, global);
    let mut acc = stack.finalize();
    if has_final_space(&0, global.sum_byte_length(), text) {
        let spacing = get_spacing(global.sum_byte_length(), text.len(), text);
        if let Some(spacing) = spacing {
            global.right();
            make_space(g, global, &spacing, &mut acc);
        }
    }
    acc
}

pub fn hash32<T: ?Sized + std::hash::Hash>(t: &T) -> u32 {
    crate::utils::clamp_u64_to_u32(&crate::utils::hash(t))
}

pub trait Prepro<HAST: HyperAST>
where
    HAST::TS: ETypeStore,
{
    const USING: bool;
    #[cfg(feature = "scripting")]
    type Scope: crate::scripting::Accumulable + crate::scripting::Finishable;
    #[cfg(not(feature = "scripting"))]
    type Scope: crate::scripting::Scriptable;
    fn preprocessing(
        &self,
        ty: <HAST::TS as ETypeStore>::Ty2,
    ) -> Result<Self::Scope, <Self::Scope as crate::scripting::Scriptable>::Error>;

    fn scripts(&self) -> &<Self::Scope as crate::scripting::Scriptable>::Scripts;
}

impl<HAST: HyperAST, Acc> Prepro<HAST> for NoOpMore<HAST::TS, Acc>
where
    HAST::TS: ETypeStore,
{
    const USING: bool = false;
    type Scope = crate::scripting::Acc;
    fn preprocessing(
        &self,
        _t: <HAST::TS as ETypeStore>::Ty2,
    ) -> Result<Self::Scope, <Self::Scope as crate::scripting::Scriptable>::Error> {
        todo!()
    }

    fn scripts(&self) -> &<Self::Scope as crate::scripting::Scriptable>::Scripts {
        todo!()
    }
}

pub type PrecompQueries = u16;

pub trait More<HAST: StoreRefAssoc> {
    type Acc: WithChildren<<HAST as HyperASTShared>::IdN>;
    const ENABLED: bool;
    fn match_precomp_queries(
        &self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        acc: &Self::Acc,
        label: Option<&str>,
    ) -> crate::tree_gen::PrecompQueries;
}

#[repr(transparent)]
pub struct NoOpMore<T, Acc>(std::marker::PhantomData<(T, Acc)>);

impl<T, Acc> Default for NoOpMore<T, Acc> {
    fn default() -> Self {
        Self(Default::default())
    }
}

pub trait WithExtra {
    type Extra;
    fn extra(&mut self) -> &mut Self::Extra;
}

#[derive(Debug)]
pub struct AccWithExtra<Acc, Extra>(Acc, Extra);

pub mod extra;

pub mod extra_pattern_precomp;

pub mod extra_chain;

impl<HAST, Acc> More<HAST> for NoOpMore<HAST::TS, Acc>
where
    HAST: StoreRefAssoc,
    Acc: WithChildren<HAST::IdN>,
{
    type Acc = Acc;
    const ENABLED: bool = false;
    fn match_precomp_queries(
        &self,
        _stores: <HAST as StoreRefAssoc>::S<'_>,
        _acc: &Acc,
        _label: Option<&str>,
    ) -> PrecompQueries {
        Default::default()
    }
}

impl<HAST, Acc> PreproTSG<HAST> for NoOpMore<HAST::TS, Acc>
where
    HAST: StoreRefAssoc,
    Acc: WithChildren<HAST::IdN>,
    Acc: WithByteRange,
{
    const GRAPHING: bool = false;
    fn compute_tsg(
        &self,
        _stores: <HAST as StoreRefAssoc>::S<'_>,
        _acc: &Self::Acc,
        _label: Option<&str>,
    ) -> Result<usize, String> {
        Ok(0)
    }
}

pub trait PreproTSG<HAST: StoreRefAssoc>: More<HAST> {
    const GRAPHING: bool;
    fn compute_tsg(
        &self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        acc: &Self::Acc,
        label: Option<&str>,
    ) -> Result<usize, String>;
}

use crate::types;

pub mod metric_definition;
