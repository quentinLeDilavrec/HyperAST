use super::Accumulator;
use super::P;
use super::Parents;
use super::TotalBytesGlobalData;
use super::TreeGen;
use super::parser;
use super::parser::Visibility;

use super::GlobalData as _;
use super::WithByteRange as _;
use super::parser::TreeCursor as _;

/// Define a zipped visitor, where you mostly have to implement,
/// [`ZippedTreeGen::pre`] going down,
/// and [`ZippedTreeGen::post`] going up in the traversal.
pub trait ZippedTreeGen: TreeGen
where
    Self::Global: TotalBytesGlobalData,
{
    type Stores;
    // # source
    type Text: ?Sized;
    type Node<'a>: parser::Node;
    type TreeCursor<'a>: parser::TreeCursor<N = Self::Node<'a>> + std::fmt::Debug + Clone;

    fn init_val(&mut self, text: &Self::Text, node: &Self::Node<'_>) -> Self::Acc;

    /// Can be implemented if you want to skip certain nodes,
    /// note that skipping only act on the "overlay" tree structure,
    /// meaning that the content of a skipped node is fed to its parents
    ///
    /// The default implementation skips nothing.
    ///
    ///  see also also the following example use:
    /// [`hyperast_gen_ts_cpp::legion::CppTreeGen::pre_skippable`](../../hyperast_gen_ts_cpp/legion/struct.CppTreeGen.html#method.pre_skippable)
    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> PreResult<<Self as TreeGen>::Acc> {
        PreResult::Ok(self.pre(text, &cursor.node(), stack, global))
    }

    /// Called when going up
    fn pre(
        &mut self,
        text: &Self::Text,
        node: &Self::Node<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> <Self as TreeGen>::Acc;

    fn acc(
        &mut self,
        parent: &mut <Self as TreeGen>::Acc,
        full_node: <<Self as TreeGen>::Acc as Accumulator>::Node,
    ) {
        parent.push(full_node);
    }

    /// Called when going up
    fn post(
        &mut self,
        parent: &mut <Self as TreeGen>::Acc,
        global: &mut Self::Global,
        text: &Self::Text,
        acc: <Self as TreeGen>::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node;

    fn stores(&mut self) -> &mut Self::Stores;

    fn r#gen(
        &mut self,
        text: &Self::Text,
        stack: &mut Parents<Self::Acc>,
        cursor: &mut Self::TreeCursor<'_>,
        global: &mut Self::Global,
    ) {
        let mut pre_post = super::utils_ts::PrePost2::new(cursor.clone());
        let mut state = ZippedTreeGenAux {
            tree_gen: self,
            text,
            stack,
            global,
        };
        if IT_VARIANT == 0 {
            // while version
            gen_while(&mut state, pre_post);
        } else if IT_VARIANT == 1 {
            // it version
            while let Some(_) = gen_next(&mut state, &mut pre_post) {}
        } else if IT_VARIANT == 2 {
            let it = ZippedTreeGenIt::new(&mut state, pre_post);
            // using the iterator
            for _ in it {}
        }
    }
}

// keep variant while trying them
const IT_VARIANT: usize = 2;

#[derive(PartialEq, Eq)]
pub enum Has {
    Down,
    Up,
    Right,
}

impl Has {
    pub fn letter(&self) -> &'static str {
        match self {
            Has::Down => "d",
            Has::Up => "u",
            Has::Right => "r",
        }
    }
}

pub enum PreResult<Acc> {
    /// Do not process node and its children
    Skip,
    /// Do not process node (but process children)
    Ignore,
    /// Do not process children
    SkipChildren(Acc),
    /// Process the node and its children
    Ok(Acc),
}

pub fn gen_while<Slf: ZippedTreeGen + ?Sized>(
    state: &mut ZippedTreeGenAux<'_, Slf>,
    mut pre_post: super::utils_ts::PrePost2<Slf::TreeCursor<'_>>,
) where
    Slf::Global: TotalBytesGlobalData,
{
    while let Some(visibility) = pre_post.next() {
        let (cursor, has) = pre_post.current();
        if *has == Has::Up || *has == Has::Right {
            if state.stack.len() == 0 {
                return;
            }
            gen_post(state);
        }
        if *has == Has::Down || *has == Has::Right {
            let cursor = cursor.unwrap();
            gen_pre(state, cursor, visibility, has);
        }
        assert_eq!(state.stack.len(), pre_post.stack.len());
    }
}

fn gen_next<Slf: ZippedTreeGen + ?Sized>(
    state: &mut ZippedTreeGenAux<'_, Slf>,
    pre_post: &mut super::utils_ts::PrePost2<Slf::TreeCursor<'_>>,
) -> Option<()>
where
    Slf::Global: TotalBytesGlobalData,
{
    let Some(visibility) = pre_post.next() else {
        return None;
    };
    let (cursor, has) = pre_post.current();

    // NOTE aux function is for testing purposes
    if let Some(value) = gen_next_aux(state, visibility, has, cursor) {
        return value;
    }
    assert_eq!(state.stack.len(), pre_post.stack.len());
    Some(())
}

#[doc(hidden)]
pub fn gen_next_aux<Slf: ZippedTreeGen + ?Sized>(
    state: &mut ZippedTreeGenAux<'_, Slf>,
    //
    visibility: Visibility,
    has: &mut Has,
    cursor: Option<&Slf::TreeCursor<'_>>,
) -> Option<Option<()>>
where
    Slf::Global: TotalBytesGlobalData,
{
    if *has == Has::Up || *has == Has::Right {
        if state.stack.len() == 0 {
            return Some(None);
        }
        gen_post(state);
    }
    if *has == Has::Down || *has == Has::Right {
        let cursor = cursor.unwrap();
        gen_pre(state, cursor, visibility, has);
    }
    None
}

fn gen_pre<Slf: ZippedTreeGen + ?Sized>(
    state: &mut ZippedTreeGenAux<'_, Slf>,
    // tree_gen: &mut Slf,
    // text: &Slf::Text,
    // stack: &mut Parents<Slf::Acc>,
    // global: &mut Slf::Global,
    cursor: &Slf::TreeCursor<'_>,
    visibility: Visibility,
    has: &mut Has,
) where
    Slf::Global: TotalBytesGlobalData,
{
    let ZippedTreeGenAux {
        tree_gen,
        text,
        stack,
        global,
    } = state;
    global.down();
    let n = Slf::pre_skippable(tree_gen, text, cursor, stack, global);
    match n {
        PreResult::Skip => {
            stack.push(P::BothHidden);
            *has = Has::Up;
            global.up();
        }
        PreResult::Ignore => {
            if let Visibility::Visible = visibility {
                stack.push(P::ManualyHidden);
            } else {
                stack.push(P::BothHidden);
            }
        }
        PreResult::SkipChildren(acc) => {
            *has = Has::Up;
            if let Visibility::Visible = visibility {
                stack.push(P::Visible(acc));
            } else {
                unimplemented!("Only concrete nodes should be leafs")
            }
        }
        PreResult::Ok(acc) => {
            global.set_sum_byte_length(acc.begin_byte());
            if let Visibility::Visible = visibility {
                stack.push(P::Visible(acc));
            } else {
                stack.push(P::Hidden(acc));
            }
        }
    }
}

fn gen_post<Slf: ZippedTreeGen + ?Sized>(state: &mut ZippedTreeGenAux<'_, Slf>)
where
    Slf::Global: TotalBytesGlobalData,
{
    let ZippedTreeGenAux {
        tree_gen,
        text,
        stack,
        global,
    } = state;
    let is_parent_hidden;
    let full_node: Option<_> = match (stack.pop().unwrap(), stack.parent_mut_with_vis()) {
        (P::Visible(acc), None) => {
            global.up();
            is_parent_hidden = false;
            global.set_sum_byte_length(acc.end_byte());
            stack.push(P::Visible(acc));
            None
        }
        (_, None) => {
            panic!();
        }
        (P::ManualyHidden, Some((v, _))) => {
            is_parent_hidden = v == Visibility::Hidden;
            None
        }
        (P::BothHidden, Some((v, _))) => {
            is_parent_hidden = v == Visibility::Hidden;
            None
        }
        (P::Visible(acc), Some((v, parent))) => {
            is_parent_hidden = v == Visibility::Hidden;
            if !acc.has_children() {
                global.set_sum_byte_length(acc.end_byte());
            }

            if is_parent_hidden && parent.end_byte() < acc.begin_byte() {
                dbg!(parent.end_byte(), acc.begin_byte());
                panic!()
            }
            global.up();
            let full_node = Slf::post(tree_gen, parent, global, text, acc);
            Some(full_node)
        }
        (P::Hidden(acc), Some((v, parent))) => {
            is_parent_hidden = v == Visibility::Hidden;
            if !acc.has_children() {
                global.set_sum_byte_length(acc.end_byte());
            }
            if is_parent_hidden && parent.end_byte() < acc.begin_byte() {
                panic!("{} {}", parent.end_byte(), acc.begin_byte());
            } else if is_parent_hidden && parent.end_byte() == acc.begin_byte() {
                log::error!("{} {}", parent.end_byte(), acc.begin_byte());
                assert!(!acc.has_children());
                global.up();
                None
            } else {
                global.up();
                let full_node = Slf::post(tree_gen, parent, global, text, acc);
                Some(full_node)
            }
        }
    };

    log::trace!("{}", is_parent_hidden);

    let parent = state.stack.parent_mut().unwrap();
    if let Some(full_node) = full_node {
        Slf::acc(state.tree_gen, parent, full_node);
    }
}

#[doc(hidden)]
pub struct ZippedTreeGenAux<'a, Slf: ZippedTreeGen + ?Sized>
where
    Slf::Global: TotalBytesGlobalData,
{
    pub tree_gen: &'a mut Slf,
    pub text: &'a Slf::Text,
    pub stack: &'a mut Parents<Slf::Acc>,
    pub global: &'a mut Slf::Global,
}

#[doc(hidden)]
pub struct ZippedTreeGenIt<'a, 'b, Slf: ZippedTreeGen + ?Sized>
where
    Slf::Global: TotalBytesGlobalData,
{
    pub aux: &'a mut ZippedTreeGenAux<'a, Slf>,
    pub pre_post: super::utils_ts::PrePost2<Slf::TreeCursor<'b>>,
}

impl<'a, 'b, Slf: ZippedTreeGen + ?Sized> ZippedTreeGenIt<'a, 'b, Slf>
where
    Slf::Global: TotalBytesGlobalData,
{
    pub fn new(
        aux: &'a mut ZippedTreeGenAux<'a, Slf>,
        pre_post: super::utils_ts::PrePost2<Slf::TreeCursor<'b>>,
    ) -> Self {
        Self { aux, pre_post }
    }
}

impl<'a, 'b, Slf: ZippedTreeGen + ?Sized> Iterator for ZippedTreeGenIt<'a, 'b, Slf>
where
    Slf::Global: TotalBytesGlobalData,
{
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        gen_next(self.aux, &mut self.pre_post)
    }
}
