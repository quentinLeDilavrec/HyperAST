//! Testing position conversion

use crate::types::{HyperAST as _, WithChildren as _};
use crate::types::{HyperType as _, Labeled as _, WithSerialization as _};

use crate::store::SimpleStores;
use crate::test_utils::simple_tree::{LS, NS, SimpleTree, TStore, Tree, TreeRef, Ty};
use crate::test_utils::simple_tree::{tree, tree_bytes_len_to_stores};

use super::PositionConverter;
use super::computing_path::{child_at_offsets, child_at_path};
use super::conversion_observer::Observed;
use super::offsets::Offsets;
use super::rooted_wrapper::RootedWrapper;
use super::tags;

type IdN = u16;

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
enum Types {
    Import,
    Class,
    ClassDeclaration,
    Identifier,
    Block,
    File = 30,

    Spaces = 32,
    Dir,
}

impl From<Ty> for Types {
    fn from(value: Ty) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

fn empty_file() -> SimpleTree<u8, usize> {
    use Types::*;
    tree!(
        Dir, ""; [
            tree!(Dir, "animals"; [
                tree!(File, "Dog.x"),
            ]),
    ])
}

fn dog_file() -> SimpleTree<u8, usize> {
    use Types::*;
    let mut import = tree!(Import);
    import.derived_data = 5;
    let mut spaces = tree!(Spaces, " ");
    spaces.derived_data = 1;
    let mut class = tree!(Class);
    class.derived_data = 5;
    let mut name = tree!(Identifier, "Dog");
    name.derived_data = 3;
    let mut block = tree!(Block);
    block.derived_data = 2;
    let mut class_decl = tree!(ClassDeclaration; [
        class,
        spaces.clone(),
        name,
        spaces.clone(),
        block,
    ]);
    class_decl.derived_data = class_decl.children.iter().map(|x| x.derived_data).sum();
    dbg!(class_decl.derived_data);
    let mut file = tree!(File, "Dog.x"; [
        import.clone(),
        spaces.clone(),
        import.clone(),
        spaces.clone(),
        import,
        spaces,
        class_decl,
    ]);
    file.derived_data = file.children.iter().map(|x| x.derived_data).sum();
    dbg!(file.derived_data);
    tree!(
        Dir, ""; [
            tree!(Dir, "animals"; [
                file
            ]),
    ])
}

#[test]
fn test_empty_file0() {
    use Types::*;
    let tree = empty_file();
    let (stores, root) = tree_bytes_len_to_stores(tree);
    let ty = stores.resolve_type(&root);
    let r = stores.resolve(&root);
    let l = label(&stores.label_store, &r);
    dbg!(ty);
    assert_eq!(Types::from(ty), Dir);
    assert_eq!(l, "");
    for x in r.children().unwrap() {
        dbg!(x);
        let r = stores.resolve(&x);
        let l = label(&stores.label_store, &r);
        dbg!(l);
    }
    let x = child_at_path(&stores, root, ["animals", "Dog.x"].into_iter()).unwrap();
    let r = stores.resolve(&x);
    let l = label(&stores.label_store, &r);
    dbg!(l);
    let x1 = child_at_offsets(&stores, root, [0, 0].into_iter()).unwrap();
    let r = stores.resolve(&x1);
    let l = label(&stores.label_store, &r);
    dbg!(l);
    assert_eq!(x, x1)
}

#[test]
fn test_empty_file() {
    let tree = empty_file();
    let (stores, root) = tree_bytes_len_to_stores(tree);
    let path = [0, 0];
    let x = child_at_offsets(&stores, root, path.clone().into_iter()).unwrap();
    let r = stores.resolve(&x);
    let l = label(&stores.label_store, &r);
    dbg!(l);
    let t = stores.resolve_type(&x);
    assert!(t.is_file());
    assert_eq!(Types::from(t), Types::File);

    top_down(&stores, root, &path);
    bottom_up(&stores, root, &path);
}

#[test]
fn test_dog_file() {
    let tree = dog_file();
    let (stores, root) = tree_bytes_len_to_stores(tree);

    let x = child_at_offsets(&stores, root, [0, 0, 1].into_iter()).unwrap();
    assert!(stores.resolve_type(&x).is_spaces());

    let path = [0, 0, 4];

    let x = child_at_offsets(&stores, root, path.clone().into_iter()).unwrap();
    let r = stores.resolve(&x);
    dbg!(stores.resolve_type(&x));
    dbg!(r.try_bytes_len());

    top_down(&stores, root, &path);
    bottom_up(&stores, root, &path);

    let path = [0, 0, 0];
    top_down(&stores, root, &path);
    bottom_up(&stores, root, &path);

    let path = [0, 0, 1];
    top_down(&stores, root, &path);
    bottom_up(&stores, root, &path);

    let path = [0, 0, 2];
    top_down(&stores, root, &path);
    bottom_up(&stores, root, &path);

    let path = [0, 0, 3];
    top_down(&stores, root, &path);
    bottom_up(&stores, root, &path);
}

// test top_down conversions given a path composed of offsets
fn top_down(stores: &SimpleStores<TStore, NS<Tree>, LS<u16>>, root: IdN, path: &[u8]) {
    let offsets = Offsets::from_iterator(path.into_iter().copied());
    dbg!(&offsets);

    let rooted_offsets = offsets.clone().with_root(root);
    dbg!(&rooted_offsets);

    // rooted offsets --> offsets
    let _offsets = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, Offsets<_>>();
    dbg!(&_offsets);

    assert_eq!(&offsets, &_offsets);

    // same but using the wrapper observing the values given through the receivers
    let _offsets: Observed<Offsets<_>> = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, Observed<Offsets<_>>>();
    dbg!(&_offsets);

    assert_eq!(&offsets, &_offsets.into_inner());

    // rooted offsets --> rooted offsets
    let _rooted_offsets = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, RootedWrapper<IdN, Offsets<_>>>();
    dbg!(&_rooted_offsets);

    assert_eq!(&rooted_offsets, &_rooted_offsets);

    // rooted offsets --> offsets and nodes
    let _rooted_offsets = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, super::offsets_and_nodes::StructuralPosition<_, _>>(
    );
    dbg!(&_rooted_offsets);

    // rooted offsets --> offsets and nodes
    let _rooted_offsets = PositionConverter::new(&_rooted_offsets)
        .with_stores(stores)
        .compute_pos_pre_order::<_, Observed<RootedWrapper<_, Offsets<_>>>>();
    dbg!(&_rooted_offsets);

    // identity: rooted offsets <--> offsets and nodes
    assert_eq!(&rooted_offsets, &_rooted_offsets.into_inner());

    // rooted offsets --> file and offset (single offset in bytes from the start of the file)
    let file_and_offset = rooted_offsets
        .with_store(&stores)
        .compute_pos_pre_order::<_, super::Position>();

    dbg!(&file_and_offset);

    // TODO remove because compute_pos_file_and_offset is more generic (leveraging receivers)
    let (node, _offsets) = crate::position::computing_path::resolve_path(
        &stores,
        root,
        file_and_offset.file().to_string_lossy().as_ref(),
        file_and_offset.range().start,
        None,
    );
    dbg!(node, &_offsets);
    let _offsets =
        Offsets::<u8, tags::TopDownFull>::from_iterator(_offsets.into_iter().map(|x| x as u8));
    dbg!(&_offsets);
    assert_eq!(offsets, _offsets);

    // back to offsets
    let rooted_file_and_offset = RootedWrapper::new(root, file_and_offset);
    let _rooted_offsets = PositionConverter::new(&rooted_file_and_offset)
        .with_stores(stores)
        .compute_pos_file_and_offset::<_, Observed<RootedWrapper<_, Offsets<_>>>>();
    dbg!(&_rooted_offsets);

    // identity: rooted offsets <--> file and offset
    assert_eq!(&rooted_offsets, &_rooted_offsets.into_inner());

    // ON HOLD because in pre-order it is not trivially:
    // - implemented (in a single path)
    //   - each time we go in a subtree we would need to go in reverse, counting the columns (but they might be discarded)
    // - and optimized (without discarding intermediate results which sometimes required we go deep in subtrees)
    //   - maybe it would be worth precomputing the cols after last newline in the subtrees
    // instead use the bottom-up approach possibly after converting to offsets and nodes with the top-down
    // // rooted offsets --> file and offset + row/col (usually called point)
    // // let row_col = PositionConverter::new(&rooted_offsets)
    // //     .with_stores(stores)
    // //     .compute_pos_pre_order::<_, super::row_col::RowCol<usize>>();
    // // dbg!(&row_col);
}

fn bottom_up(stores: &SimpleStores<TStore, NS<Tree>, LS<u16>>, root: IdN, path: &[u8]) {
    let offsets =
        Offsets::<_, tags::BottomUpFull>::from_iterator_bottom_up(path.into_iter().copied().rev());
    dbg!(&offsets);

    let rooted_offsets = offsets.clone().with_root(root);
    dbg!(&rooted_offsets);

    // NOTE not possible, we need a fully resolved path to do it in post order
    // let _offsets = rooted_offsets
    //     .with_store(&stores)
    //     .compute_pos_post_order::<_, super::offsets::Offsets<_, tags::BottomUpFull>>();
    // assert_eq!(&offsets, &_offsets);

    // we can still use the pre order conversion like with the top down
    let _offsets = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, Offsets<_, tags::BottomUpFull>>();
    dbg!(&_offsets);

    assert_eq!(&offsets, &_offsets);

    let _offsets: Offsets<_> = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, Offsets<_>>();
    dbg!(&_offsets);

    assert_eq!(&offsets, &_offsets.reverse());

    // now computing the full offsets and nodes in bottom up order
    let _full_offsets_and_nodes = rooted_offsets
        .with_store(stores)
        .compute_pos_pre_order::<_, Observed<super::offsets_and_nodes::StructuralPosition<_, _, tags::BottomUpFull>>>().into_inner();
    dbg!(&_full_offsets_and_nodes);

    // offsets and nodes --> file and offset
    let file_and_offset = PositionConverter::new(&_full_offsets_and_nodes)
        .with_stores(stores)
        .compute_pos_post_order::<_, Observed<super::Position>>()
        .into_inner();
    dbg!(&file_and_offset);

    // offsets and nodes --> row/col
    let row_col = PositionConverter::new(&_full_offsets_and_nodes)
        .with_stores(stores)
        .compute_pos_post_order::<_, super::row_col::RowCol<usize>>();
    dbg!(&row_col);

    // we didn't put any newline in the examples, so those two invariants should hold.
    assert_eq!(0, row_col.row());
    assert_eq!(file_and_offset.range().start, row_col.col());

    // offsets and nodes --> file and range (start and line) (approximate)
    let file_and_range = PositionConverter::new(&_full_offsets_and_nodes)
        .with_stores(stores)
        .compute_pos_post_order::<_, super::file_and_range::Position<_, _>>();

    dbg!(&file_and_range);
}

fn label<'a>(ls: &'a LS<u16>, x: &TreeRef<'_, Tree>) -> &'a str {
    use crate::types::LabelStore;
    ls.resolve(x.get_label_unchecked())
}
