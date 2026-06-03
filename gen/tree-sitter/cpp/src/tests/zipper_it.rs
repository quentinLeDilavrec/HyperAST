//! Test the traversal of the tree-sitter CST with the Zipped iterator.
use hyperast::tree_gen::TotalBytesGlobalData as _;

use crate::legion::tree_sitter_parse;

use super::{CppTreeGen, SimpleStores};

use super::EX_ISSUE_MISSING_NODE;

#[test_log::test]
pub(crate) fn cpp_parsing_error_it_test() {
    let text = EX_ISSUE_MISSING_NODE.as_bytes();
    let tree = match tree_sitter_parse(text) {
        Ok(t) => t,
        Err(t) => t,
    };
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);

    let cursor = tree.walk();

    let mut global = crate::legion::Global::from(hyperast::tree_gen::TextedGlobalData::new(
        Default::default(),
        text,
    ));
    use hyperast::tree_gen::ZippedTreeGen;
    let mut init = tree_gen.init_val(text, &hyperast::tree_gen::utils_ts::TNode(cursor.node()));
    let cursor = hyperast::tree_gen::utils_ts::TTreeCursor(cursor);

    let spacing = hyperast::tree_gen::get_spacing(
        init.padding_start,
        init.start_byte,
        text,
        hyperast::tree_gen::AccIndentation::indentation(&init),
    );
    use hyperast::tree_gen::GlobalData;
    if let Some(spacing) = spacing {
        global.down();
        global.set_sum_byte_length(init.start_byte);
        hyperast::tree_gen::Accumulator::push(
            &mut init,
            hyperast::full::FullNode {
                global: global.simple(),
                local: tree_gen.make_spacing(spacing),
            },
        );
        global.right();
    }
    let mut stack = init.into();

    let pre_post = hyperast::tree_gen::utils_ts::PrePost2::new(cursor.clone());
    let mut aux = hyperast::tree_gen::zipped::ZippedTreeGenAux {
        tree_gen: &mut tree_gen,
        text,
        stack: &mut stack,
        global: &mut global,
    };
    let mut it = hyperast::tree_gen::zipped::ZippedTreeGenIt::new(&mut aux, pre_post);
    // for _ in 0..10 {
    //     let Some(_) = it.next() else { return };
    //     dbg!();
    // }
    // dbg!();
    while let Some(_) = it.next() {
        let n = &it.pre_post.cursor.0.node();
        let has = &it.pre_post.has;
        let has = has.letter();
        eprintln!(
            "{} {} {}-{} {} {has}",
            it.aux.stack.len(),
            n.kind(),
            n.start_byte(),
            n.end_byte(),
            n.is_missing(),
        );
    }
}
