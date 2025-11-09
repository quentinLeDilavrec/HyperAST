//! Mock test the traversal of the tree-sitter CST.
//!
//! No HyperAST is built here.
//!
//! The mock implementations of the different traits are at the end of the file
use hyperast::tree_gen::TotalBytesGlobalData;
use hyperast::tree_gen::parser::Node as _;

use crate::legion::tree_sitter_parse;

use super::EX_ISSUE_MISSING_NODE;

struct CppTreeGenMock(());
pub struct GlobalDataMock {
    sum_byte_length: usize,
}
#[allow(unused)]
pub struct AccMock {
    kind: &'static str,
    padding_start: usize,
    begin_byte: usize,
    end_byte: usize,
    children: Vec<&'static str>,
}

#[test_log::test]
// With these mocks we do not build an HyperAST, we just access the data that would be used
pub(crate) fn cpp_parsing_error_zipper_mock_test() {
    let text = EX_ISSUE_MISSING_NODE.as_bytes();
    let tree = match tree_sitter_parse(text) {
        Ok(t) => t,
        Err(t) => t,
    };
    println!("{:#?}", tree.root_node().to_sexp());
    let mut cursor = hyperast::tree_gen::utils_ts::TTreeCursor::<true>(tree.walk());
    let mut tree_gen = CppTreeGenMock(());
    let mut global = GlobalDataMock { sum_byte_length: 0 };
    let acc = tree_gen.init_val(&[], &hyperast::tree_gen::utils_ts::TNode(cursor.0.node()));
    let mut stack = hyperast::tree_gen::Parents::from(acc);
    use hyperast::tree_gen::ZippedTreeGen;
    tree_gen.r#gen(text, &mut stack, &mut cursor, &mut global);
}

#[test_log::test]
// With these mocks we do not build an HyperAST, we just access the data that would be used
pub(crate) fn cpp_parsing_error_zipper_it_mock_test() {
    let text = EX_ISSUE_MISSING_NODE.as_bytes();
    let tree = match tree_sitter_parse(text) {
        Ok(t) => t,
        Err(t) => t,
    };
    println!("{:#?}", tree.root_node().to_sexp());
    let cursor = hyperast::tree_gen::utils_ts::TTreeCursor::<true>(tree.walk());
    let mut tree_gen = CppTreeGenMock(());
    let mut global = GlobalDataMock { sum_byte_length: 0 };
    let acc = tree_gen.init_val(&[], &hyperast::tree_gen::utils_ts::TNode(cursor.0.node()));
    let mut stack = hyperast::tree_gen::Parents::from(acc);
    use hyperast::tree_gen::ZippedTreeGen;
    let pre_post = hyperast::tree_gen::utils_ts::PrePost2::new(cursor.clone());
    let mut aux = hyperast::tree_gen::zipped::ZippedTreeGenAux {
        tree_gen: &mut tree_gen,
        text,
        stack: &mut stack,
        global: &mut global,
    };
    let mut it = hyperast::tree_gen::zipped::ZippedTreeGenIt::new(&mut aux, pre_post);
    for _ in 0..94 {
        it.next();
    }
    while let Some(vis) = it.pre_post.next() {
        let (cursor, has) = it.pre_post.current();
        let Some(cursor) = cursor else {
            continue;
        };
        let node = cursor.0.node();
        eprintln!(
            "{:?}\t{}\t{}\t{} {}-{}\t{}",
            vis,
            has.letter(),
            node.is_missing(),
            it.aux.global.sum_byte_length,
            node.start_byte(),
            node.end_byte(),
            node.kind(),
        );

        if let Some(_) = hyperast::tree_gen::zipped::gen_next_aux(it.aux, vis, has, Some(cursor)) {
            continue;
        }
        assert_eq!(it.aux.stack.len(), it.pre_post.stack.len());
    }
}

impl hyperast::tree_gen::GlobalData for GlobalDataMock {
    fn up(&mut self) {}
    fn right(&mut self) {}
    fn down(&mut self) {}
}
impl hyperast::tree_gen::TotalBytesGlobalData for GlobalDataMock {
    fn set_sum_byte_length(&mut self, sum_byte_length: usize) {
        self.sum_byte_length = sum_byte_length;
    }
}
impl hyperast::tree_gen::AccIndentation for AccMock {
    fn indentation(&self) -> &hyperast::tree_gen::Spaces {
        todo!()
    }
}
impl hyperast::tree_gen::WithByteRange for AccMock {
    fn begin_byte(&self) -> usize {
        self.begin_byte
    }
    fn end_byte(&self) -> usize {
        self.end_byte
    }
    fn has_children(&self) -> bool {
        !self.children.is_empty()
    }
}
impl hyperast::tree_gen::Accumulator for AccMock {
    type Node = ();

    fn push(&mut self, _full_node: Self::Node) {}
}
impl hyperast::tree_gen::TreeGen for CppTreeGenMock {
    type Acc = AccMock;

    type Global = GlobalDataMock;

    fn make(
        &mut self,
        _global: &mut Self::Global,
        _acc: <Self as hyperast::tree_gen::TreeGen>::Acc,
        _label: Option<String>,
    ) -> <<Self as hyperast::tree_gen::TreeGen>::Acc as hyperast::tree_gen::Accumulator>::Node {
        ()
    }
}
impl hyperast::tree_gen::ZippedTreeGen for CppTreeGenMock {
    type Stores = ();
    type Text = [u8];
    type Node<'b> = hyperast::tree_gen::utils_ts::TNode<'b>;
    type TreeCursor<'b> = hyperast::tree_gen::utils_ts::TTreeCursor<'b, true>;

    fn init_val(&mut self, _text: &Self::Text, node: &Self::Node<'_>) -> Self::Acc {
        let kind = node.0.kind();
        let begin_byte = node.start_byte();
        let end_byte = node.end_byte();
        // eprintln!(
        //     "pre \t{}-{}\t{}\t{}\t{}",
        //     begin_byte,
        //     end_byte,
        //     "  ",
        //     node.0.is_missing(),
        //     kind
        // );
        let children = vec![""; node.0.descendant_count() - 1];
        AccMock {
            kind,
            padding_start: 0,
            begin_byte,
            end_byte,
            children,
        }
    }
    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &hyperast::tree_gen::Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> hyperast::tree_gen::PreResult<Self::Acc> {
        if cursor.node().is_missing() {
            return hyperast::tree_gen::PreResult::Skip;
        }
        use hyperast::tree_gen::parser::TreeCursor;
        let pre = self.pre(text, &cursor.node(), stack, global);
        hyperast::tree_gen::PreResult::Ok(pre)
    }
    fn pre(
        &mut self,
        _text: &Self::Text,
        node: &Self::Node<'_>,
        _stack: &hyperast::tree_gen::Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> <Self as hyperast::tree_gen::TreeGen>::Acc {
        let kind = node.0.kind();
        let begin_byte = node.start_byte();
        let end_byte = node.end_byte();
        dbg!(end_byte);

        if node.start_byte() < global.sum_byte_length {
            panic!(
                "broken invariant {} < {}",
                node.start_byte(),
                global.sum_byte_length
            );
        }
        let children = vec![""; node.0.descendant_count() - 1];
        let padding_start = global.sum_byte_length;
        AccMock {
            kind,
            begin_byte,
            end_byte,
            padding_start,
            children,
        }
    }

    fn post(
        &mut self,
        _parent: &mut Self::Acc,
        global: &mut Self::Global,
        text: &Self::Text,
        acc: Self::Acc,
    ) {
        let AccMock {
            padding_start,
            begin_byte,
            end_byte,
            ..
        } = acc;
        let spc = std::str::from_utf8(&text[padding_start..begin_byte]).unwrap();
        dbg!(spc);
        dbg!(end_byte);
        global.set_sum_byte_length(end_byte);
        use hyperast::tree_gen::WithByteRange;
        if acc.has_children() {
            assert_eq!(global.sum_byte_length, end_byte)
        }
        assert!(global.sum_byte_length <= end_byte);
    }

    fn stores(&mut self) -> &mut Self::Stores {
        &mut self.0
    }
}
