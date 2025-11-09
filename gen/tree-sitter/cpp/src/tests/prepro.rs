//! Test the PrePost iterator that provides all the nodes from tree-sitter's CST.
//!
//! Even the hidden nodes
use hyperast::tree_gen::parser::Node as _;

use crate::legion::tree_sitter_parse;

use super::EX_ISSUE_MISSING_NODE;

#[test_log::test]
pub(crate) fn cpp_parsing_error_prepost_test() {
    let text = EX_ISSUE_MISSING_NODE.as_bytes();
    let tree = match tree_sitter_parse(text) {
        Ok(t) => t,
        Err(t) => t,
    };
    println!("{:#?}", tree.root_node().to_sexp());
    let cursor = hyperast::tree_gen::utils_ts::TTreeCursor::<true>(tree.walk());
    let mut prepost = hyperast::tree_gen::utils_ts::PrePost2::new(cursor);
    fn last_four<T: Copy>(ts: &[T]) -> Vec<T> {
        ts.iter().rev().take(4).rev().copied().collect::<Vec<_>>()
    }
    let mut ts = vec![];
    let mut os = vec![];
    let (cursor, has) = prepost.current();
    if let Some(cursor) = cursor {
        use hyperast::tree_gen::parser::TreeCursor;
        let node = cursor.node();
        let (start, end) = (node.start_byte(), node.end_byte());
        let kind = node.0.kind();
        let vis = "Visible";
        let dir = has.letter();
        eprintln!(
            "{vis}\t{dir}\t{}\t{start}-{end}\t{kind:20}\t{:?}",
            node.is_missing(),
            last_four(&ts),
        );
        ts.push(kind);
        os.push((node.start_byte(), node.end_byte()));
    };

    for _ in 0..92 {
        let Some(_vis) = prepost.next() else {
            break;
        };
        let (cursor, has) = prepost.current();
        let Some(cursor) = cursor else {
            continue;
        };
        use hyperast::tree_gen::parser::TreeCursor;
        let node = cursor.node();
        use hyperast::tree_gen::zipped::Has;
        if *has == Has::Up || *has == Has::Right {
            ts.pop().unwrap();
            os.pop();
        }
        if *has == Has::Down || *has == Has::Right {
            ts.push(node.0.kind());
            os.push((node.start_byte(), node.end_byte()));
        }
    }

    loop {
        let Some(vis) = prepost.next() else {
            break;
        };
        let (cursor, has) = prepost.current();
        let Some(cursor) = cursor else {
            continue;
        };
        use hyperast::tree_gen::parser::TreeCursor;
        let node = cursor.node();
        let dir = has.letter();
        if dir == "u" {
            let (start, end) = os.pop().unwrap();
            let kind = ts.pop().unwrap();
            eprintln!(
                "      \t{dir}\t  \t{start}-{end}\t{kind:20}\t{:?}",
                last_four(&ts),
            );
        } else {
            let dir = if dir == "r" {
                let dir = "u r"; // going up phase of going right
                let (start, end) = os.pop().unwrap();
                let kind = ts.pop().unwrap();
                eprintln!("{vis:?}\t{dir}\t     \t{start}-{end}\t{kind}",);
                "d r" // going down phase of going right
            } else {
                dir
            };
            let (start, end) = (node.start_byte(), node.end_byte());
            let kind = node.0.kind();
            eprintln!(
                "{vis:?}\t{dir}\t{}\t{start}-{end}\t{kind:20}\t{:?}",
                node.is_missing(),
                last_four(&ts),
            );
            ts.push(kind);
            os.push((node.start_byte(), node.end_byte()));
            continue;
        }
    }
}
