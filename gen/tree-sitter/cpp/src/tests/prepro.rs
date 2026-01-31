//! Test the PrePost iterator that provides all the nodes from tree-sitter's CST.
//!
//! Even the hidden nodes
use hyperast::tree_gen::parser::Node as _;

use crate::legion::tree_sitter_parse;
use crate::tests::EX_ISSUE_MISSING_NODE3;

use super::EX_ISSUE_MISSING_NODE;
use super::EX_ISSUE_MISSING_NODE2;

#[ignore]
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

// TODO consolidate a proper helper to assert the invariants

#[ignore]
#[test_log::test]
pub(crate) fn cpp_parsing_error2_prepost_test() {
    let text = EX_ISSUE_MISSING_NODE2.as_bytes();
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
    let mut global_start;
    let mut global_end;
    let mut finished_offset = 0;
    if let Some(cursor) = cursor {
        use hyperast::tree_gen::parser::TreeCursor;
        let node = cursor.node();
        let (start, end) = (node.start_byte(), node.end_byte());
        global_start = start;
        global_end = end;
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
    } else {
        unreachable!()
    };
    dbg!();
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
            assert!(start <= end);
            assert!(global_start <= global_end);
            assert!(start <= global_start);
            assert!(end >= global_end);
            global_start = start;
            global_end = end;
            assert!(finished_offset <= end);
            finished_offset = end;
        } else {
            let dir = if dir == "r" {
                let dir = "u r"; // going up phase of going right
                let (start, end) = os.pop().unwrap();
                let kind = ts.pop().unwrap();
                eprintln!("{vis:?}\t{dir}\t     \t{start}-{end}\t{kind}",);
                assert!(start <= end);
                assert!(global_start <= global_end);
                if global_end < end {
                    assert!(start <= global_start);
                    dbg!(global_start, global_end);
                } else if global_end == end {
                    assert!(start <= global_start);
                    dbg!(global_start, global_end);
                } else {
                    panic!()
                }
                global_start = start;
                global_end = end;
                assert!(finished_offset <= end);
                finished_offset = end;
                dbg!(finished_offset);
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
            assert!(start <= end);
            assert!(global_start <= global_end);
            if global_start == start {
                // <(a)>
                // <(>a)
                assert!(end <= global_end);
                dbg!();
            } else if global_start < start {
                // <(>a)
                // (<a>)
                assert!(
                    global_end <= end,
                    "{global_start} {start} {global_end} {end}",
                );
                dbg!(global_start, global_end);
            } else {
                panic!()
            }
            assert!(finished_offset <= start);
            global_start = start;
            global_end = end;
            ts.push(kind);
            os.push((node.start_byte(), node.end_byte()));
            continue;
        }
    }
    assert_eq!(ts.len(), 1);
}

#[ignore]
#[test_log::test]
pub(crate) fn cpp_parsing_error3_prepost_test() {
    let text = EX_ISSUE_MISSING_NODE3.as_bytes();
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
    let mut global_start;
    let mut global_end;
    let mut finished_offset = 0;
    if let Some(cursor) = cursor {
        use hyperast::tree_gen::parser::TreeCursor;
        let node = cursor.node();
        let (start, end) = (node.start_byte(), node.end_byte());
        global_start = start;
        global_end = end;
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
    } else {
        unreachable!()
    };
    dbg!();
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
            assert!(start <= end);
            assert!(global_start <= global_end);
            assert!(start <= global_start);
            assert!(end >= global_end);
            global_start = start;
            global_end = end;
            assert!(finished_offset <= end);
            finished_offset = end;
        } else {
            let dir = if dir == "r" {
                let dir = "u r"; // going up phase of going right
                let (start, end) = os.pop().unwrap();
                let kind = ts.pop().unwrap();
                eprintln!("{vis:?}\t{dir}\t     \t{start}-{end}\t{kind}",);
                assert!(start <= end);
                assert!(global_start <= global_end);
                if global_end < end {
                    assert!(start <= global_start);
                    dbg!(global_start, global_end);
                } else if global_end == end {
                    assert!(start <= global_start);
                    dbg!(global_start, global_end);
                } else {
                    panic!()
                }
                global_start = start;
                global_end = end;
                assert!(finished_offset <= end);
                finished_offset = end;
                dbg!(finished_offset);
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
            assert!(start <= end);
            assert!(global_start <= global_end);
            if global_start == start {
                // <(a)>
                // <(>a)
                assert!(end <= global_end);
                dbg!();
            } else if global_start < start {
                // <(>a)
                // (<a>)
                assert!(
                    global_end <= end,
                    "{global_start} {start} {global_end} {end}",
                );
                dbg!(global_start, global_end);
            } else {
                panic!()
            }
            assert!(finished_offset <= start);
            global_start = start;
            global_end = end;
            ts.push(kind);
            os.push((node.start_byte(), node.end_byte()));
            continue;
        }
    }
    assert_eq!(ts.len(), 1);
}
