//! HyperAST (the generator of the git processor, `legion_ts_simp`) and Tree-sitter's own query
//! cursor count the same matches: on a file without parse errors, and on the children of error
//! nodes.

use hyperast::tree_gen::parser::{Node as _, NodeWithU16TypeId as _, TreeCursor as _};
use hyperast::tree_gen::utils_ts::TTreeCursor;

use crate::legion::tree_sitter_parse;

type SimpleStores = hyperast::store::SimpleStores<crate::TStore>;

/// The matches of a query on a C file with HyperAST.
fn hyperast_matches(query: &str, text: &str) -> usize {
    let query = hyperast_tsquery::Query::new(query, crate::language()).unwrap();
    let tree = tree_sitter_parse(text.as_bytes());
    let mut stores = SimpleStores::default();
    let mut tree_gen = crate::legion_ts_simp::CTreeGen::bare(&mut stores);
    let file = tree_gen
        .generate_file(b"", text.as_bytes(), tree.walk())
        .local
        .compressed_node;
    let pos = hyperast::position::StructuralPosition::new(file);
    let cursor = hyperast_tsquery::hyperast_cursor::TreeCursor::new(&stores, pos);
    query.matches(cursor).count()
}

/// The matches of a query on a C file with Tree-sitter's own query cursor.
fn tree_sitter_matches(query: &str, text: &str) -> usize {
    use streaming_iterator::StreamingIterator;
    let query = tree_sitter::Query::new(&crate::language(), query).unwrap();
    let tree = tree_sitter_parse(text.as_bytes());
    let mut cursor = tree_sitter::QueryCursor::new();
    let mut matches = cursor.matches(&query, tree.root_node(), text.as_bytes());
    let mut count = 0;
    while matches.next().is_some() {
        count += 1;
    }
    count
}

/// A bare list of bytes (as u-boot's FPGA bitstreams, `board/**/fpgadata*.c`): not valid C on
/// its own, one error node with thousands of children.
fn bare_byte_list(n: usize) -> String {
    (0..n)
        .map(|i| {
            let indent = if i % 16 == 0 { " " } else { "" };
            let eol = if i % 16 == 15 { "\n" } else { "" };
            format!("{indent}0x{:02x},{eol}", (i * 37) % 256)
        })
        .collect()
}

/// The children of an error node are held by hidden repeat nodes (`ts_builtin_sym_error_repeat`,
/// `_ERROR`), which the hidden-node cursor (`TTreeCursor<true>`) reaches. Tree-sitter has no
/// public symbol for them: `kind_id` read past the end of the language's `public_symbol_map`
/// (1204 in debug, out of the 363 kinds of the grammar: the index out of bounds of
/// `Type::from_u16`; `register` in release). Every node gets a kind of the grammar, and the
/// repeat nodes the reserved `_ERROR`.
#[test]
fn hidden_cursor_kinds_in_range_on_a_bare_byte_list() {
    let text = bare_byte_list(6_600);
    let tree = tree_sitter_parse(text.as_bytes());
    let kind_count = crate::language().node_kind_count() as u16;
    let mut cursor = TTreeCursor::<true>(tree.walk());
    let mut error_repeats = 0;
    let mut bad = vec![];
    'walk: loop {
        let kind = cursor.node().kind_id();
        let node = cursor.0.node();
        if node.grammar_id() == u16::MAX - 1 {
            error_repeats += 1;
            if kind != u16::MAX - 1 {
                bad.push(format!("error repeat at {}: kind {kind}", node.start_byte()));
            }
        } else if kind >= kind_count && kind != u16::MAX {
            bad.push(format!("{} at {}: kind {kind}", node.kind(), node.start_byte()));
        }
        if cursor.goto_first_child_extended().is_some() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling_extended().is_some() {
                break;
            }
            if cursor.goto_parent_extended().is_none() {
                break 'walk;
            }
        }
    }
    assert!(error_repeats > 0);
    assert!(bad.is_empty(), "{} nodes of {error_repeats} error repeats or out of range, e.g. {:?}", bad.len(), &bad[..bad.len().min(5)]);
}

/// The children of an error node, under its hidden `_ERROR` repeat nodes: typed as another kind
/// (`register` in release), or not hidden, the repeat nodes hid all the children but the first
/// from the query cursor (1 match instead of 8 for the first query).
#[test]
fn c_query_on_error_node_children_test() {
    let text = " 0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,\nint y = 1;\n";
    for query in [
        "(ERROR (number_literal) @n)",
        "(ERROR (number_literal) (number_literal)) @e",
        "(number_literal) @n",
    ] {
        let tree_sitter = tree_sitter_matches(query, text);
        let hyperast = hyperast_matches(query, text);
        assert_eq!(hyperast, tree_sitter, "{query}");
    }
}

/// A C file without parse errors, with most kinds of declarations, statements and expressions.
const VARIED: &str = r#"#include <stdio.h>
#define MAX(a, b) ((a) > (b) ? (a) : (b))

struct point { int x; int y; };
typedef enum { RED, GREEN = 2 } color;
union value { int i; float f; };
static const char *names[] = {"a\tb", "c\n", "plain"};

static int sum(const int *values, size_t n) {
    int total = 0;
    for (size_t i = 0; i < n; i++) {
        total += values[i];
    }
    return total;
}

int main(int argc, char **argv) {
    struct point p = {.x = 1, .y = 2};
    color c = GREEN;
    int values[] = {1, 2, 3};
    char *s = argc > 1 ? argv[1] : "none";
    while (p.x < 10) p.x++;
    do { p.y--; } while (p.y > 0);
    switch (c) {
    case RED: puts("red"); break;
    default: break;
    }
    if (sum(values, sizeof values / sizeof *values) != 6 && !s) {
        goto end;
    }
    printf("%d %s %c\n", MAX(p.x, p.y), names[0], 'z');
end:
    return (int) 0.5f;
}
"#;

/// HyperAST and Tree-sitter count the same matches for every named node type of a C file, and
/// for the children of string literals. On a file without parse errors: HyperAST leaves out the
/// empty nodes of Tree-sitter's error recovery (e.g. `(expression_statement (MISSING ";"))` after
/// a label that ends a block), which Tree-sitter's queries match.
#[test]
fn c_same_matches_as_tree_sitter_test() {
    let text = VARIED.to_string();
    let tree = tree_sitter_parse(text.as_bytes());
    assert!(!tree.root_node().has_error(), "{}", tree.root_node().to_sexp());
    let mut kinds = std::collections::BTreeSet::new();
    let mut stack = vec![tree.root_node()];
    while let Some(node) = stack.pop() {
        if node.is_named() && !node.is_error() {
            kinds.insert(node.kind().to_string());
        }
        let mut walk = node.walk();
        stack.extend(node.children(&mut walk));
    }
    let mut queries: Vec<String> = kinds.iter().map(|kind| format!("({kind}) @c")).collect();
    queries.push("(string_literal (string_content)) @c".to_string());
    queries.push("(string_literal (escape_sequence)) @c".to_string());
    let differ: Vec<String> = queries
        .iter()
        .filter_map(|query| {
            let hyperast = hyperast_matches(query, &text);
            let tree_sitter = tree_sitter_matches(query, &text);
            (hyperast != tree_sitter)
                .then(|| format!("{query}: HyperAST {hyperast}, Tree-sitter {tree_sitter}"))
        })
        .collect();
    assert!(queries.len() > 60, "too few node types: {}", queries.len());
    assert!(differ.is_empty(), "{} of {} queries differ:\n{}", differ.len(), queries.len(), differ.join("\n"));
}
