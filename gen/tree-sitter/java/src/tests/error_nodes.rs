//! HyperAST (the generator of the git processor, `legion_ts_simp`) and Tree-sitter's own query
//! cursor count the same matches on the children of error nodes.

/// The children of an error node are held by hidden repeat nodes (`ts_builtin_sym_error_repeat`,
/// `_ERROR`). Tree-sitter has no public symbol for them: `kind_id` read past the end of the
/// language's `public_symbol_map` (out of the grammar's kinds: the index out of bounds of
/// `Type::from_u16`, also in release); and `_ERROR` was not hidden, so the query cursor did not
/// see the children under it (1 match instead of 8 for the second query).
#[test]
fn java_query_on_error_node_children() {
    use streaming_iterator::StreamingIterator;
    let text = "class A { void f() { 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 } }\n";
    let tree = crate::legion_with_refs::tree_sitter_parse(text.as_bytes());
    let mut stores = hyperast::store::SimpleStores::<crate::TStore>::default();
    let mut tree_gen = crate::legion_ts_simp::JavaTreeGen::bare(&mut stores);
    let file = tree_gen
        .generate_file(b"", text.as_bytes(), tree.walk())
        .local
        .compressed_node;
    for query in [
        "(ERROR (decimal_integer_literal) @n)",
        "(ERROR (hex_integer_literal) @n)",
        "(ERROR (_) @c)",
        "(hex_integer_literal) @n",
    ] {
        let tree_sitter = {
            let q = tree_sitter::Query::new(&crate::language(), query).unwrap();
            let mut cursor = tree_sitter::QueryCursor::new();
            let mut matches = cursor.matches(&q, tree.root_node(), text.as_bytes());
            let mut count = 0;
            while matches.next().is_some() {
                count += 1;
            }
            count
        };
        let hyperast = {
            let q = hyperast_tsquery::Query::new(query, crate::language()).unwrap();
            let pos = hyperast::position::StructuralPosition::new(file);
            let cursor = hyperast_tsquery::hyperast_cursor::TreeCursor::new(&stores, pos);
            q.matches(cursor).count()
        };
        assert_eq!(hyperast, tree_sitter, "{query}");
    }
}
