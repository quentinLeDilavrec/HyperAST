pub(crate) fn only_parse_query_precomp<'a>(
    precomputeds: &[&str],
    language: tree_sitter::Language,
) -> Result<hyperast_tsquery::Query, tree_sitter::QueryError> {
    static DQ: &str = "(_)";
    use hyperast_tsquery::Query;
    let (precomp, _) = Query::with_precomputed(DQ, language, precomputeds)?;
    Ok(precomp)
}
