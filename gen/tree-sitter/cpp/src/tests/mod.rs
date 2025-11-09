use hyperast::tree_gen::NoOpMore;

use crate::types::TStore;

type CppTreeGen<'store, 'cache, HAST, Acc> =
    crate::legion::CppTreeGen<'store, 'cache, TStore, NoOpMore<HAST, Acc>, true>;
type SimpleStores = hyperast::store::SimpleStores<TStore>;

mod prepro;
mod simple;
mod types;
mod zipper_it;
mod zipper_mock;

/// This example caused an issue related to missing nodes.
///
/// The missing node as an empty span, so it should not cause the cursor to move--even up or down.
static EX_ISSUE_MISSING_NODE: &str = r#"#ifndef TREE_SITTER_UPPER_PARSER_NAME_H_
#define TREE_SITTER_UPPER_PARSER_NAME_H_

typedef struct TSLanguage TSLanguage;

#ifdef __cplusplus
extern "C" {
#endif

const TSLanguage *tree_sitter_PARSER_NAME(void);

#ifdef __cplusplus
}
#endif

#endif // TREE_SITTER_UPPER_PARSER_NAME_H_
"#;
