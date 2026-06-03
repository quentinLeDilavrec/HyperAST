use hyperast::tree_gen::NoOpMore;

use crate::types::TStore;

type SimpleStores = hyperast::store::SimpleStores<TStore>;
type CppTreeGen<'store, 'cache, HAST, Acc> =
    crate::legion::CppTreeGen<'store, 'cache, TStore, SimpleStores, NoOpMore<HAST, Acc>, true>;

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

static EX_ISSUE_MISSING_NODE2: &str = r#"int f() {
if (asprintf(&p, "/proc/"PID_FMT"/task/"PID_FMT"/comm", pid, tid) < 0)
    return -ENOMEM;
}
"#;

/// https://github.com/systemd/systemd/blob/904dcaf9d4933499f8334859f52ea8497f2d24ff/src/login/pam_systemd.c
static EX_ISSUE_MISSING_NODE3: &str = r#"/* SPDX-License-Identifier: LGPL-2.1+ */
static int parse_argv() {
        for (i = 0; i < (unsigned) argc; i++) {
                if (startswith(argv[i], "class=")) {
                        if (class)
                                *class = argv[i] + 6;
                } else if (startswith(argv[i], "debug=")) {
                        int k;
                        k = parse_boolean(argv[i] + 6);
                        if (k < 0)
                                pam_syslog(handle, LOG_WARNING, "Failed to parse debug= argument, ignoring.");
                        else if (debug)
                                *debug = k;
                } else
                        pam_syslog(handle, LOG_WARNING, "Unknown parameter '%s', ignoring", argv[i]);
        }
        return 0;
}
"#;
