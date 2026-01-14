use crate::app::types::Config;
use crate::app::utils::{Commit, Forge, Repo};

#[derive(Debug, Clone)]
pub struct Example {
    pub(super) name: &'static str,
    pub(super) commit: Commit,
    pub(super) meta_gen: &'static str,
    pub(super) meta_simp: &'static str,
    pub(super) config: Config,
    pub(super) simple_matching: bool,
    pub(super) prepro_matching: bool,
}

impl Into<super::ComputeConfigQuery> for &Example {
    fn into(self) -> super::ComputeConfigQuery {
        super::ComputeConfigQuery {
            //google/gson/commit/99cc4cb11f73a6d672aa6381013d651b7921e00f
            //Marcono1234/gson/commit/3d241ca0a6435cbf1fa1cdaed2af8480b99fecde
            commit: Into::into(&self.commit),
            len: 1,
            meta_gen: self.meta_gen.into(),
            meta_simp: self.meta_simp.into(),
            config: self.config,
            simple_matching: self.simple_matching,
            prepro_matching: self.prepro_matching,
            wanted_matches: usize::MAX..usize::MAX,
            advanced_open: false,
            examples: vec![],
        }
    }
}

impl Example {
    pub(super) fn show(&self, ui: &mut egui::Ui) -> egui::Response {
        ui.button(self.name)
    }
}

pub(super) static BASE_TRY_FAIL_CATCH_EX: Example = Example {
    commit: Commit {
        repo: Repo {
            forge: Forge::GitHub,
            user: "Marcono1234",
            name: "gson",
        },
        id: "3d241ca0a6435cbf1fa1cdaed2af8480b99fecde",
    },
    name: "simple try fail catch",
    meta_gen: r#"(identifier) @label
["{" ";" "." "try" "(" ")" "}" "catch" "import"] @skip"#,
    meta_simp: r#"(predicate
    (identifier) (#EQ? "EQ")
    (parameters
        (string) @label
    )
) @pred
(_
    (named_node
        (identifier) (#EQ? "expression_statement")
    ) @rm
    .
)
(_
    (named_node
        (identifier) (#EQ? "expression_statement")
    ) @rm
    .
    (named_node)
)
(_
    (named_node
        (identifier) (#EQ? "expression_statement")
    ) @rm
    .
    (anonymous_node)
)"#,
    config: Config::MavenJava,
    simple_matching: true,
    prepro_matching: true,
};

pub(super) static MORE_TRY_FAIL_CATCH_EX: Example = Example {
    name: "more on try fail catch",
    commit: Commit {
        repo: Repo {
            forge: Forge::GitHub,
            user: "Marcono1234",
            name: "gson",
        },
        id: "3d241ca0a6435cbf1fa1cdaed2af8480b99fecde",
    },
    meta_gen: r#"[
    "{" ";" "." "try" "(" ")" "}" "catch" "import"
    (line_comment) (block_comment)
] @skip
(type_identifier) @label
(identifier) @label
(_literal) @abstract"#,
    meta_simp: r#"(named_node
    (identifier) (#EQ? "try_statement")
) @uniq
(named_node
    (identifier) (#EQ? "catch_type")
) @rm
(named_node
    (identifier) (#EQ? "string_literal")
) @rm.all.full
(named_node
    (identifier) (#EQ? "decimal_integer_literal")
) @rm.all.full
(named_node
    (identifier) (#EQ? "null_literal")
) @rm.all.full
(named_node
    (identifier) (#EQ? "generic_type")
) @rm.all.full
(named_node
    (identifier) (#EQ? "cast_expression")
    .
) @rm.all.full
(named_node
    (identifier) (#EQ? "unary_expression")
    (named_node
        (identifier) .
    )
) @rm
(named_node
    (identifier) (#EQ? "object_creation_expression")
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm.all.full
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
    (named_node
        (identifier) (#EQ? "argument_list")
        (named_node
            (identifier) (#EQ? "identifier")
            .
        ) .
    )
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm
(named_node
    (identifier) (#EQ? "block") .
    (named_node
        (identifier) (#EQ? "try_statement")
    ) @focus .
    (capture
        (identifier) (#EQ? "_root")
    )
)
(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
        (identifier) (#EQ? "identifier")
    ) @rm.all.full .
    (predicate
        (identifier) (#EQ? "EQ")
    ) @rm.all.full .
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
    )
)
(named_node
    (identifier) (#EQ? "catch_formal_parameter")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred
) @rm.all
(named_node
    (identifier) (#EQ? "catch_clause")
    (named_node
        (identifier) (#EQ? "catch_formal_parameter")
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
        )
    ) @rm.all.full
    (named_node
        (identifier) (#EQ? "block")
        .
    )
)
(named_node
    (identifier) (#EQ? "catch_clause")
    (named_node
        (identifier) (#EQ? "catch_formal_parameter")
        (named_node
            (identifier) (#EQ? "identifier")
        )
    ) @rm
    (named_node
        (identifier) (#EQ? "block")
        .
    )
)
(named_node
    (identifier) (#EQ? "catch_clause")
    (named_node
        (identifier) (#EQ? "block")
        (named_node
            (identifier) (#EQ? "expression_statement") .
        ) @rm
    )
)
(named_node
    (identifier) (#EQ? "catch_clause")
    .
    (named_node
        (identifier) (#EQ? "block")
        .
    ) @rm
)
(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "method_invocation") .
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string) @label
            )
        ) @pred
    )
) @rm.all
(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "method_invocation") .
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
        )
    ) @rm
)
(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred
) @rm.all
(named_node
    (identifier) (#EQ? "variable_declarator")
    (named_node
        (identifier) (#EQ? "method_invocation")
        (named_node
            (identifier) (#EQ? "argument_list")
        ) @rm.all.full
    ) .
)
(named_node
    (identifier) (#EQ? "variable_declarator")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred @rm.all
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred @rm.all
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "block") .
    ) @rm.all.full
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "block")
        (named_node
            (identifier) (#EQ? "local_variable_declaration")
        ) @rm.all.full
    )
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "formal_parameters")
        (named_node
            (identifier) (#EQ? "formal_parameter")
            (named_node
                (identifier) (#EQ? "identifier") .
            ) .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @pred @rm.all
        )
    )
)
(named_node
    (identifier) (#EQ? "enhanced_for_statement")
    (named_node
        (identifier) (#EQ? "block")
    ) @rm
)
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier") .
    )
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    )
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm.all.full
(named_node .
    (identifier) .
    "/" @rm.all.full .
    (identifier) @rm.all.full
)"#,
    config: Config::MavenJava,
    simple_matching: true,
    prepro_matching: true,
};

pub(super) static BALANCED_EX: Example = Example {
    commit: Commit {
        repo: Repo {
            forge: Forge::GitHub,
            user: "Marcono1234",
            name: "gson",
        },
        id: "3d241ca0a6435cbf1fa1cdaed2af8480b99fecde",
    },
    // only label identifiers
    // only do removes
    name: "balanced synth",
    meta_gen: r#"[
    "{" "}" ";" "." "try" "(" ")" "catch" "import"
    "if" "else"
    (line_comment) (block_comment)
] @skip
;(type_identifier) @label
(identifier) @label
(_literal) @abstract"#,
    meta_simp: r#"(_
    (named_node
        (identifier) (#IS? "statement")
    ) @rm
    .
)"#,
    config: Config::MavenJava,
    simple_matching: true,
    prepro_matching: true,
};
