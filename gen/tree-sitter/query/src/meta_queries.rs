/// queries used to generate the initial patterns from examples
pub static META_GEN: &str = r#"[
"{" "}" ";" "." "," "=" "(" ")" "[" "]" "!"
"try" "catch" "import" "finally" "return" "throw" "if" "else"
(line_comment) (block_comment)
] @skip
(type_identifier) @label
(identifier) @label
(_literal) @abstract"#;

// queries used iteratively to simplify patterns until a fixed point
pub static META_SIMP: &str = concat!(
    include_str!("../meta_queries/expressions.scm"),
    include_str!("../meta_queries/primary_expressions.scm"),
    include_str!("../meta_queries/statements.scm"),
    include_str!("../meta_queries/method_invocation.scm"),
    include_str!("../meta_queries/method_declaration.scm"),
    include_str!("../meta_queries/variable_declarator.scm"),
    include_str!("../meta_queries/try_statements.scm"),
    include_str!("../meta_queries/rest.scm"),
);

#[test]
fn non_empty_meta_queries() {
    assert_ne!(0, META_SIMP.len());
}

#[test]
#[ignore]
fn p() {
    println!("{}", META_SIMP)
}

const _TRY_FAIL_CATCH: &str = r#"(named_node
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
)"#;
const __TRY_FAIL_CATCH: &str = r#"(named_node
    (identifier) (#EQ? "try_statement")
) @uniq
(named_node
    (identifier) (#EQ? "method_invocation")
) @need
(named_node
    (identifier) (#EQ? "catch_type")
) @rm.all.full
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
    (identifier) (#EQ? "unary_expression")
    (named_node
        (identifier) .
    )
) @rm
(named_node
    (identifier) (#EQ? "field_access")
    (named_node
    )
    (named_node
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
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
        )
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
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
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
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
    .
    (named_node
        (identifier) (#EQ? "block")
    ) @rm.all.full
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
        (identifier) (#EQ? "identifier") .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string) @label
            )
        ) @pred
    )
) @rm.all
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier") .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
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
)"#;
