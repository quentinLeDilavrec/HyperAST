pub const Q1: &str = "(try_statement)";

pub const Q2: &str = r#"(try_statement
    (block
        (expression_statement
            (method_invocation
                (identifier) (#EQ? "fail")
            )
        )
    )
    (catch_clause)
) @root"#;

pub const Q3: &str = r#"(try_statement
(block
    (expression_statement
        (method_invocation
            (identifier) (#EQ? "fail")
        )
    )
)
(catch_clause)
) @root

(try_statement
(block
    (expression_statement
        (method_invocation
            (identifier) (#EQ? "fail")
        )
    ) .
)
(catch_clause)
) @root

(try_statement
(block .
    (expression_statement
        (method_invocation
            (identifier) (#EQ? "fail")
        )
    ) .
)
(catch_clause)
) @root

(try_statement
(block . (_) .
    (expression_statement
        (method_invocation
            (identifier) (#EQ? "fail")
        )
    ) .
)
(catch_clause)
) @root"#;
