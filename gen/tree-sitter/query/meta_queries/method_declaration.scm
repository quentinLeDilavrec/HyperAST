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
