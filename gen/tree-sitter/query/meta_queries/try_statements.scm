
(named_node
    (identifier) (#EQ? "catch_type")
) @_rm

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
    .
    (named_node
        (identifier) (#EQ? "block") .
    ) @rm.all.full
)

(named_node
    (identifier) (#EQ? "finally_clause")
    .
    (named_node
        (identifier) (#EQ? "block")
    ) @rm.all.full
)

(named_node
    (identifier) (#EQ? "catch_clause")
    .
    (named_node
        (identifier) (#EQ? "catch_formal_parameter")
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
    ) @rm.all.full .
    (named_node
        (identifier) (#EQ? "block") .
    )? @rm.all.full .
)



(named_node
    (identifier) (#EQ? "catch_formal_parameter")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
) @rm.all.full

(named_node
    (identifier) (#EQ? "catch_formal_parameter") .
    (named_node
        (identifier) .
    ) .
) @rm.all.full

(named_node
    (identifier) (#EQ? "catch_formal_parameter")
    (named_node
        (identifier) (#EQ? "modifiers")
    ) @rm.all.full
)
