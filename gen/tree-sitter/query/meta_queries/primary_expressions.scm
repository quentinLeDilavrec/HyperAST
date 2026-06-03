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
    (identifier) (#EQ? "array_type")
    (named_node
        (identifier) (#EQ? "dimensions")
    ) @rm.all.full .
)
