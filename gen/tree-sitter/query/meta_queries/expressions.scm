(named_node
    (identifier) (#EQ? "generic_type")
) @rm.all.full
(named_node
    (identifier) (#EQ? "cast_expression")
    .
) @rm.all.full
(named_node
    (identifier) (#EQ? "class_literal")
    (anonymous_node) @rm.all.full (#EQ? "\"class\"")
)

(named_node
    (identifier) (#EQ? "unary_expression")
    (named_node
        (identifier) .
    )
) @rm.all.full
(named_node
    (identifier) (#EQ? "unary_expression")
    (anonymous_node)
) @rm.all.full
(named_node
    (identifier) (#EQ? "array_creation_expression") .
    (anonymous_node) (#EQ? "\"new\"")
) @rm.all.full
(named_node
    (identifier) (#EQ? "binary_expression")
    (named_node
        (identifier) (#EQ? "string_literal")
    ) .
    (anonymous_node) .
) @rm.all.full
(named_node
    (identifier) (#EQ? "binary_expression")
    (named_node (identifier) (#EQ? "string_literal")) .
    (anonymous_node) .
    (named_node (identifier) (#EQ? "string_literal")) .
) @rm.all.full
(named_node
    (identifier) (#EQ? "binary_expression") .
    (anonymous_node) .
    (named_node
        (identifier) (#EQ? "string_literal")
    )
) @rm.all.full
(named_node
    (identifier) (#EQ? "binary_expression") .
    (anonymous_node) .
) @rm.all.full
(named_node
    (identifier) (#EQ? "binary_expression")
    (named_node
        (identifier) (#EQ? "identifier")
    ) @rm.all .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred @rm.all
)

(named_node
    (identifier) (#EQ? "binary_expression")
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
        ) @pred .
        (named_node
            (identifier) (#EQ? "identifier")
        )
    ) @rm.all
)

(named_node
    (identifier) (#EQ? "cast_expression") .
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
    (identifier) (#EQ? "field_access") .
    (named_node
      (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred .
    (named_node)
)
(named_node
    (identifier) (#EQ? "object_creation_expression")
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm.all.full

(named_node
    (identifier) (#EQ? "instanceof_expression")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred @rm.all
) @rm.all.full

(named_node
    (identifier) (#EQ? "field_access") .
    (named_node
      (identifier) (#EQ? "field_access") .
      (named_node
        (identifier) (#EQ? "field_access") .
        (named_node
          (identifier) (#EQ? "field_access")
        )
      )
    ) @rm .
    (named_node
      (identifier) (#EQ? "identifier") .
    )
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) (#MATCH? "[A-Z].+")
        )
    ) .
)
