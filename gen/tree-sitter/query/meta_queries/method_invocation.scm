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
) @_rm
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
    )
    (named_node
        (identifier) (#EQ? "argument_list")
        (named_node
            (identifier) (#EQ? "identifier")
            .
        ) .
    )
) @_rm
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
    )
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @_rm
(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
        (identifier) (#EQ? "identifier")
    ) @_rm.all.full .
    (predicate
        (identifier) (#EQ? "EQ")
    ) @_rm.all.full .
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
    )
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
    ) @_pred
) @_rm.all

(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred @rm.all
)

(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "argument_list")
        (named_node
            (identifier) (#EQ? "method_invocation")
            [(named_node) (predicate)]* @_rm.all.full
        ) .
    )
)

(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "argument_list") .
    ) @rm.all.full
)

(named_node
    (identifier) (#EQ? "argument_list") .
    (named_node
        (identifier) (#EQ? "string_literal")
    ) @rm .
)

(named_node
    (identifier) (#EQ? "argument_list") .
    (named_node
        (identifier) (#EQ? "decimal_floating_point_literal")
    ) @rm .
)

(named_node
    (identifier) (#EQ? "argument_list") .
    (named_node
        (identifier) (#EQ? "true")
    ) @rm .
)

(named_node
    (identifier) (#EQ? "argument_list") .
    (named_node
        (identifier) (#EQ? "false")
    ) @rm .
)

(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
        (identifier) (#EQ? "identifier") .
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
)

(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
      (identifier) (#EQ? "identifier")
    ) @_rm .
    (named_node
        (identifier) (#EQ? "identifier")
    )
)


(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "method_invocation")
    ) @rm.all.full
)

(named_node
    (identifier) (#EQ? "argument_list") .
    (named_node
      (identifier) (#EQ? "binary_expression")
    ) @rm.all.full
)


(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "method_invocation")
    ) @rm.final
)
