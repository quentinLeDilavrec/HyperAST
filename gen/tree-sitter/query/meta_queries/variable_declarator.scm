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
    (identifier) (#EQ? "local_variable_declaration")
    (named_node
        (identifier) (#EQ? "variable_declarator") .
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
    (identifier) (#EQ? "variable_declarator")
    (named_node
        (identifier) (#EQ? "identifier")
    ) @_rm.all.full .
    (named_node
        (identifier) (#EQ? "method_invocation")
        (named_node) @_rm.all.full
        (predicate) @_rm.all.full
    ) .
)
(named_node
    (identifier) (#EQ? "variable_declarator") .
    (named_node
        (identifier) (#EQ? "identifier")
    ) @_rm.all.full .
    (anonymous_node) @_rm.all.full .
)

(named_node
    (identifier) (#EQ? "variable_declarator") .
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (named_node
        (identifier) (#EQ? "null_literal")
    ) @_rm.all.full .
)

(named_node
  (identifier) (#EQ? "local_variable_declaration")
  (named_node) .
  (named_node
    (identifier) (#EQ? "variable_declarator") .
    (named_node
        (identifier) (#EQ? "identifier") .
    )
  ) @_rm.all.full .
)

(named_node
  (identifier) (#EQ? "variable_declarator") .
) @_rm.all.full

(named_node
  (identifier) (#EQ? "variable_declarator") .
  (named_node
    (identifier) (#EQ? "identifier") .
  ) .
) @_rm.all.full

(named_node
  (identifier) (#EQ? "variable_declarator") .
) @_rm.all.full
