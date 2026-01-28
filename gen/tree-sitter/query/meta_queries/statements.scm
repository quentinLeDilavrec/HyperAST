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
  (named_node
      (identifier) (#EQ? "expression_statement")
  ) @_rm
)

(named_node
    (identifier) (#EQ? "enhanced_for_statement")
    (named_node
        (identifier) (#EQ? "block")
    ) @rm
)

(named_node
    (identifier) (#EQ? "throw_statement") .
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
    (identifier) (#EQ? "enhanced_for_statement")
    (named_node
        (identifier) (#EQ? "type_identifier")
    ) .
    (predicate) .
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
    (identifier) (#EQ? "enhanced_for_statement")
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
        (identifier) (#EQ? "block")
    ) .
) @rm.all


(named_node
    (identifier) (#EQ? "expression_statement")
    (named_node
        (identifier) (#EQ? "method_invocation")
    ) @rm.final
)

(named_node
    (identifier) (#EQ? "expression_statement")
    (named_node
        (identifier) (#EQ? "object_creation_expression")
    ) @rm.final
)

(named_node
    (identifier) (#EQ? "expression_statement")
    (named_node
        (identifier) (#EQ? "assignment_expression")
    ) @rm.final
)

(named_node
    (identifier) (#EQ? "while_statement")
    (named_node
        (identifier) (#EQ? "block")
    ) @rm.final
)

(named_node
    (identifier) (#EQ? "while_statement")
    (named_node
        (identifier) (#EQ? "parenthesized_expression")
    ) @rm.final
)

(named_node
  (named_node
      (identifier) (#EQ? "local_variable_declaration")
  ) @rm.final
)

(named_node
    (identifier) (#EQ? "local_variable_declaration")
    (named_node
        (identifier) (#EQ? "variable_declarator")
    ) @_rm.final
)
(named_node
    (identifier) (#EQ? "local_variable_declaration")
    (named_node
        (identifier) (#EQ? "type_identifier") .
    ) @_rm.final
)
(named_node
    (identifier) (#EQ? "local_variable_declaration")
    (predicate
      (identifier) (#EQ? "EQ")
      (parameters
          (string)
      )
    ) @_rm.final
)

(named_node
  (named_node
      (identifier) (#EQ? "catch_formal_parameter")
  ) @rm.final
)
