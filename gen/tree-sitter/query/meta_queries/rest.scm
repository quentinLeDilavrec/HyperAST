[
  (named_node
    (identifier) (#EQ? "expression_statement")
  )
  (named_node
    (identifier) (#EQ? "return_statement")
  )
  (named_node
    (identifier) (#EQ? "method_invocation")
  )
  (named_node
    (identifier) (#EQ? "method_declaration")
  )
] @need
[
  (named_node
    (identifier) (#EQ? "expression_statement")
    (named_node
      (identifier) (#EQ? "method_invocation")
    )
  )
  (named_node
    (identifier) (#EQ? "expression_statement")
    (named_node
      (identifier) (#EQ? "method_invocation")
    )
  )
] @_need

[
  (named_node
      (identifier) (#EQ? "expression_statement")
      (named_node
        (identifier) (#EQ? "method_invocation")
        (named_node
          (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string) (#MATCH? "\"(assert|check)")
            )
        ) .
        (named_node
          (identifier) (#EQ? "argument_list")
        ) ? .
      )
  )
  (named_node
      (identifier) (#EQ? "catch_clause")
  )
] @_uniq

(named_node .
    (identifier) .
    "/" @rm.all.full .
    (identifier) @rm.all.full
)
