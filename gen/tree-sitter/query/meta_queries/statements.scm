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
