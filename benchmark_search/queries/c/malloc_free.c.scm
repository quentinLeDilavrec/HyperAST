(compound_statement
  (declaration
    (init_declarator
      (call_expression
        (identifier) (#EQ? "malloc")
      )
    ) @malloc
  )
  (statement
    (call_expression
      (identifier) (#EQ? "free")
    ) @free
  )
)

(compound_statement
  (declaration
    (init_declarator
      (pointer_declarator
        (identifier) @p1
      )
      (call_expression
        (identifier) (#EQ? "malloc")
      )
    ) @malloc
  )
  (statement
    (call_expression
      (identifier) (#EQ? "free")
      (argument_list
        (identifier) @p2
      )
    ) @free
  )
  (#eq? @p1 @p2)
)
