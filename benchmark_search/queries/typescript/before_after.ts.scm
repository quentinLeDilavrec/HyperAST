(_
  (lexical_declaration
    (variable_declarator
      (identifier) (#EQ? "after")
      (arrow_function)
    )
  ) @after
  (expression_statement
    (call_expression
      (identifier) (#EQ? "task")
      (arguments
        (arrow_function) @before
        (identifier) (#EQ? "after")
      )
    )
  )
)
