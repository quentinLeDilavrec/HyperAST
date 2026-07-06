(_
  (lexical_declaration
    (variable_declarator
      (identifier) @p1 (#eq? @p1 "after")
      (arrow_function)
    )
  ) @after
  (expression_statement
    (call_expression
      (identifier) @task (#eq? @task "task")
      (arguments
        (arrow_function) @before
        (identifier) @p2 (#eq? @p2 "after")
      )
    )
  )
)
