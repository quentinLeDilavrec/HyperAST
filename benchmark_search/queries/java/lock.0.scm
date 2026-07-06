(try_statement
  (finally_clause
    (block
      (expression_statement
        (method_invocation
          (identifier) @id (#eq? @id "unlock")
        )
      )
    )
  )
) @root


(try_statement
  (finally_clause
    (block
      (expression_statement
        (method_invocation
          (identifier) @id (#eq? @id "unlock")
        )
      ) .
    )
  )
) @root
