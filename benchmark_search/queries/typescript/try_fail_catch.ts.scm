(try_statement
  (statement_block
    (expression_statement
      (call_expression
        (identifier) (#EQ? "assert")
        (arguments
          (false)
        )
      )
    )
  )
  (catch_clause)
)


(try_statement
  (statement_block
    (expression_statement
      (call_expression
        (identifier) (#EQ? "assert")
        (arguments
          (false)
        )
      )
    ) .
  )
  (catch_clause)
)


(try_statement
  (statement_block
    (_)+ .
    (expression_statement
      (call_expression
        (identifier) (#EQ? "assert")
        (arguments
          (false)
        )
      )
    ) .
  )
  (catch_clause)
)


(try_statement
  (statement_block
    (_)+ . (_) .
    (expression_statement
      (call_expression
        (identifier) (#EQ? "assert")
        (arguments
          (false)
        )
      )
    ) .
  )
  (catch_clause)
)


(try_statement
  (statement_block
    . (_) .
    (expression_statement
      (call_expression
        (identifier) (#EQ? "assert")
        (arguments
          (false)
        )
      )
    ) .
  )
  (catch_clause)
)
