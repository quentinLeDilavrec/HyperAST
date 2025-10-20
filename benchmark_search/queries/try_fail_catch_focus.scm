(try_statement
  (block
    (expression_statement
      (method_invocation
        (identifier) (#EQ? "fail")
      )
    )
  )
  (catch_clause)
) @root


(try_statement
  (block
    (expression_statement
      (method_invocation
        (identifier) (#EQ? "fail")
      )
    ) .
  )
(catch_clause)
) @root


(try_statement
  (block (_) .
    (expression_statement
      (method_invocation
        (identifier) (#EQ? "fail")
      )
    ) .
  )
(catch_clause)
) @root


(try_statement
  (block (_) . (_) .
    (expression_statement
      (method_invocation
        (identifier) (#EQ? "fail")
      )
    ) .
  )
(catch_clause)
) @root


(try_statement
  (block . (_) .
    (expression_statement
      (method_invocation
        (identifier) (#EQ? "fail")
      )
    ) .
  )
(catch_clause)
) @root
