(try_statement)

(method_invocation
  (identifier) (#EQ? "fail")
)

(try_statement
  (block
    (expression_statement
      (method_invocation
        (identifier) (#EQ? "fail")
      )
    )
  )
  (catch_clause)
)
