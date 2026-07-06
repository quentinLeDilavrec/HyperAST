(try_statement
  (except_clause)
)

(try_statement
  (except_clause
    (as_pattern)
  )
)

(call
  (identifier) (#EQ? "fail")
)

(call
  (attribute
    (identifier) (#EQ? "fail")
  )
)

(call
  (attribute
    (identifier) (#EQ? "assertRaises")
  )
)

(assert_statement
  "assert"
  (false)
)
