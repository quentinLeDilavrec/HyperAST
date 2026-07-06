(with_statement
  (with_clause
    (with_item
      (as_pattern
        (call
          (attribute
            (identifier) (#EQ? "assertRaises") .
          )
        )
      )
    )
  )
)


(try_statement
  (except_clause)
)


(try_statement
  (block
    (expression_statement
      (call
        (attribute
          (identifier) (#EQ? "fail") .
        )
      )
    )
  )
  (except_clause
    (as_pattern)
  )
)


(try_statement
  (block
    (expression_statement
      (call
        (attribute
          (identifier) (#EQ? "fail") .
        )
      )
    )
  )
  (except_clause
    (as_pattern)
  )
)


(try_statement
  (block
    (expression_statement
      (call
        (identifier); (#EQ? "fail")
      )
    )
  )
  (except_clause)
)


(try_statement
  (block
    (expression_statement
      (call
        (identifier) (#EQ? "fail")
      )
    ) .
  )
  (except_clause)
)


(try_statement
  (block (_)+ .
    (expression_statement
      (call
        (identifier) (#EQ? "fail")
      )
    ) .
  )
  (except_clause)
)


(try_statement
  (block (_)+ . (_) .
    (expression_statement
      (call
        (identifier) (#EQ? "fail")
      )
    ) .
  )
 (except_clause)
)


(try_statement
  (block . (_) .
    (expression_statement
      (call
        (identifier) (#EQ? "fail")
      )
    ) .
  )
  (except_clause)
)
