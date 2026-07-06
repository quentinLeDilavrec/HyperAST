(let_declaration
  (if_expression
    (else_clause
      (block
        (expression_statement
          (return_expression)
        )
      ) @old_return
    )
  )
)


(let_declaration
  (if_expression
    (else_clause
      (block
        (expression_statement
          [
            (return_expression)
            (continue_expression)
            (break_expression)
          ]
        )
      ) @old_early_return
    )
  )
)


(let_declaration
  (block
    [
      (expression_statement
        [
          (return_expression)
          (continue_expression)
          (break_expression)
        ]
      )
      (return_expression)
      (continue_expression)
      (break_expression)
    ]
  ) @new_early_return_stmt_and_expr
)

(let_declaration
  (block
    (expression_statement
      (return_expression)
    )
  ) @let_return_stmt
)


(let_declaration
  (block
    (break_expression
    )
  ) @let_break_expr
)


(let_declaration
  (block
    (continue_expression)
  ) @let_continue_expr
)
