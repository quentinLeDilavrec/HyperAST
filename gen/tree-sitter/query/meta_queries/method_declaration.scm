(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred @rm.all
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "block") .
    ) @rm.all.full
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "block")
        (named_node
            (identifier) (#EQ? "local_variable_declaration")
        ) @rm.all.full
    )
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "formal_parameters")
        (named_node
            (identifier) (#EQ? "formal_parameter")
            (named_node
                (identifier) (#EQ? "identifier") .
            ) .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @pred @rm.all
        )
    )
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "formal_parameters")
        (named_node
            (identifier) (#EQ? "formal_parameter")
            (named_node
                (identifier) (#EQ? "type_identifier") .
            ) .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @pred @rm.all
        )
    )
)

(named_node
  (named_node
      (identifier) (#EQ? "method_declaration")
  ) @rm.final
)

(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "block")
        (named_node) @rm.final
    )
)
(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) @rm.final
)

(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) @rm.all.full
)

(named_node
    (identifier) (#EQ? "method_declaration")
    (named_node
      (identifier) (#EQ? "throws")
      (named_node
          (identifier) (#EQ? "type_identifier") .
      ) .
      (predicate
          (identifier) (#EQ? "EQ")
          (parameters
              (string)
          )
      ) @rm.all.full
    )
)

(named_node
    (identifier) (#EQ? "annotation_argument_list")
    (named_node) @rm.all.full
)
