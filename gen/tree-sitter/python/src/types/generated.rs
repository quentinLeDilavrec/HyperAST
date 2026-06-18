// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_python --example regen_python_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Identifier = "identifier";
    SemiColon = ";";
    Import = "import";
    Dot = ".";
    From = "from";
    TS0 = "__future__";
    LParen = "(";
    RParen = ")";
    Comma = ",";
    As = "as";
    Star = "*";
    Print = "print";
    GtGt = ">>";
    Assert = "assert";
    TS1 = ":=";
    Return
    = "return";
    Del = "del";
    Raise = "raise";
    Pass = "pass";
    Break = "break";
    Continue = "continue";
    If = "if";
    Colon = ":";
    Elif = "elif";
    Else = "else";
    Match = "match";
    Case = "case";
    Async = "async";
    For = "for";
    In = "in";
    While = "while";
    Try = "try";
    Except = "except";
    TS2 = "except*";
    Finally = "finally";
    With = "with";
    Def = "def";
    DashGt = "->";
    StarStar = "**";
    Global = "global";
    Nonlocal = "nonlocal";
    Exec = "exec";
    Type = "type";
    Eq = "=";
    Class = "class";
    LBracket = "[";
    RBracket = "]";
    At
    = "@";
    Dash = "-";
    Inderscore = "_";
    Pipe = "|";
    LBrace = "{";
    RBrace = "}";
    Plus = "+";
    Not = "not";
    And = "and";
    Or = "or";
    Slash = "/";
    Percent = "%";
    TS3 = "//";
    Amp
    = "&";
    Caret = "^";
    LtLt = "<<";
    Tilde = "~";
    Is = "is";
    LT = "<";
    LTEq = "<=";
    EqEq
    = "==";
    BangEq = "!=";
    GTEq = ">=";
    GT = ">";
    TS4 = "<>";
    Lambda = "lambda";
    PlusEq = "+=";
    DashEq = "-=";
    StarEq = "*=";
    SlashEq = "/=";
    TS5 = "@=";
    TS6 = "//=";
    PercentEq = "%=";
    StarStarEq = "**=";
    GtGtEq = ">>=";
    LtLtEq = "<<=";
    AmpEq = "&=";
    CaretEq = "^=";
    PipeEq = "|=";
    Yield = "yield";
    Ellipsis = "ellipsis";
    EscapeSequence
    = "escape_sequence";
    TS7 = "\\";
    FormatSpecifierToken1 = "format_specifier_token1";
    TypeConversion = "type_conversion";
    Integer = "integer";
    Float = "float";
    Await = "await";
    True = "true";
    False = "false";
    None = "none";
    Comment = "comment";
    LineContinuation = "line_continuation";
    _Newline = "_newline";
    _Indent = "_indent";
    _Dedent = "_dedent";
    StringStart = "string_start";
    _StringContent = "_string_content";
    EscapeInterpolation = "escape_interpolation";
    StringEnd = "string_end";
    Module = "module";
    _Statement = "_statement";
    _SimpleStatements = "_simple_statements";
    ImportStatement = "import_statement";
    ImportPrefix = "import_prefix";
    RelativeImport = "relative_import";
    FutureImportStatement = "future_import_statement";
    ImportFromStatement = "import_from_statement";
    _ImportList
    = "_import_list";
    AliasedImport = "aliased_import";
    WildcardImport = "wildcard_import";
    PrintStatement = "print_statement";
    Chevron = "chevron";
    AssertStatement = "assert_statement";
    ExpressionStatement = "expression_statement";
    NamedExpression = "named_expression";
    _NamedExpressionLhs = "_named_expression_lhs";
    ReturnStatement = "return_statement";
    DeleteStatement = "delete_statement";
    RaiseStatement = "raise_statement";
    PassStatement = "pass_statement";
    BreakStatement
    = "break_statement";
    ContinueStatement = "continue_statement";
    IfStatement = "if_statement";
    ElifClause = "elif_clause";
    ElseClause = "else_clause";
    MatchStatement = "match_statement";
    Block = "block";
    CaseClause = "case_clause";
    ForStatement = "for_statement";
    WhileStatement = "while_statement";
    TryStatement = "try_statement";
    ExceptClause = "except_clause";
    ExceptGroupClause = "except_group_clause";
    FinallyClause = "finally_clause";
    WithStatement = "with_statement";
    WithClause = "with_clause";
    WithItem = "with_item";
    FunctionDefinition = "function_definition";
    Parameters = "parameters";
    LambdaParameters = "lambda_parameters";
    ListSplat = "list_splat";
    DictionarySplat = "dictionary_splat";
    GlobalStatement = "global_statement";
    NonlocalStatement = "nonlocal_statement";
    ExecStatement = "exec_statement";
    TypeAliasStatement = "type_alias_statement";
    ClassDefinition = "class_definition";
    TypeParameter = "type_parameter";
    ParenthesizedListSplat = "parenthesized_list_splat";
    ArgumentList = "argument_list";
    DecoratedDefinition = "decorated_definition";
    Decorator = "decorator";
    Block_ = "block";
    ExpressionList = "expression_list";
    DottedName = "dotted_name";
    CasePattern = "case_pattern";
    _SimplePattern = "_simple_pattern";
    AsPattern = "as_pattern";
    UnionPattern = "union_pattern";
    ListPattern = "list_pattern";
    TuplePattern = "tuple_pattern";
    DictPattern = "dict_pattern";
    _KeyValuePattern = "_key_value_pattern";
    KeywordPattern = "keyword_pattern";
    SplatPattern = "splat_pattern";
    ClassPattern = "class_pattern";
    ComplexPattern = "complex_pattern";
    _Parameters = "_parameters";
    _Patterns = "_patterns";
    Parameter = "parameter";
    Pattern = "pattern";
    TuplePattern_ = "tuple_pattern";
    ListPattern_ = "list_pattern";
    DefaultParameter = "default_parameter";
    TypedDefaultParameter = "typed_default_parameter";
    ListSplatPattern = "list_splat_pattern";
    DictionarySplatPattern = "dictionary_splat_pattern";
    AsPattern_ = "as_pattern";
    _ExpressionWithinForInClause = "_expression_within_for_in_clause";
    Expression = "expression";
    PrimaryExpression = "primary_expression";
    NotOperator = "not_operator";
    BooleanOperator = "boolean_operator";
    BinaryOperator = "binary_operator";
    UnaryOperator = "unary_operator";
    TS8 = "not in";
    TS9 = "is not";
    ComparisonOperator
    = "comparison_operator";
    Lambda_ = "lambda";
    Lambda__ = "lambda";
    Assignment = "assignment";
    AugmentedAssignment = "augmented_assignment";
    PatternList = "pattern_list";
    _RightHandSide = "_right_hand_side";
    Yield_ = "yield";
    Attribute = "attribute";
    Subscript = "subscript";
    Slice = "slice";
    Call = "call";
    TypedParameter
    = "typed_parameter";
    Type_ = "type";
    SplatType = "splat_type";
    GenericType = "generic_type";
    UnionType = "union_type";
    ConstrainedType = "constrained_type";
    MemberType = "member_type";
    KeywordArgument = "keyword_argument";
    List = "list";
    Set
    = "set";
    Tuple = "tuple";
    Dictionary = "dictionary";
    Pair = "pair";
    ListComprehension
    = "list_comprehension";
    DictionaryComprehension = "dictionary_comprehension";
    SetComprehension = "set_comprehension";
    GeneratorExpression = "generator_expression";
    _ComprehensionClauses = "_comprehension_clauses";
    ParenthesizedExpression = "parenthesized_expression";
    _CollectionElements = "_collection_elements";
    ForInClause
    = "for_in_clause";
    IfClause = "if_clause";
    ConditionalExpression = "conditional_expression";
    ConcatenatedString = "concatenated_string";
    String = "string";
    StringContent = "string_content";
    Interpolation = "interpolation";
    _FExpression = "_f_expression";
    _NotEscapeSequence = "_not_escape_sequence";
    FormatSpecifier = "format_specifier";
    Await_ = "await";
    PositionalSeparator = "positional_separator";
    KeywordSeparator = "keyword_separator";
    ModuleRepeat1 = "module_repeat1";
    _SimpleStatementsRepeat1 = "_simple_statements_repeat1";
    ImportPrefixRepeat1 = "import_prefix_repeat1";
    _ImportListRepeat1 = "_import_list_repeat1";
    PrintStatementRepeat1 = "print_statement_repeat1";
    AssertStatementRepeat1 = "assert_statement_repeat1";
    IfStatementRepeat1 = "if_statement_repeat1";
    MatchStatementRepeat1 = "match_statement_repeat1";
    _MatchBlockRepeat1 = "_match_block_repeat1";
    CaseClauseRepeat1 = "case_clause_repeat1";
    TryStatementRepeat1 = "try_statement_repeat1";
    TryStatementRepeat2 = "try_statement_repeat2";
    WithClauseRepeat1 = "with_clause_repeat1";
    GlobalStatementRepeat1 = "global_statement_repeat1";
    TypeParameterRepeat1 = "type_parameter_repeat1";
    ArgumentListRepeat1 = "argument_list_repeat1";
    DecoratedDefinitionRepeat1 = "decorated_definition_repeat1";
    DottedNameRepeat1 = "dotted_name_repeat1";
    UnionPatternRepeat1 = "union_pattern_repeat1";
    DictPatternRepeat1 = "dict_pattern_repeat1";
    _ParametersRepeat1 = "_parameters_repeat1";
    _PatternsRepeat1 = "_patterns_repeat1";
    ComparisonOperatorRepeat1 = "comparison_operator_repeat1";
    SubscriptRepeat1 = "subscript_repeat1";
    DictionaryRepeat1 = "dictionary_repeat1";
    _ComprehensionClausesRepeat1 = "_comprehension_clauses_repeat1";
    _CollectionElementsRepeat1 = "_collection_elements_repeat1";
    ForInClauseRepeat1 = "for_in_clause_repeat1";
    ConcatenatedStringRepeat1 = "concatenated_string_repeat1";
    StringRepeat1 = "string_repeat1";
    StringContentRepeat1 = "string_content_repeat1";
    FormatSpecifierRepeat1 = "format_specifier_repeat1";
    AsPatternTarget = "as_pattern_target";
    FormatExpression = "format_expression";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self,
            End,
            FormatSpecifierToken1,
            _Newline,
            _Indent,
            _Dedent,
            _StringContent,
            _Statement,
            _SimpleStatements,
            _ImportList,
            _NamedExpressionLhs,
            _SimplePattern,
            _KeyValuePattern,
            _Parameters,
            _Patterns,
            Parameter,
            Pattern,
            _ExpressionWithinForInClause,
            Expression,
            PrimaryExpression,
            _RightHandSide,
            _ComprehensionClauses,
            _CollectionElements,
            _FExpression,
            _NotEscapeSequence,
            ModuleRepeat1,
            _SimpleStatementsRepeat1,
            ImportPrefixRepeat1,
            _ImportListRepeat1,
            PrintStatementRepeat1,
            AssertStatementRepeat1,
            IfStatementRepeat1,
            MatchStatementRepeat1,
            _MatchBlockRepeat1,
            CaseClauseRepeat1,
            TryStatementRepeat1,
            TryStatementRepeat2,
            WithClauseRepeat1,
            GlobalStatementRepeat1,
            TypeParameterRepeat1,
            ArgumentListRepeat1,
            DecoratedDefinitionRepeat1,
            DottedNameRepeat1,
            UnionPatternRepeat1,
            DictPatternRepeat1,
            _ParametersRepeat1,
            _PatternsRepeat1,
            ComparisonOperatorRepeat1,
            SubscriptRepeat1,
            DictionaryRepeat1,
            _ComprehensionClausesRepeat1,
            _CollectionElementsRepeat1,
            ForInClauseRepeat1,
            ConcatenatedStringRepeat1,
            StringRepeat1,
            StringContentRepeat1,
            FormatSpecifierRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(self, Parameter, Pattern, Expression, PrimaryExpression,)
    }
    pub fn is_named(&self) -> bool {
        is!(
            self,
            Identifier,
            Ellipsis,
            EscapeSequence,
            TypeConversion,
            Integer,
            Float,
            True,
            False,
            None,
            Comment,
            LineContinuation,
            StringStart,
            EscapeInterpolation,
            StringEnd,
            Module,
            ImportStatement,
            ImportPrefix,
            RelativeImport,
            FutureImportStatement,
            ImportFromStatement,
            AliasedImport,
            WildcardImport,
            PrintStatement,
            Chevron,
            AssertStatement,
            ExpressionStatement,
            NamedExpression,
            ReturnStatement,
            DeleteStatement,
            RaiseStatement,
            PassStatement,
            BreakStatement,
            ContinueStatement,
            IfStatement,
            ElifClause,
            ElseClause,
            MatchStatement,
            Block,
            CaseClause,
            ForStatement,
            WhileStatement,
            TryStatement,
            ExceptClause,
            ExceptGroupClause,
            FinallyClause,
            WithStatement,
            WithClause,
            WithItem,
            FunctionDefinition,
            Parameters,
            LambdaParameters,
            ListSplat,
            DictionarySplat,
            GlobalStatement,
            NonlocalStatement,
            ExecStatement,
            TypeAliasStatement,
            ClassDefinition,
            TypeParameter,
            ParenthesizedListSplat,
            ArgumentList,
            DecoratedDefinition,
            Decorator,
            Block_,
            ExpressionList,
            DottedName,
            CasePattern,
            AsPattern,
            UnionPattern,
            ListPattern,
            TuplePattern,
            DictPattern,
            KeywordPattern,
            SplatPattern,
            ClassPattern,
            ComplexPattern,
            Parameter,
            Pattern,
            TuplePattern_,
            ListPattern_,
            DefaultParameter,
            TypedDefaultParameter,
            ListSplatPattern,
            DictionarySplatPattern,
            AsPattern_,
            Expression,
            PrimaryExpression,
            NotOperator,
            BooleanOperator,
            BinaryOperator,
            UnaryOperator,
            ComparisonOperator,
            Assignment,
            AugmentedAssignment,
            PatternList,
            Attribute,
            Subscript,
            Slice,
            Call,
            TypedParameter,
            SplatType,
            GenericType,
            UnionType,
            ConstrainedType,
            MemberType,
            KeywordArgument,
            List,
            Set,
            Tuple,
            Dictionary,
            Pair,
            ListComprehension,
            DictionaryComprehension,
            SetComprehension,
            GeneratorExpression,
            ParenthesizedExpression,
            ForInClause,
            IfClause,
            ConditionalExpression,
            ConcatenatedString,
            String,
            StringContent,
            Interpolation,
            FormatSpecifier,
            PositionalSeparator,
            KeywordSeparator,
            AsPatternTarget,
            FormatExpression,
        )
    }
}
