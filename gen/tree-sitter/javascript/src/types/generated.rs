// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_javascript --example regen_javascript_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Identifier = "identifier";
    HashBangLine = "hash_bang_line";
    Export = "export";
    Star = "*";
    Default = "default";
    As = "as";
    LBrace = "{";
    Comma = ",";
    RBrace = "}";
    Import = "import";
    From = "from";
    With = "with";
    Var = "var";
    Let = "let";
    Const = "const";
    Else = "else";
    If = "if";
    Switch = "switch";
    For = "for";
    LParen = "(";
    SemiColon = ";";
    RParen = ")";
    Await = "await";
    In = "in";
    Of = "of";
    While = "while";
    Do = "do";
    Try = "try";
    Break = "break";
    Continue = "continue";
    Debugger = "debugger";
    Return = "return";
    Throw = "throw";
    Colon = ":";
    Case = "case";
    Catch = "catch";
    Finally = "finally";
    Yield = "yield";
    Eq = "=";
    LBracket = "[";
    RBracket = "]";
    HtmlCharacterReference = "html_character_reference";
    LT = "<";
    GT = ">";
    Identifier_ = "identifier";
    Dot = ".";
    TS0 = "</";
    TS1 = "/>";
    DQuote = "\"";
    SQuote = "'";
    StringFragment = "string_fragment";
    StringFragment_ = "string_fragment";
    Class = "class";
    Extends = "extends";
    Async = "async";
    Function = "function";
    BigArrow = "=>";
    OptionalChain = "optional_chain";
    New = "new";
    PlusEq = "+=";
    DashEq = "-=";
    StarEq = "*=";
    SlashEq = "/=";
    PercentEq = "%=";
    CaretEq = "^=";
    AmpEq = "&=";
    PipeEq = "|=";
    GtGtEq = ">>=";
    GtGtGtEq = ">>>=";
    LtLtEq = "<<=";
    StarStarEq = "**=";
    AmpAmpEq = "&&=";
    PipePipeEq = "||=";
    QMarkQMarkEq = "??=";
    DotDotDot = "...";
    AmpAmp = "&&";
    PipePipe = "||";
    GtGt = ">>";
    GtGtGt = ">>>";
    LtLt
    = "<<";
    Amp = "&";
    Caret = "^";
    Pipe = "|";
    Plus = "+";
    Dash = "-";
    Slash = "/";
    Percent = "%";
    StarStar = "**";
    LTEq = "<=";
    EqEq = "==";
    EqEqEq = "===";
    BangEq = "!=";
    BangEqEq = "!==";
    GTEq = ">=";
    QMarkQMark = "??";
    Instanceof = "instanceof";
    Bang = "!";
    Tilde = "~";
    Typeof = "typeof";
    Void = "void";
    Delete = "delete";
    PlusPlus = "++";
    DashDash = "--";
    StringFragment__ = "string_fragment";
    StringFragment___ = "string_fragment";
    EscapeSequence = "escape_sequence";
    Comment = "comment";
    BQuote = "`";
    DollarLBrace = "${";
    Slash_ = "/";
    RegexPattern = "regex_pattern";
    RegexFlags = "regex_flags";
    Number = "number";
    PrivatePropertyIdentifier = "private_property_identifier";
    Target = "target";
    Meta = "meta";
    This = "this";
    Super = "super";
    TS2 = "true";
    TS3 = "false";
    Null = "null";
    Undefined = "undefined";
    At = "@";
    Static = "static";
    TS4 = "static get";
    Get = "get";
    Set = "set";
    _AutomaticSemicolon = "_automatic_semicolon";
    StringFragment____
    = "string_fragment";
    QMark = "?";
    HtmlComment = "html_comment";
    JsxText = "jsx_text";
    Program = "program";
    ExportStatement = "export_statement";
    NamespaceExport = "namespace_export";
    ExportClause = "export_clause";
    ExportSpecifier = "export_specifier";
    _ModuleExportName = "_module_export_name";
    Declaration = "declaration";
    Import_ = "import";
    ImportStatement = "import_statement";
    ImportClause
    = "import_clause";
    _FromClause = "_from_clause";
    NamespaceImport = "namespace_import";
    NamedImports = "named_imports";
    ImportSpecifier = "import_specifier";
    ImportAttribute = "import_attribute";
    Statement = "statement";
    ExpressionStatement = "expression_statement";
    VariableDeclaration = "variable_declaration";
    LexicalDeclaration = "lexical_declaration";
    VariableDeclarator = "variable_declarator";
    StatementBlock = "statement_block";
    ElseClause = "else_clause";
    IfStatement = "if_statement";
    SwitchStatement = "switch_statement";
    ForStatement = "for_statement";
    ForInStatement = "for_in_statement";
    _ForHeader = "_for_header";
    WhileStatement = "while_statement";
    DoStatement = "do_statement";
    TryStatement = "try_statement";
    WithStatement = "with_statement";
    BreakStatement = "break_statement";
    ContinueStatement = "continue_statement";
    DebuggerStatement = "debugger_statement";
    ReturnStatement = "return_statement";
    ThrowStatement = "throw_statement";
    EmptyStatement = "empty_statement";
    LabeledStatement = "labeled_statement";
    SwitchBody = "switch_body";
    SwitchCase = "switch_case";
    SwitchDefault = "switch_default";
    CatchClause = "catch_clause";
    FinallyClause = "finally_clause";
    ParenthesizedExpression = "parenthesized_expression";
    Expression = "expression";
    PrimaryExpression = "primary_expression";
    YieldExpression = "yield_expression";
    Object = "object";
    ObjectPattern = "object_pattern";
    AssignmentPattern = "assignment_pattern";
    ObjectAssignmentPattern = "object_assignment_pattern";
    Array = "array";
    ArrayPattern = "array_pattern";
    JsxElement = "jsx_element";
    JsxExpression = "jsx_expression";
    JsxOpeningElement = "jsx_opening_element";
    MemberExpression = "member_expression";
    JsxNamespaceName = "jsx_namespace_name";
    JsxClosingElement = "jsx_closing_element";
    JsxSelfClosingElement = "jsx_self_closing_element";
    JsxAttribute = "jsx_attribute";
    String = "string";
    Class_ = "class";
    ClassDeclaration
    = "class_declaration";
    ClassHeritage = "class_heritage";
    FunctionExpression = "function_expression";
    FunctionDeclaration = "function_declaration";
    GeneratorFunction = "generator_function";
    GeneratorFunctionDeclaration = "generator_function_declaration";
    ArrowFunction = "arrow_function";
    CallExpression = "call_expression";
    NewExpression = "new_expression";
    AwaitExpression = "await_expression";
    MemberExpression_ = "member_expression";
    SubscriptExpression = "subscript_expression";
    AssignmentExpression = "assignment_expression";
    _AugmentedAssignmentLhs = "_augmented_assignment_lhs";
    AugmentedAssignmentExpression
    = "augmented_assignment_expression";
    _Initializer = "_initializer";
    _DestructuringPattern = "_destructuring_pattern";
    SpreadElement = "spread_element";
    TernaryExpression = "ternary_expression";
    BinaryExpression = "binary_expression";
    UnaryExpression = "unary_expression";
    UpdateExpression = "update_expression";
    SequenceExpression = "sequence_expression";
    String_ = "string";
    TemplateString = "template_string";
    TemplateSubstitution = "template_substitution";
    Regex = "regex";
    MetaProperty = "meta_property";
    Arguments = "arguments";
    Decorator = "decorator";
    MemberExpression__ = "member_expression";
    CallExpression_ = "call_expression";
    ClassBody = "class_body";
    FieldDefinition = "field_definition";
    FormalParameters = "formal_parameters";
    ClassStaticBlock = "class_static_block";
    Pattern = "pattern";
    RestPattern = "rest_pattern";
    MethodDefinition = "method_definition";
    Pair = "pair";
    PairPattern = "pair_pattern";
    _PropertyName = "_property_name";
    ComputedPropertyName
    = "computed_property_name";
    ProgramRepeat1 = "program_repeat1";
    ExportStatementRepeat1 = "export_statement_repeat1";
    ExportClauseRepeat1 = "export_clause_repeat1";
    NamedImportsRepeat1 = "named_imports_repeat1";
    VariableDeclarationRepeat1 = "variable_declaration_repeat1";
    SwitchBodyRepeat1 = "switch_body_repeat1";
    ObjectRepeat1 = "object_repeat1";
    ObjectPatternRepeat1 = "object_pattern_repeat1";
    ArrayRepeat1 = "array_repeat1";
    ArrayPatternRepeat1 = "array_pattern_repeat1";
    JsxElementRepeat1 = "jsx_element_repeat1";
    JsxOpeningElementRepeat1 = "jsx_opening_element_repeat1";
    _JsxStringRepeat1 = "_jsx_string_repeat1";
    _JsxStringRepeat2 = "_jsx_string_repeat2";
    SequenceExpressionRepeat1 = "sequence_expression_repeat1";
    StringRepeat1 = "string_repeat1";
    StringRepeat2 = "string_repeat2";
    TemplateStringRepeat1 = "template_string_repeat1";
    ClassBodyRepeat1 = "class_body_repeat1";
    FormalParametersRepeat1 = "formal_parameters_repeat1";
    PropertyIdentifier = "property_identifier";
    ShorthandPropertyIdentifier = "shorthand_property_identifier";
    ShorthandPropertyIdentifierPattern = "shorthand_property_identifier_pattern";
    StatementIdentifier = "statement_identifier";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self,
            End,
            _AutomaticSemicolon,
            _ModuleExportName,
            Declaration,
            _FromClause,
            Statement,
            _ForHeader,
            Expression,
            PrimaryExpression,
            _AugmentedAssignmentLhs,
            _Initializer,
            _DestructuringPattern,
            Pattern,
            _PropertyName,
            ProgramRepeat1,
            ExportStatementRepeat1,
            ExportClauseRepeat1,
            NamedImportsRepeat1,
            VariableDeclarationRepeat1,
            SwitchBodyRepeat1,
            ObjectRepeat1,
            ObjectPatternRepeat1,
            ArrayRepeat1,
            ArrayPatternRepeat1,
            JsxElementRepeat1,
            JsxOpeningElementRepeat1,
            _JsxStringRepeat1,
            _JsxStringRepeat2,
            SequenceExpressionRepeat1,
            StringRepeat1,
            StringRepeat2,
            TemplateStringRepeat1,
            ClassBodyRepeat1,
            FormalParametersRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(
            self,
            Declaration,
            Statement,
            Expression,
            PrimaryExpression,
            Pattern,
        )
    }
    pub fn is_named(&self) -> bool {
        is!(
            self,
            Identifier,
            HashBangLine,
            HtmlCharacterReference,
            Identifier_,
            StringFragment,
            StringFragment_,
            OptionalChain,
            StringFragment__,
            StringFragment___,
            EscapeSequence,
            Comment,
            RegexPattern,
            RegexFlags,
            Number,
            PrivatePropertyIdentifier,
            This,
            Super,
            TS2,
            TS3,
            Null,
            Undefined,
            StringFragment____,
            HtmlComment,
            JsxText,
            Program,
            ExportStatement,
            NamespaceExport,
            ExportClause,
            ExportSpecifier,
            Declaration,
            ImportStatement,
            ImportClause,
            NamespaceImport,
            NamedImports,
            ImportSpecifier,
            ImportAttribute,
            Statement,
            ExpressionStatement,
            VariableDeclaration,
            LexicalDeclaration,
            VariableDeclarator,
            StatementBlock,
            ElseClause,
            IfStatement,
            SwitchStatement,
            ForStatement,
            ForInStatement,
            WhileStatement,
            DoStatement,
            TryStatement,
            WithStatement,
            BreakStatement,
            ContinueStatement,
            DebuggerStatement,
            ReturnStatement,
            ThrowStatement,
            EmptyStatement,
            LabeledStatement,
            SwitchBody,
            SwitchCase,
            SwitchDefault,
            CatchClause,
            FinallyClause,
            ParenthesizedExpression,
            Expression,
            PrimaryExpression,
            YieldExpression,
            Object,
            ObjectPattern,
            AssignmentPattern,
            ObjectAssignmentPattern,
            Array,
            ArrayPattern,
            JsxElement,
            JsxExpression,
            JsxOpeningElement,
            MemberExpression,
            JsxNamespaceName,
            JsxClosingElement,
            JsxSelfClosingElement,
            JsxAttribute,
            String,
            ClassDeclaration,
            ClassHeritage,
            FunctionExpression,
            FunctionDeclaration,
            GeneratorFunction,
            GeneratorFunctionDeclaration,
            ArrowFunction,
            CallExpression,
            NewExpression,
            AwaitExpression,
            MemberExpression_,
            SubscriptExpression,
            AssignmentExpression,
            AugmentedAssignmentExpression,
            SpreadElement,
            TernaryExpression,
            BinaryExpression,
            UnaryExpression,
            UpdateExpression,
            SequenceExpression,
            String_,
            TemplateString,
            TemplateSubstitution,
            Regex,
            MetaProperty,
            Arguments,
            Decorator,
            MemberExpression__,
            CallExpression_,
            ClassBody,
            FieldDefinition,
            FormalParameters,
            ClassStaticBlock,
            Pattern,
            RestPattern,
            MethodDefinition,
            Pair,
            PairPattern,
            ComputedPropertyName,
            PropertyIdentifier,
            ShorthandPropertyIdentifier,
            ShorthandPropertyIdentifierPattern,
            StatementIdentifier,
        )
    }
}
