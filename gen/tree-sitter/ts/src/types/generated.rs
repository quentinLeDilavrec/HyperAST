// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_ts --example regen_ts_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Identifier = "identifier";
    HashBangLine = "hash_bang_line";
    Export = "export";
    Star = "*";
    Default = "default";
    Type = "type";
    Eq = "=";
    As = "as";
    Namespace = "namespace";
    LBrace = "{";
    Comma = ",";
    RBrace = "}";
    Typeof = "typeof";
    Import = "import";
    From = "from";
    With = "with";
    Assert = "assert";
    Var = "var";
    Let
    = "let";
    Const = "const";
    Bang = "!";
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
    LBracket = "[";
    RBracket = "]";
    Dot = ".";
    Class = "class";
    Async = "async";
    Function = "function";
    BigArrow = "=>";
    QMarkDot = "?.";
    New = "new";
    Using = "using";
    PlusEq = "+=";
    DashEq
    = "-=";
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
    LtLt = "<<";
    Amp = "&";
    Caret = "^";
    Pipe = "|";
    Plus = "+";
    Dash = "-";
    Slash = "/";
    Percent = "%";
    StarStar
    = "**";
    LT = "<";
    LTEq = "<=";
    EqEq = "==";
    EqEqEq = "===";
    BangEq = "!=";
    BangEqEq = "!==";
    GTEq = ">=";
    GT = ">";
    QMarkQMark = "??";
    Instanceof = "instanceof";
    Tilde = "~";
    Void = "void";
    Delete = "delete";
    PlusPlus = "++";
    DashDash = "--";
    DQuote = "\"";
    SQuote = "'";
    StringFragment = "string_fragment";
    StringFragment_ = "string_fragment";
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
    Super
    = "super";
    True = "true";
    False = "false";
    Null = "null";
    Undefined = "undefined";
    At
    = "@";
    Static = "static";
    Readonly = "readonly";
    Get = "get";
    Set = "set";
    QMark = "?";
    Declare = "declare";
    Public = "public";
    Private = "private";
    Protected = "protected";
    Override = "override";
    Module = "module";
    Any = "any";
    Number_ = "number";
    Boolean = "boolean";
    String = "string";
    Symbol = "symbol";
    Object = "object";
    Abstract = "abstract";
    Accessor = "accessor";
    Satisfies = "satisfies";
    Require = "require";
    Extends = "extends";
    Implements = "implements";
    Global = "global";
    Interface = "interface";
    Enum = "enum";
    MinusQMarkColon = "-?:";
    PlusQMarkColon = "+?:";
    QMarkColon = "?:";
    Asserts = "asserts";
    Infer = "infer";
    Is = "is";
    Keyof = "keyof";
    TS0 = "unique symbol";
    Unknown = "unknown";
    Never = "never";
    LBracePipe = "{|";
    PipeRBrace = "|}";
    _AutomaticSemicolon = "_automatic_semicolon";
    StringFragment__ = "string_fragment";
    QMark_ = "?";
    HtmlComment = "html_comment";
    JsxText = "jsx_text";
    _FunctionSignatureAutomaticSemicolon = "_function_signature_automatic_semicolon";
    __ErrorRecovery = "__error_recovery";
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
    Object_ = "object";
    ObjectPattern = "object_pattern";
    AssignmentPattern = "assignment_pattern";
    ObjectAssignmentPattern = "object_assignment_pattern";
    Array = "array";
    ArrayPattern = "array_pattern";
    NestedIdentifier = "nested_identifier";
    Class_ = "class";
    ClassDeclaration = "class_declaration";
    ClassHeritage = "class_heritage";
    FunctionExpression = "function_expression";
    FunctionDeclaration = "function_declaration";
    GeneratorFunction = "generator_function";
    GeneratorFunctionDeclaration = "generator_function_declaration";
    ArrowFunction = "arrow_function";
    _CallSignature = "_call_signature";
    _FormalParameter = "_formal_parameter";
    OptionalChain = "optional_chain";
    CallExpression = "call_expression";
    NewExpression = "new_expression";
    AwaitExpression = "await_expression";
    MemberExpression = "member_expression";
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
    MemberExpression_ = "member_expression";
    CallExpression_ = "call_expression";
    ClassBody = "class_body";
    FormalParameters = "formal_parameters";
    ClassStaticBlock = "class_static_block";
    Pattern = "pattern";
    RestPattern = "rest_pattern";
    MethodDefinition = "method_definition";
    Pair = "pair";
    PairPattern = "pair_pattern";
    _PropertyName = "_property_name";
    ComputedPropertyName = "computed_property_name";
    PublicFieldDefinition = "public_field_definition";
    _ImportIdentifier = "_import_identifier";
    NonNullExpression = "non_null_expression";
    MethodSignature = "method_signature";
    AbstractMethodSignature = "abstract_method_signature";
    FunctionSignature = "function_signature";
    ParenthesizedExpression_ = "parenthesized_expression";
    TypeAssertion = "type_assertion";
    AsExpression = "as_expression";
    SatisfiesExpression = "satisfies_expression";
    InstantiationExpression = "instantiation_expression";
    ImportRequireClause = "import_require_clause";
    ExtendsClause = "extends_clause";
    _ExtendsClauseSingle = "_extends_clause_single";
    ImplementsClause = "implements_clause";
    AmbientDeclaration
    = "ambient_declaration";
    AbstractClassDeclaration = "abstract_class_declaration";
    Module_ = "module";
    InternalModule = "internal_module";
    _Module = "_module";
    ImportAlias = "import_alias";
    NestedTypeIdentifier = "nested_type_identifier";
    InterfaceDeclaration = "interface_declaration";
    ExtendsTypeClause = "extends_type_clause";
    EnumDeclaration = "enum_declaration";
    EnumBody = "enum_body";
    EnumAssignment = "enum_assignment";
    TypeAliasDeclaration = "type_alias_declaration";
    AccessibilityModifier = "accessibility_modifier";
    OverrideModifier = "override_modifier";
    RequiredParameter = "required_parameter";
    OptionalParameter = "optional_parameter";
    _ParameterName = "_parameter_name";
    OmittingTypeAnnotation = "omitting_type_annotation";
    AddingTypeAnnotation = "adding_type_annotation";
    OptingTypeAnnotation = "opting_type_annotation";
    TypeAnnotation = "type_annotation";
    MemberExpression__ = "member_expression";
    CallExpression__ = "call_expression";
    Asserts_ = "asserts";
    AssertsAnnotation = "asserts_annotation";
    Type_ = "type";
    RequiredParameter_ = "required_parameter";
    OptionalParameter_ = "optional_parameter";
    OptionalType = "optional_type";
    RestType = "rest_type";
    _TupleTypeMember = "_tuple_type_member";
    ConstructorType = "constructor_type";
    PrimaryType = "primary_type";
    TemplateType = "template_type";
    TemplateLiteralType = "template_literal_type";
    InferType = "infer_type";
    ConditionalType = "conditional_type";
    GenericType = "generic_type";
    TypePredicate = "type_predicate";
    TypePredicateAnnotation = "type_predicate_annotation";
    MemberExpression___ = "member_expression";
    SubscriptExpression_ = "subscript_expression";
    CallExpression___
    = "call_expression";
    InstantiationExpression_ = "instantiation_expression";
    TypeQuery
    = "type_query";
    IndexTypeQuery = "index_type_query";
    LookupType = "lookup_type";
    MappedTypeClause = "mapped_type_clause";
    LiteralType = "literal_type";
    UnaryExpression_ = "unary_expression";
    ExistentialType = "existential_type";
    FlowMaybeType = "flow_maybe_type";
    ParenthesizedType = "parenthesized_type";
    PredefinedType = "predefined_type";
    TypeArguments = "type_arguments";
    ObjectType = "object_type";
    CallSignature = "call_signature";
    PropertySignature = "property_signature";
    TypeParameters = "type_parameters";
    TypeParameter = "type_parameter";
    DefaultType = "default_type";
    Constraint = "constraint";
    ConstructSignature = "construct_signature";
    IndexSignature = "index_signature";
    ArrayType = "array_type";
    TupleType = "tuple_type";
    ReadonlyType = "readonly_type";
    UnionType = "union_type";
    IntersectionType = "intersection_type";
    FunctionType = "function_type";
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
    SequenceExpressionRepeat1 = "sequence_expression_repeat1";
    StringRepeat1 = "string_repeat1";
    StringRepeat2 = "string_repeat2";
    TemplateStringRepeat1 = "template_string_repeat1";
    ClassBodyRepeat1 = "class_body_repeat1";
    FormalParametersRepeat1 = "formal_parameters_repeat1";
    ExtendsClauseRepeat1 = "extends_clause_repeat1";
    ImplementsClauseRepeat1 = "implements_clause_repeat1";
    ExtendsTypeClauseRepeat1 = "extends_type_clause_repeat1";
    EnumBodyRepeat1 = "enum_body_repeat1";
    TemplateLiteralTypeRepeat1 = "template_literal_type_repeat1";
    ObjectTypeRepeat1 = "object_type_repeat1";
    TypeParametersRepeat1 = "type_parameters_repeat1";
    TupleTypeRepeat1 = "tuple_type_repeat1";
    InterfaceBody = "interface_body";
    PropertyIdentifier = "property_identifier";
    ShorthandPropertyIdentifier = "shorthand_property_identifier";
    ShorthandPropertyIdentifierPattern = "shorthand_property_identifier_pattern";
    StatementIdentifier = "statement_identifier";
    ThisType = "this_type";
    TypeIdentifier
    = "type_identifier";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self, End, _AutomaticSemicolon, _FunctionSignatureAutomaticSemicolon,
            __ErrorRecovery, _ModuleExportName, Declaration, _FromClause, Statement,
            _ForHeader, Expression, PrimaryExpression, _CallSignature, _FormalParameter,
            _AugmentedAssignmentLhs, _Initializer, _DestructuringPattern, Pattern,
            _PropertyName, _ImportIdentifier, _ExtendsClauseSingle, _Module,
            _ParameterName, _TupleTypeMember, PrimaryType, ProgramRepeat1,
            ExportStatementRepeat1, ExportClauseRepeat1, NamedImportsRepeat1,
            VariableDeclarationRepeat1, SwitchBodyRepeat1, ObjectRepeat1,
            ObjectPatternRepeat1, ArrayRepeat1, ArrayPatternRepeat1,
            SequenceExpressionRepeat1, StringRepeat1, StringRepeat2,
            TemplateStringRepeat1, ClassBodyRepeat1, FormalParametersRepeat1,
            ExtendsClauseRepeat1, ImplementsClauseRepeat1, ExtendsTypeClauseRepeat1,
            EnumBodyRepeat1, TemplateLiteralTypeRepeat1, ObjectTypeRepeat1,
            TypeParametersRepeat1, TupleTypeRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(
            self, Declaration, Statement, Expression, PrimaryExpression, Pattern,
            PrimaryType,
        )
    }
    pub fn is_named(&self) -> bool {
        is!(
            self, Identifier, HashBangLine, StringFragment, StringFragment_,
            EscapeSequence, Comment, RegexPattern, RegexFlags, PrivatePropertyIdentifier,
            This, Super, True, False, Null, Undefined, StringFragment__, HtmlComment,
            JsxText, Program, ExportStatement, NamespaceExport, ExportClause,
            ExportSpecifier, Declaration, ImportStatement, ImportClause, NamespaceImport,
            NamedImports, ImportSpecifier, ImportAttribute, Statement,
            ExpressionStatement, VariableDeclaration, LexicalDeclaration,
            VariableDeclarator, StatementBlock, ElseClause, IfStatement, SwitchStatement,
            ForStatement, ForInStatement, WhileStatement, DoStatement, TryStatement,
            WithStatement, BreakStatement, ContinueStatement, DebuggerStatement,
            ReturnStatement, ThrowStatement, EmptyStatement, LabeledStatement,
            SwitchBody, SwitchCase, SwitchDefault, CatchClause, FinallyClause,
            ParenthesizedExpression, Expression, PrimaryExpression, YieldExpression,
            ObjectPattern, AssignmentPattern, ObjectAssignmentPattern, Array,
            ArrayPattern, NestedIdentifier, ClassDeclaration, ClassHeritage,
            FunctionExpression, FunctionDeclaration, GeneratorFunction,
            GeneratorFunctionDeclaration, ArrowFunction, OptionalChain, CallExpression,
            NewExpression, AwaitExpression, MemberExpression, SubscriptExpression,
            AssignmentExpression, AugmentedAssignmentExpression, SpreadElement,
            TernaryExpression, BinaryExpression, UnaryExpression, UpdateExpression,
            SequenceExpression, TemplateString, TemplateSubstitution, Regex,
            MetaProperty, Arguments, Decorator, MemberExpression_, CallExpression_,
            ClassBody, FormalParameters, ClassStaticBlock, Pattern, RestPattern,
            MethodDefinition, Pair, PairPattern, ComputedPropertyName,
            PublicFieldDefinition, NonNullExpression, MethodSignature,
            AbstractMethodSignature, FunctionSignature, ParenthesizedExpression_,
            TypeAssertion, AsExpression, SatisfiesExpression, InstantiationExpression,
            ImportRequireClause, ExtendsClause, ImplementsClause, AmbientDeclaration,
            AbstractClassDeclaration, InternalModule, ImportAlias, NestedTypeIdentifier,
            InterfaceDeclaration, ExtendsTypeClause, EnumDeclaration, EnumBody,
            EnumAssignment, TypeAliasDeclaration, AccessibilityModifier,
            OverrideModifier, RequiredParameter, OptionalParameter,
            OmittingTypeAnnotation, AddingTypeAnnotation, OptingTypeAnnotation,
            TypeAnnotation, MemberExpression__, CallExpression__, AssertsAnnotation,
            RequiredParameter_, OptionalParameter_, OptionalType, RestType,
            ConstructorType, PrimaryType, TemplateType, TemplateLiteralType, InferType,
            ConditionalType, GenericType, TypePredicate, TypePredicateAnnotation,
            MemberExpression___, SubscriptExpression_, CallExpression___,
            InstantiationExpression_, TypeQuery, IndexTypeQuery, LookupType,
            MappedTypeClause, LiteralType, UnaryExpression_, ExistentialType,
            FlowMaybeType, ParenthesizedType, PredefinedType, TypeArguments, ObjectType,
            CallSignature, PropertySignature, TypeParameters, TypeParameter, DefaultType,
            Constraint, ConstructSignature, IndexSignature, ArrayType, TupleType,
            ReadonlyType, UnionType, IntersectionType, FunctionType, InterfaceBody,
            PropertyIdentifier, ShorthandPropertyIdentifier,
            ShorthandPropertyIdentifierPattern, StatementIdentifier, ThisType,
            TypeIdentifier,
        )
    }
}
