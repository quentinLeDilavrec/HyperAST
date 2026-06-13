// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_java --example regen_java_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Identifier = "identifier";
    DecimalIntegerLiteral = "decimal_integer_literal";
    HexIntegerLiteral = "hex_integer_literal";
    OctalIntegerLiteral = "octal_integer_literal";
    BinaryIntegerLiteral = "binary_integer_literal";
    DecimalFloatingPointLiteral = "decimal_floating_point_literal";
    HexFloatingPointLiteral = "hex_floating_point_literal";
    True = "true";
    False = "false";
    CharacterLiteral = "character_literal";
    DQuote = "\"";
    TS0 = "\"\"\"";
    StringFragment = "string_fragment";
    _MultilineStringFragmentToken1 = "_multiline_string_fragment_token1";
    _MultilineStringFragmentToken2 = "_multiline_string_fragment_token2";
    TS1 = "\\{";
    RBrace = "}";
    _EscapeSequenceToken1
    = "_escape_sequence_token1";
    EscapeSequence = "escape_sequence";
    NullLiteral = "null_literal";
    LParen = "(";
    RParen = ")";
    Amp = "&";
    Eq = "=";
    PlusEq = "+=";
    DashEq = "-=";
    StarEq = "*=";
    SlashEq = "/=";
    AmpEq = "&=";
    PipeEq = "|=";
    CaretEq = "^=";
    PercentEq = "%=";
    LtLtEq = "<<=";
    GtGtEq = ">>=";
    GtGtGtEq = ">>>=";
    GT = ">";
    LT = "<";
    GTEq = ">=";
    LTEq = "<=";
    EqEq = "==";
    BangEq = "!=";
    AmpAmp = "&&";
    PipePipe = "||";
    Plus = "+";
    Dash = "-";
    Star = "*";
    Slash = "/";
    Pipe = "|";
    Caret = "^";
    Percent = "%";
    LtLt = "<<";
    GtGt = ">>";
    GtGtGt = ">>>";
    Instanceof = "instanceof";
    Final = "final";
    DashGt = "->";
    Comma = ",";
    QMark = "?";
    Colon = ":";
    Bang = "!";
    Tilde = "~";
    PlusPlus = "++";
    DashDash = "--";
    New = "new";
    LBracket = "[";
    RBracket = "]";
    Dot = ".";
    Class = "class";
    ColonColon = "::";
    Extends = "extends";
    Switch = "switch";
    LBrace = "{";
    Case = "case";
    Default = "default";
    UnderscorePattern = "underscore_pattern";
    When = "when";
    SemiColon = ";";
    Assert = "assert";
    Do = "do";
    While = "while";
    Break = "break";
    Continue = "continue";
    Return
    = "return";
    Yield = "yield";
    Synchronized = "synchronized";
    Throw = "throw";
    Try = "try";
    Catch = "catch";
    Finally = "finally";
    If = "if";
    Else = "else";
    For = "for";
    At = "@";
    Open = "open";
    Module = "module";
    Requires = "requires";
    Transitive = "transitive";
    Static = "static";
    Exports = "exports";
    To = "to";
    Opens = "opens";
    Uses = "uses";
    Provides = "provides";
    With = "with";
    Package = "package";
    Import = "import";
    Enum = "enum";
    Public = "public";
    Protected = "protected";
    Private = "private";
    Abstract = "abstract";
    Strictfp = "strictfp";
    Native = "native";
    Transient
    = "transient";
    Volatile = "volatile";
    Sealed = "sealed";
    TS2 = "non-sealed";
    Implements = "implements";
    Permits = "permits";
    Record = "record";
    TS3 = "@interface";
    Interface = "interface";
    Byte = "byte";
    Short = "short";
    Int = "int";
    Long = "long";
    Char = "char";
    Float = "float";
    Double = "double";
    BooleanType = "boolean_type";
    VoidType = "void_type";
    DotDotDot = "...";
    Throws = "throws";
    This = "this";
    Super = "super";
    LineComment = "line_comment";
    BlockComment = "block_comment";
    Program = "program";
    _ToplevelStatement = "_toplevel_statement";
    _Literal = "_literal";
    StringLiteral = "string_literal";
    _StringLiteral = "_string_literal";
    _MultilineStringLiteral = "_multiline_string_literal";
    MultilineStringFragment = "multiline_string_fragment";
    StringInterpolation = "string_interpolation";
    _EscapeSequence = "_escape_sequence";
    Expression = "expression";
    CastExpression = "cast_expression";
    AssignmentExpression = "assignment_expression";
    BinaryExpression = "binary_expression";
    InstanceofExpression
    = "instanceof_expression";
    LambdaExpression = "lambda_expression";
    InferredParameters
    = "inferred_parameters";
    TernaryExpression = "ternary_expression";
    UnaryExpression = "unary_expression";
    UpdateExpression = "update_expression";
    PrimaryExpression = "primary_expression";
    ArrayCreationExpression = "array_creation_expression";
    DimensionsExpr = "dimensions_expr";
    ParenthesizedExpression = "parenthesized_expression";
    ClassLiteral = "class_literal";
    ObjectCreationExpression
    = "object_creation_expression";
    _UnqualifiedObjectCreationExpression = "_unqualified_object_creation_expression";
    FieldAccess = "field_access";
    TemplateExpression = "template_expression";
    ArrayAccess = "array_access";
    MethodInvocation = "method_invocation";
    ArgumentList = "argument_list";
    MethodReference = "method_reference";
    TypeArguments = "type_arguments";
    Wildcard = "wildcard";
    _WildcardBounds = "_wildcard_bounds";
    Dimensions = "dimensions";
    SwitchExpression = "switch_expression";
    SwitchBlock = "switch_block";
    SwitchBlockStatementGroup = "switch_block_statement_group";
    SwitchRule = "switch_rule";
    SwitchLabel = "switch_label";
    Pattern = "pattern";
    TypePattern = "type_pattern";
    RecordPattern = "record_pattern";
    RecordPatternBody = "record_pattern_body";
    RecordPatternComponent = "record_pattern_component";
    Guard = "guard";
    Statement = "statement";
    Block = "block";
    ExpressionStatement = "expression_statement";
    LabeledStatement = "labeled_statement";
    AssertStatement = "assert_statement";
    DoStatement = "do_statement";
    BreakStatement = "break_statement";
    ContinueStatement = "continue_statement";
    ReturnStatement = "return_statement";
    YieldStatement = "yield_statement";
    SynchronizedStatement = "synchronized_statement";
    ThrowStatement = "throw_statement";
    TryStatement = "try_statement";
    CatchClause = "catch_clause";
    CatchFormalParameter = "catch_formal_parameter";
    CatchType = "catch_type";
    FinallyClause = "finally_clause";
    TryWithResourcesStatement = "try_with_resources_statement";
    ResourceSpecification = "resource_specification";
    Resource = "resource";
    IfStatement = "if_statement";
    WhileStatement = "while_statement";
    ForStatement = "for_statement";
    EnhancedForStatement = "enhanced_for_statement";
    _Annotation = "_annotation";
    MarkerAnnotation = "marker_annotation";
    Annotation = "annotation";
    AnnotationArgumentList = "annotation_argument_list";
    ElementValuePair = "element_value_pair";
    _ElementValue = "_element_value";
    ElementValueArrayInitializer = "element_value_array_initializer";
    Declaration = "declaration";
    ModuleDeclaration = "module_declaration";
    ModuleBody = "module_body";
    ModuleDirective = "module_directive";
    RequiresModuleDirective = "requires_module_directive";
    RequiresModifier = "requires_modifier";
    ExportsModuleDirective = "exports_module_directive";
    OpensModuleDirective = "opens_module_directive";
    UsesModuleDirective = "uses_module_directive";
    ProvidesModuleDirective = "provides_module_directive";
    PackageDeclaration = "package_declaration";
    ImportDeclaration = "import_declaration";
    Asterisk = "asterisk";
    EnumDeclaration = "enum_declaration";
    EnumBody = "enum_body";
    EnumBodyDeclarations = "enum_body_declarations";
    EnumConstant = "enum_constant";
    ClassDeclaration = "class_declaration";
    Modifiers = "modifiers";
    TypeParameters = "type_parameters";
    TypeParameter = "type_parameter";
    TypeBound = "type_bound";
    Superclass = "superclass";
    SuperInterfaces = "super_interfaces";
    TypeList = "type_list";
    Permits_ = "permits";
    ClassBody = "class_body";
    StaticInitializer = "static_initializer";
    ConstructorDeclaration = "constructor_declaration";
    _ConstructorDeclarator = "_constructor_declarator";
    ConstructorBody = "constructor_body";
    ExplicitConstructorInvocation = "explicit_constructor_invocation";
    ScopedIdentifier = "scoped_identifier";
    FieldDeclaration = "field_declaration";
    RecordDeclaration = "record_declaration";
    AnnotationTypeDeclaration = "annotation_type_declaration";
    AnnotationTypeBody = "annotation_type_body";
    AnnotationTypeElementDeclaration = "annotation_type_element_declaration";
    _DefaultValue = "_default_value";
    InterfaceDeclaration = "interface_declaration";
    ExtendsInterfaces = "extends_interfaces";
    InterfaceBody = "interface_body";
    ConstantDeclaration = "constant_declaration";
    _VariableDeclaratorList = "_variable_declarator_list";
    VariableDeclarator = "variable_declarator";
    _VariableDeclaratorId = "_variable_declarator_id";
    ArrayInitializer = "array_initializer";
    _Type = "_type";
    _UnannotatedType = "_unannotated_type";
    AnnotatedType = "annotated_type";
    ScopedTypeIdentifier = "scoped_type_identifier";
    GenericType = "generic_type";
    ArrayType = "array_type";
    IntegralType = "integral_type";
    FloatingPointType = "floating_point_type";
    _MethodHeader = "_method_header";
    _MethodDeclarator = "_method_declarator";
    FormalParameters = "formal_parameters";
    FormalParameter = "formal_parameter";
    ReceiverParameter = "receiver_parameter";
    SpreadParameter = "spread_parameter";
    Throws_ = "throws";
    LocalVariableDeclaration = "local_variable_declaration";
    MethodDeclaration = "method_declaration";
    CompactConstructorDeclaration = "compact_constructor_declaration";
    _ReservedIdentifier = "_reserved_identifier";
    ProgramRepeat1 = "program_repeat1";
    _StringLiteralRepeat1 = "_string_literal_repeat1";
    _MultilineStringLiteralRepeat1 = "_multiline_string_literal_repeat1";
    CastExpressionRepeat1 = "cast_expression_repeat1";
    InferredParametersRepeat1 = "inferred_parameters_repeat1";
    ArrayCreationExpressionRepeat1 = "array_creation_expression_repeat1";
    ArrayCreationExpressionRepeat2 = "array_creation_expression_repeat2";
    ArgumentListRepeat1 = "argument_list_repeat1";
    TypeArgumentsRepeat1 = "type_arguments_repeat1";
    DimensionsRepeat1 = "dimensions_repeat1";
    SwitchBlockRepeat1 = "switch_block_repeat1";
    SwitchBlockRepeat2 = "switch_block_repeat2";
    SwitchBlockStatementGroupRepeat1 = "switch_block_statement_group_repeat1";
    SwitchBlockStatementGroupRepeat2 = "switch_block_statement_group_repeat2";
    RecordPatternBodyRepeat1 = "record_pattern_body_repeat1";
    TryStatementRepeat1 = "try_statement_repeat1";
    CatchTypeRepeat1 = "catch_type_repeat1";
    ResourceSpecificationRepeat1 = "resource_specification_repeat1";
    ForStatementRepeat1 = "for_statement_repeat1";
    ForStatementRepeat2 = "for_statement_repeat2";
    AnnotationArgumentListRepeat1 = "annotation_argument_list_repeat1";
    ElementValueArrayInitializerRepeat1 = "element_value_array_initializer_repeat1";
    ModuleBodyRepeat1 = "module_body_repeat1";
    RequiresModuleDirectiveRepeat1 = "requires_module_directive_repeat1";
    ExportsModuleDirectiveRepeat1 = "exports_module_directive_repeat1";
    ProvidesModuleDirectiveRepeat1 = "provides_module_directive_repeat1";
    EnumBodyRepeat1
    = "enum_body_repeat1";
    EnumBodyDeclarationsRepeat1 = "enum_body_declarations_repeat1";
    ModifiersRepeat1 = "modifiers_repeat1";
    TypeParametersRepeat1 = "type_parameters_repeat1";
    TypeBoundRepeat1 = "type_bound_repeat1";
    TypeListRepeat1 = "type_list_repeat1";
    AnnotationTypeBodyRepeat1 = "annotation_type_body_repeat1";
    InterfaceBodyRepeat1 = "interface_body_repeat1";
    _VariableDeclaratorListRepeat1 = "_variable_declarator_list_repeat1";
    ArrayInitializerRepeat1 = "array_initializer_repeat1";
    FormalParametersRepeat1 = "formal_parameters_repeat1";
    ReceiverParameterRepeat1 = "receiver_parameter_repeat1";
    TypeIdentifier = "type_identifier";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self, End, _MultilineStringFragmentToken1, _MultilineStringFragmentToken2,
            _EscapeSequenceToken1, _ToplevelStatement, _Literal, _StringLiteral,
            _MultilineStringLiteral, _EscapeSequence, Expression, PrimaryExpression,
            _UnqualifiedObjectCreationExpression, _WildcardBounds, Statement,
            _Annotation, _ElementValue, Declaration, ModuleDirective,
            _ConstructorDeclarator, _DefaultValue, _VariableDeclaratorList,
            _VariableDeclaratorId, _Type, _UnannotatedType, _MethodHeader,
            _MethodDeclarator, _ReservedIdentifier, ProgramRepeat1,
            _StringLiteralRepeat1, _MultilineStringLiteralRepeat1, CastExpressionRepeat1,
            InferredParametersRepeat1, ArrayCreationExpressionRepeat1,
            ArrayCreationExpressionRepeat2, ArgumentListRepeat1, TypeArgumentsRepeat1,
            DimensionsRepeat1, SwitchBlockRepeat1, SwitchBlockRepeat2,
            SwitchBlockStatementGroupRepeat1, SwitchBlockStatementGroupRepeat2,
            RecordPatternBodyRepeat1, TryStatementRepeat1, CatchTypeRepeat1,
            ResourceSpecificationRepeat1, ForStatementRepeat1, ForStatementRepeat2,
            AnnotationArgumentListRepeat1, ElementValueArrayInitializerRepeat1,
            ModuleBodyRepeat1, RequiresModuleDirectiveRepeat1,
            ExportsModuleDirectiveRepeat1, ProvidesModuleDirectiveRepeat1,
            EnumBodyRepeat1, EnumBodyDeclarationsRepeat1, ModifiersRepeat1,
            TypeParametersRepeat1, TypeBoundRepeat1, TypeListRepeat1,
            AnnotationTypeBodyRepeat1, InterfaceBodyRepeat1,
            _VariableDeclaratorListRepeat1, ArrayInitializerRepeat1,
            FormalParametersRepeat1, ReceiverParameterRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(
            self, _Literal, Expression, PrimaryExpression, Statement, Declaration,
            ModuleDirective, _Type, _UnannotatedType,
        )
    }
    pub fn is_named(&self) -> bool {
        is!(
            self, Identifier, DecimalIntegerLiteral, HexIntegerLiteral,
            OctalIntegerLiteral, BinaryIntegerLiteral, DecimalFloatingPointLiteral,
            HexFloatingPointLiteral, True, False, CharacterLiteral, StringFragment,
            EscapeSequence, NullLiteral, UnderscorePattern, BooleanType, VoidType, This,
            Super, LineComment, BlockComment, Program, _Literal, StringLiteral,
            MultilineStringFragment, StringInterpolation, Expression, CastExpression,
            AssignmentExpression, BinaryExpression, InstanceofExpression,
            LambdaExpression, InferredParameters, TernaryExpression, UnaryExpression,
            UpdateExpression, PrimaryExpression, ArrayCreationExpression, DimensionsExpr,
            ParenthesizedExpression, ClassLiteral, ObjectCreationExpression, FieldAccess,
            TemplateExpression, ArrayAccess, MethodInvocation, ArgumentList,
            MethodReference, TypeArguments, Wildcard, Dimensions, SwitchExpression,
            SwitchBlock, SwitchBlockStatementGroup, SwitchRule, SwitchLabel, Pattern,
            TypePattern, RecordPattern, RecordPatternBody, RecordPatternComponent, Guard,
            Statement, Block, ExpressionStatement, LabeledStatement, AssertStatement,
            DoStatement, BreakStatement, ContinueStatement, ReturnStatement,
            YieldStatement, SynchronizedStatement, ThrowStatement, TryStatement,
            CatchClause, CatchFormalParameter, CatchType, FinallyClause,
            TryWithResourcesStatement, ResourceSpecification, Resource, IfStatement,
            WhileStatement, ForStatement, EnhancedForStatement, MarkerAnnotation,
            Annotation, AnnotationArgumentList, ElementValuePair,
            ElementValueArrayInitializer, Declaration, ModuleDeclaration, ModuleBody,
            ModuleDirective, RequiresModuleDirective, RequiresModifier,
            ExportsModuleDirective, OpensModuleDirective, UsesModuleDirective,
            ProvidesModuleDirective, PackageDeclaration, ImportDeclaration, Asterisk,
            EnumDeclaration, EnumBody, EnumBodyDeclarations, EnumConstant,
            ClassDeclaration, Modifiers, TypeParameters, TypeParameter, TypeBound,
            Superclass, SuperInterfaces, TypeList, ClassBody, StaticInitializer,
            ConstructorDeclaration, ConstructorBody, ExplicitConstructorInvocation,
            ScopedIdentifier, FieldDeclaration, RecordDeclaration,
            AnnotationTypeDeclaration, AnnotationTypeBody,
            AnnotationTypeElementDeclaration, InterfaceDeclaration, ExtendsInterfaces,
            InterfaceBody, ConstantDeclaration, VariableDeclarator, ArrayInitializer,
            _Type, _UnannotatedType, AnnotatedType, ScopedTypeIdentifier, GenericType,
            ArrayType, IntegralType, FloatingPointType, FormalParameters,
            FormalParameter, ReceiverParameter, SpreadParameter,
            LocalVariableDeclaration, MethodDeclaration, CompactConstructorDeclaration,
            TypeIdentifier,
        )
    }
}
