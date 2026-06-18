// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_rust --example regen_rust_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Identifier = "identifier";
    SemiColon = ";";
    TS0 = "macro_rules!";
    LParen
    = "(";
    RParen = ")";
    LBracket = "[";
    RBracket = "]";
    LBrace = "{";
    RBrace = "}";
    BigArrow = "=>";
    Colon = ":";
    TS1 = "$";
    TokenRepetitionPatternToken1 = "token_repetition_pattern_token1";
    Plus = "+";
    Star = "*";
    QMark = "?";
    Block = "block";
    Expr = "expr";
    TS2 = "expr_2021";
    Ident = "ident";
    Item = "item";
    Lifetime = "lifetime";
    Literal = "literal";
    Meta = "meta";
    Pat = "pat";
    PatParam = "pat_param";
    Path = "path";
    Stmt = "stmt";
    Tt = "tt";
    Ty = "ty";
    Vis = "vis";
    PrimitiveType = "primitive_type";
    PrimitiveType_ = "primitive_type";
    PrimitiveType__ = "primitive_type";
    PrimitiveType___ = "primitive_type";
    PrimitiveType____ = "primitive_type";
    PrimitiveType_____ = "primitive_type";
    PrimitiveType______ = "primitive_type";
    PrimitiveType_______ = "primitive_type";
    PrimitiveType________ = "primitive_type";
    PrimitiveType_________ = "primitive_type";
    PrimitiveType__________ = "primitive_type";
    PrimitiveType___________ = "primitive_type";
    PrimitiveType____________ = "primitive_type";
    PrimitiveType_____________ = "primitive_type";
    PrimitiveType______________ = "primitive_type";
    PrimitiveType_______________ = "primitive_type";
    PrimitiveType________________ = "primitive_type";
    Dash = "-";
    Slash = "/";
    Percent = "%";
    Caret = "^";
    Bang = "!";
    Amp = "&";
    Pipe = "|";
    AmpAmp = "&&";
    PipePipe = "||";
    LtLt = "<<";
    GtGt = ">>";
    PlusEq = "+=";
    DashEq = "-=";
    StarEq = "*=";
    SlashEq = "/=";
    PercentEq = "%=";
    CaretEq = "^=";
    AmpEq = "&=";
    PipeEq = "|=";
    LtLtEq = "<<=";
    GtGtEq = ">>=";
    Eq = "=";
    EqEq = "==";
    BangEq = "!=";
    GT = ">";
    LT = "<";
    GTEq = ">=";
    LTEq = "<=";
    At = "@";
    Inderscore = "_";
    Dot = ".";
    TS3 = "..";
    DotDotDot = "...";
    TS4 = "..=";
    Comma = ",";
    ColonColon = "::";
    DashGt = "->";
    Sharp = "#";
    SQuote = "'";
    As = "as";
    Async = "async";
    Await = "await";
    Break = "break";
    Const = "const";
    Continue = "continue";
    Default = "default";
    Enum = "enum";
    Fn = "fn";
    For = "for";
    Gen = "gen";
    If = "if";
    Impl = "impl";
    Let = "let";
    Loop = "loop";
    Match = "match";
    Mod = "mod";
    Pub = "pub";
    Return = "return";
    Static = "static";
    Struct = "struct";
    Trait = "trait";
    Type = "type";
    Union = "union";
    Unsafe = "unsafe";
    Use = "use";
    Where = "where";
    While = "while";
    Extern = "extern";
    Ref = "ref";
    Else = "else";
    In = "in";
    LT_ = "<";
    Dyn = "dyn";
    MutableSpecifier = "mutable_specifier";
    Raw = "raw";
    Yield = "yield";
    Move = "move";
    Try = "try";
    IntegerLiteral = "integer_literal";
    DQuote = "\"";
    DQuote_ = "\"";
    CharLiteral = "char_literal";
    EscapeSequence = "escape_sequence";
    True = "true";
    False = "false";
    TS5 = "//";
    LineCommentToken1 = "line_comment_token1";
    LineCommentToken2 = "line_comment_token2";
    LineCommentToken3 = "line_comment_token3";
    Bang_ = "!";
    Slash_ = "/";
    TS6 = "/*";
    TS7 = "*/";
    Shebang = "shebang";
    TS8 = "self";
    Super = "super";
    Crate = "crate";
    Metavariable = "metavariable";
    StringContent = "string_content";
    _RawStringLiteralStart = "_raw_string_literal_start";
    StringContent_ = "string_content";
    _RawStringLiteralEnd = "_raw_string_literal_end";
    FloatLiteral = "float_literal";
    OuterDocCommentMarker = "outer_doc_comment_marker";
    InnerDocCommentMarker = "inner_doc_comment_marker";
    _BlockCommentContent = "_block_comment_content";
    DocComment = "doc_comment";
    _ErrorSentinel = "_error_sentinel";
    SourceFile = "source_file";
    _Statement = "_statement";
    EmptyStatement = "empty_statement";
    ExpressionStatement = "expression_statement";
    MacroDefinition = "macro_definition";
    MacroRule = "macro_rule";
    _TokenPattern = "_token_pattern";
    TokenTreePattern = "token_tree_pattern";
    TokenBindingPattern = "token_binding_pattern";
    TokenRepetitionPattern = "token_repetition_pattern";
    FragmentSpecifier = "fragment_specifier";
    TokenTree = "token_tree";
    TokenRepetition = "token_repetition";
    AttributeItem = "attribute_item";
    InnerAttributeItem = "inner_attribute_item";
    Attribute = "attribute";
    ModItem = "mod_item";
    ForeignModItem
    = "foreign_mod_item";
    DeclarationList = "declaration_list";
    StructItem = "struct_item";
    UnionItem = "union_item";
    EnumItem = "enum_item";
    EnumVariantList = "enum_variant_list";
    EnumVariant = "enum_variant";
    FieldDeclarationList = "field_declaration_list";
    FieldDeclaration = "field_declaration";
    OrderedFieldDeclarationList = "ordered_field_declaration_list";
    ExternCrateDeclaration = "extern_crate_declaration";
    ConstItem = "const_item";
    StaticItem = "static_item";
    TypeItem = "type_item";
    FunctionItem = "function_item";
    FunctionSignatureItem = "function_signature_item";
    FunctionModifiers = "function_modifiers";
    WhereClause = "where_clause";
    WherePredicate = "where_predicate";
    ImplItem = "impl_item";
    TraitItem = "trait_item";
    AssociatedType = "associated_type";
    TraitBounds = "trait_bounds";
    HigherRankedTraitBound = "higher_ranked_trait_bound";
    RemovedTraitBound = "removed_trait_bound";
    TypeParameters = "type_parameters";
    ConstParameter = "const_parameter";
    TypeParameter
    = "type_parameter";
    LifetimeParameter = "lifetime_parameter";
    LetDeclaration = "let_declaration";
    UseDeclaration = "use_declaration";
    _UseClause = "_use_clause";
    ScopedUseList = "scoped_use_list";
    UseList = "use_list";
    UseAsClause = "use_as_clause";
    UseWildcard = "use_wildcard";
    Parameters = "parameters";
    SelfParameter = "self_parameter";
    VariadicParameter = "variadic_parameter";
    Parameter
    = "parameter";
    ExternModifier = "extern_modifier";
    VisibilityModifier = "visibility_modifier";
    _Type = "_type";
    BracketedType = "bracketed_type";
    QualifiedType = "qualified_type";
    Lifetime_ = "lifetime";
    ArrayType = "array_type";
    ForLifetimes = "for_lifetimes";
    FunctionType = "function_type";
    TupleType = "tuple_type";
    UnitType = "unit_type";
    GenericFunction = "generic_function";
    GenericType = "generic_type";
    GenericTypeWithTurbofish = "generic_type_with_turbofish";
    BoundedType = "bounded_type";
    UseBounds = "use_bounds";
    TypeArguments = "type_arguments";
    TypeBinding = "type_binding";
    ReferenceType = "reference_type";
    PointerType = "pointer_type";
    NeverType = "never_type";
    AbstractType = "abstract_type";
    DynamicType = "dynamic_type";
    _ExpressionExceptRange = "_expression_except_range";
    _Expression = "_expression";
    MacroInvocation = "macro_invocation";
    TokenTree_ = "token_tree";
    _DelimTokens = "_delim_tokens";
    _NonDelimToken = "_non_delim_token";
    ScopedIdentifier = "scoped_identifier";
    ScopedTypeIdentifier = "scoped_type_identifier";
    ScopedTypeIdentifier_ = "scoped_type_identifier";
    RangeExpression = "range_expression";
    UnaryExpression = "unary_expression";
    TryExpression = "try_expression";
    ReferenceExpression = "reference_expression";
    BinaryExpression = "binary_expression";
    AssignmentExpression = "assignment_expression";
    CompoundAssignmentExpr = "compound_assignment_expr";
    TypeCastExpression = "type_cast_expression";
    ReturnExpression = "return_expression";
    YieldExpression = "yield_expression";
    CallExpression = "call_expression";
    Arguments = "arguments";
    ArrayExpression = "array_expression";
    ParenthesizedExpression = "parenthesized_expression";
    TupleExpression = "tuple_expression";
    UnitExpression = "unit_expression";
    StructExpression = "struct_expression";
    FieldInitializerList = "field_initializer_list";
    ShorthandFieldInitializer = "shorthand_field_initializer";
    FieldInitializer = "field_initializer";
    BaseFieldInitializer = "base_field_initializer";
    IfExpression = "if_expression";
    LetCondition = "let_condition";
    _LetChain = "_let_chain";
    _Condition = "_condition";
    ElseClause = "else_clause";
    MatchExpression = "match_expression";
    MatchBlock = "match_block";
    MatchArm = "match_arm";
    MatchArm_ = "match_arm";
    MatchPattern = "match_pattern";
    WhileExpression = "while_expression";
    LoopExpression = "loop_expression";
    ForExpression = "for_expression";
    ConstBlock = "const_block";
    ClosureExpression = "closure_expression";
    ClosureParameters = "closure_parameters";
    Label = "label";
    BreakExpression = "break_expression";
    ContinueExpression = "continue_expression";
    IndexExpression = "index_expression";
    AwaitExpression = "await_expression";
    FieldExpression = "field_expression";
    UnsafeBlock = "unsafe_block";
    AsyncBlock = "async_block";
    GenBlock = "gen_block";
    TryBlock = "try_block";
    Block_ = "block";
    _Pattern = "_pattern";
    GenericPattern = "generic_pattern";
    TuplePattern = "tuple_pattern";
    SlicePattern = "slice_pattern";
    TupleStructPattern = "tuple_struct_pattern";
    StructPattern = "struct_pattern";
    FieldPattern = "field_pattern";
    RemainingFieldPattern = "remaining_field_pattern";
    MutPattern = "mut_pattern";
    RangePattern = "range_pattern";
    RefPattern = "ref_pattern";
    CapturedPattern = "captured_pattern";
    ReferencePattern = "reference_pattern";
    OrPattern = "or_pattern";
    _Literal = "_literal";
    _LiteralPattern = "_literal_pattern";
    NegativeLiteral = "negative_literal";
    StringLiteral = "string_literal";
    RawStringLiteral = "raw_string_literal";
    BooleanLiteral = "boolean_literal";
    LineComment = "line_comment";
    _LineDocCommentMarker = "_line_doc_comment_marker";
    InnerDocCommentMarker_ = "inner_doc_comment_marker";
    OuterDocCommentMarker_ = "outer_doc_comment_marker";
    BlockComment = "block_comment";
    _BlockDocCommentMarker = "_block_doc_comment_marker";
    SourceFileRepeat1 = "source_file_repeat1";
    MacroDefinitionRepeat1 = "macro_definition_repeat1";
    TokenTreePatternRepeat1 = "token_tree_pattern_repeat1";
    TokenTreeRepeat1 = "token_tree_repeat1";
    _NonSpecialTokenRepeat1 = "_non_special_token_repeat1";
    DeclarationListRepeat1 = "declaration_list_repeat1";
    EnumVariantListRepeat1 = "enum_variant_list_repeat1";
    EnumVariantListRepeat2 = "enum_variant_list_repeat2";
    FieldDeclarationListRepeat1 = "field_declaration_list_repeat1";
    OrderedFieldDeclarationListRepeat1 = "ordered_field_declaration_list_repeat1";
    FunctionModifiersRepeat1 = "function_modifiers_repeat1";
    WhereClauseRepeat1 = "where_clause_repeat1";
    TraitBoundsRepeat1 = "trait_bounds_repeat1";
    TypeParametersRepeat1 = "type_parameters_repeat1";
    UseListRepeat1 = "use_list_repeat1";
    ParametersRepeat1 = "parameters_repeat1";
    ForLifetimesRepeat1 = "for_lifetimes_repeat1";
    TupleTypeRepeat1 = "tuple_type_repeat1";
    UseBoundsRepeat1 = "use_bounds_repeat1";
    TypeArgumentsRepeat1 = "type_arguments_repeat1";
    DelimTokenTreeRepeat1 = "delim_token_tree_repeat1";
    ArgumentsRepeat1 = "arguments_repeat1";
    TupleExpressionRepeat1 = "tuple_expression_repeat1";
    FieldInitializerListRepeat1 = "field_initializer_list_repeat1";
    MatchBlockRepeat1 = "match_block_repeat1";
    MatchArmRepeat1 = "match_arm_repeat1";
    ClosureParametersRepeat1 = "closure_parameters_repeat1";
    TuplePatternRepeat1 = "tuple_pattern_repeat1";
    SlicePatternRepeat1 = "slice_pattern_repeat1";
    StructPatternRepeat1 = "struct_pattern_repeat1";
    StringLiteralRepeat1 = "string_literal_repeat1";
    FieldIdentifier = "field_identifier";
    LetChain = "let_chain";
    ShorthandFieldIdentifier = "shorthand_field_identifier";
    TypeIdentifier
    = "type_identifier";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self,
            End,
            TokenRepetitionPatternToken1,
            LineCommentToken1,
            LineCommentToken2,
            LineCommentToken3,
            _RawStringLiteralStart,
            _RawStringLiteralEnd,
            _BlockCommentContent,
            _ErrorSentinel,
            _Statement,
            _TokenPattern,
            _UseClause,
            _Type,
            _ExpressionExceptRange,
            _Expression,
            _DelimTokens,
            _NonDelimToken,
            _LetChain,
            _Condition,
            _Pattern,
            _Literal,
            _LiteralPattern,
            _LineDocCommentMarker,
            _BlockDocCommentMarker,
            SourceFileRepeat1,
            MacroDefinitionRepeat1,
            TokenTreePatternRepeat1,
            TokenTreeRepeat1,
            _NonSpecialTokenRepeat1,
            DeclarationListRepeat1,
            EnumVariantListRepeat1,
            EnumVariantListRepeat2,
            FieldDeclarationListRepeat1,
            OrderedFieldDeclarationListRepeat1,
            FunctionModifiersRepeat1,
            WhereClauseRepeat1,
            TraitBoundsRepeat1,
            TypeParametersRepeat1,
            UseListRepeat1,
            ParametersRepeat1,
            ForLifetimesRepeat1,
            TupleTypeRepeat1,
            UseBoundsRepeat1,
            TypeArgumentsRepeat1,
            DelimTokenTreeRepeat1,
            ArgumentsRepeat1,
            TupleExpressionRepeat1,
            FieldInitializerListRepeat1,
            MatchBlockRepeat1,
            MatchArmRepeat1,
            ClosureParametersRepeat1,
            TuplePatternRepeat1,
            SlicePatternRepeat1,
            StructPatternRepeat1,
            StringLiteralRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(
            self,
            _Type,
            _Expression,
            _Pattern,
            _Literal,
            _LiteralPattern,
        )
    }
    pub fn is_named(&self) -> bool {
        is!(
            self,
            Identifier,
            PrimitiveType,
            PrimitiveType_,
            PrimitiveType__,
            PrimitiveType___,
            PrimitiveType____,
            PrimitiveType_____,
            PrimitiveType______,
            PrimitiveType_______,
            PrimitiveType________,
            PrimitiveType_________,
            PrimitiveType__________,
            PrimitiveType___________,
            PrimitiveType____________,
            PrimitiveType_____________,
            PrimitiveType______________,
            PrimitiveType_______________,
            PrimitiveType________________,
            MutableSpecifier,
            IntegerLiteral,
            CharLiteral,
            EscapeSequence,
            Shebang,
            TS8,
            Super,
            Crate,
            Metavariable,
            StringContent,
            StringContent_,
            FloatLiteral,
            OuterDocCommentMarker,
            InnerDocCommentMarker,
            DocComment,
            SourceFile,
            EmptyStatement,
            ExpressionStatement,
            MacroDefinition,
            MacroRule,
            TokenTreePattern,
            TokenBindingPattern,
            TokenRepetitionPattern,
            FragmentSpecifier,
            TokenTree,
            TokenRepetition,
            AttributeItem,
            InnerAttributeItem,
            Attribute,
            ModItem,
            ForeignModItem,
            DeclarationList,
            StructItem,
            UnionItem,
            EnumItem,
            EnumVariantList,
            EnumVariant,
            FieldDeclarationList,
            FieldDeclaration,
            OrderedFieldDeclarationList,
            ExternCrateDeclaration,
            ConstItem,
            StaticItem,
            TypeItem,
            FunctionItem,
            FunctionSignatureItem,
            FunctionModifiers,
            WhereClause,
            WherePredicate,
            ImplItem,
            TraitItem,
            AssociatedType,
            TraitBounds,
            HigherRankedTraitBound,
            RemovedTraitBound,
            TypeParameters,
            ConstParameter,
            TypeParameter,
            LifetimeParameter,
            LetDeclaration,
            UseDeclaration,
            ScopedUseList,
            UseList,
            UseAsClause,
            UseWildcard,
            Parameters,
            SelfParameter,
            VariadicParameter,
            Parameter,
            ExternModifier,
            VisibilityModifier,
            _Type,
            BracketedType,
            QualifiedType,
            ArrayType,
            ForLifetimes,
            FunctionType,
            TupleType,
            UnitType,
            GenericFunction,
            GenericType,
            GenericTypeWithTurbofish,
            BoundedType,
            UseBounds,
            TypeArguments,
            TypeBinding,
            ReferenceType,
            PointerType,
            NeverType,
            AbstractType,
            DynamicType,
            _Expression,
            MacroInvocation,
            TokenTree_,
            ScopedIdentifier,
            ScopedTypeIdentifier,
            ScopedTypeIdentifier_,
            RangeExpression,
            UnaryExpression,
            TryExpression,
            ReferenceExpression,
            BinaryExpression,
            AssignmentExpression,
            CompoundAssignmentExpr,
            TypeCastExpression,
            ReturnExpression,
            YieldExpression,
            CallExpression,
            Arguments,
            ArrayExpression,
            ParenthesizedExpression,
            TupleExpression,
            UnitExpression,
            StructExpression,
            FieldInitializerList,
            ShorthandFieldInitializer,
            FieldInitializer,
            BaseFieldInitializer,
            IfExpression,
            LetCondition,
            ElseClause,
            MatchExpression,
            MatchBlock,
            MatchArm,
            MatchArm_,
            MatchPattern,
            WhileExpression,
            LoopExpression,
            ForExpression,
            ConstBlock,
            ClosureExpression,
            ClosureParameters,
            Label,
            BreakExpression,
            ContinueExpression,
            IndexExpression,
            AwaitExpression,
            FieldExpression,
            UnsafeBlock,
            AsyncBlock,
            GenBlock,
            TryBlock,
            _Pattern,
            GenericPattern,
            TuplePattern,
            SlicePattern,
            TupleStructPattern,
            StructPattern,
            FieldPattern,
            RemainingFieldPattern,
            MutPattern,
            RangePattern,
            RefPattern,
            CapturedPattern,
            ReferencePattern,
            OrPattern,
            _Literal,
            _LiteralPattern,
            NegativeLiteral,
            StringLiteral,
            RawStringLiteral,
            BooleanLiteral,
            LineComment,
            InnerDocCommentMarker_,
            OuterDocCommentMarker_,
            BlockComment,
            FieldIdentifier,
            LetChain,
            ShorthandFieldIdentifier,
            TypeIdentifier,
        )
    }
}
