// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_c --example regen_c_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Identifier = "identifier";
    HashInclude = "#include";
    PreprocIncludeToken2 = "preproc_include_token2";
    HashDefine = "#define";
    LParen = "(";
    DotDotDot = "...";
    Comma = ",";
    RParen = ")";
    HashIf = "#if";
    NewLine = "\n";
    HashEndif = "#endif";
    HashIfdef = "#ifdef";
    HashIfndef = "#ifndef";
    HashElse = "#else";
    HashElif = "#elif";
    HashElifdef = "#elifdef";
    HashElifndef = "#elifndef";
    PreprocArg = "preproc_arg";
    PreprocDirective = "preproc_directive";
    LParen_ = "(";
    Defined = "defined";
    Bang = "!";
    Tilde = "~";
    Dash = "-";
    Plus = "+";
    Star = "*";
    Slash = "/";
    Percent = "%";
    PipePipe = "||";
    AmpAmp = "&&";
    Pipe = "|";
    Caret = "^";
    Amp = "&";
    EqEq = "==";
    BangEq = "!=";
    GT = ">";
    GTEq = ">=";
    LTEq = "<=";
    LT = "<";
    LtLt = "<<";
    GtGt = ">>";
    SemiColon = ";";
    TS0 = "__extension__";
    Typedef = "typedef";
    Extern = "extern";
    TS1 = "__attribute__";
    __Attribute = "__attribute";
    ColonColon = "::";
    TS2 = "[[";
    TS3 = "]]";
    __Declspec = "__declspec";
    __Based = "__based";
    __Cdecl = "__cdecl";
    __Clrcall = "__clrcall";
    __Stdcall = "__stdcall";
    __Fastcall = "__fastcall";
    __Thiscall = "__thiscall";
    __Vectorcall = "__vectorcall";
    MsRestrictModifier = "ms_restrict_modifier";
    MsUnsignedPtrModifier = "ms_unsigned_ptr_modifier";
    MsSignedPtrModifier = "ms_signed_ptr_modifier";
    _Unaligned = "_unaligned";
    __Unaligned = "__unaligned";
    LBrace = "{";
    RBrace = "}";
    Signed = "signed";
    Unsigned = "unsigned";
    Long = "long";
    Short = "short";
    LBracket = "[";
    Static = "static";
    RBracket = "]";
    Eq = "=";
    Auto = "auto";
    Register = "register";
    Inline = "inline";
    __Inline = "__inline";
    TS4 = "__inline__";
    __Forceinline = "__forceinline";
    ThreadLocal = "thread_local";
    __Thread = "__thread";
    Const = "const";
    Constexpr = "constexpr";
    Volatile = "volatile";
    Restrict = "restrict";
    TS5 = "__restrict__";
    TS6 = "_Atomic";
    TS7 = "_Noreturn";
    Noreturn = "noreturn";
    TS8 = "_Nonnull";
    Alignas = "alignas";
    TS9 = "_Alignas";
    PrimitiveType = "primitive_type";
    Enum = "enum";
    Colon = ":";
    Struct = "struct";
    Union = "union";
    If
    = "if";
    Else = "else";
    Switch = "switch";
    Case = "case";
    Default = "default";
    While = "while";
    Do = "do";
    For = "for";
    Return = "return";
    Break = "break";
    Continue = "continue";
    Goto = "goto";
    __Try = "__try";
    __Except = "__except";
    __Finally = "__finally";
    __Leave = "__leave";
    QMark = "?";
    StarEq = "*=";
    SlashEq = "/=";
    PercentEq = "%=";
    PlusEq = "+=";
    DashEq = "-=";
    LtLtEq = "<<=";
    GtGtEq = ">>=";
    AmpEq
    = "&=";
    CaretEq = "^=";
    PipeEq = "|=";
    DashDash = "--";
    PlusPlus = "++";
    Sizeof = "sizeof";
    TS10 = "__alignof__";
    __Alignof = "__alignof";
    _Alignof = "_alignof";
    Alignof = "alignof";
    TS11 = "_Alignof";
    Offsetof = "offsetof";
    TS12 = "_Generic";
    Asm
    = "asm";
    TS13 = "__asm__";
    __Asm = "__asm";
    TS14 = "__volatile__";
    Dot = ".";
    DashGt
    = "->";
    NumberLiteral = "number_literal";
    TS15 = "L'";
    TS16 = "u'";
    TS17 = "U'";
    TS18
    = "u8'";
    SQuote = "'";
    Character = "character";
    TS19 = "L\"";
    TS20 = "u\"";
    TS21 = "U\"";
    TS22 = "u8\"";
    DQuote = "\"";
    StringContent = "string_content";
    EscapeSequence
    = "escape_sequence";
    SystemLibString = "system_lib_string";
    True = "true";
    False = "false";
    TS23 = "NULL";
    Nullptr = "nullptr";
    Comment = "comment";
    TranslationUnit = "translation_unit";
    _TopLevelItem = "_top_level_item";
    _BlockItem = "_block_item";
    PreprocInclude = "preproc_include";
    PreprocDef = "preproc_def";
    PreprocFunctionDef = "preproc_function_def";
    PreprocParams = "preproc_params";
    PreprocCall = "preproc_call";
    PreprocIf = "preproc_if";
    PreprocIfdef = "preproc_ifdef";
    PreprocElse
    = "preproc_else";
    PreprocElif = "preproc_elif";
    PreprocElifdef = "preproc_elifdef";
    PreprocIf_ = "preproc_if";
    PreprocIfdef_ = "preproc_ifdef";
    PreprocElse_ = "preproc_else";
    PreprocElif_ = "preproc_elif";
    PreprocElifdef_ = "preproc_elifdef";
    PreprocIf__ = "preproc_if";
    PreprocIfdef__ = "preproc_ifdef";
    PreprocElse__ = "preproc_else";
    PreprocElif__ = "preproc_elif";
    PreprocElifdef__ = "preproc_elifdef";
    PreprocIf___ = "preproc_if";
    PreprocIfdef___ = "preproc_ifdef";
    PreprocElse___ = "preproc_else";
    PreprocElif___ = "preproc_elif";
    PreprocElifdef___ = "preproc_elifdef";
    _PreprocExpression = "_preproc_expression";
    ParenthesizedExpression = "parenthesized_expression";
    PreprocDefined = "preproc_defined";
    UnaryExpression = "unary_expression";
    CallExpression = "call_expression";
    ArgumentList = "argument_list";
    BinaryExpression = "binary_expression";
    FunctionDefinition = "function_definition";
    FunctionDefinition_
    = "function_definition";
    Declaration = "declaration";
    TypeDefinition = "type_definition";
    _TypeDefinitionType = "_type_definition_type";
    _TypeDefinitionDeclarators = "_type_definition_declarators";
    _DeclarationModifiers = "_declaration_modifiers";
    _DeclarationSpecifiers = "_declaration_specifiers";
    LinkageSpecification = "linkage_specification";
    AttributeSpecifier = "attribute_specifier";
    Attribute = "attribute";
    AttributeDeclaration = "attribute_declaration";
    MsDeclspecModifier = "ms_declspec_modifier";
    MsBasedModifier
    = "ms_based_modifier";
    MsCallModifier = "ms_call_modifier";
    MsUnalignedPtrModifier = "ms_unaligned_ptr_modifier";
    MsPointerModifier = "ms_pointer_modifier";
    DeclarationList = "declaration_list";
    _Declarator = "_declarator";
    _DeclarationDeclarator = "_declaration_declarator";
    _FieldDeclarator = "_field_declarator";
    _TypeDeclarator = "_type_declarator";
    _AbstractDeclarator = "_abstract_declarator";
    ParenthesizedDeclarator = "parenthesized_declarator";
    ParenthesizedDeclarator_ = "parenthesized_declarator";
    ParenthesizedDeclarator__ = "parenthesized_declarator";
    AbstractParenthesizedDeclarator = "abstract_parenthesized_declarator";
    AttributedDeclarator = "attributed_declarator";
    AttributedDeclarator_ = "attributed_declarator";
    AttributedDeclarator__ = "attributed_declarator";
    PointerDeclarator = "pointer_declarator";
    PointerDeclarator_
    = "pointer_declarator";
    PointerDeclarator__ = "pointer_declarator";
    AbstractPointerDeclarator = "abstract_pointer_declarator";
    FunctionDeclarator = "function_declarator";
    FunctionDeclarator_ = "function_declarator";
    FunctionDeclarator__ = "function_declarator";
    FunctionDeclarator___ = "function_declarator";
    AbstractFunctionDeclarator = "abstract_function_declarator";
    FunctionDeclarator____ = "function_declarator";
    ArrayDeclarator = "array_declarator";
    ArrayDeclarator_ = "array_declarator";
    ArrayDeclarator__ = "array_declarator";
    AbstractArrayDeclarator = "abstract_array_declarator";
    InitDeclarator = "init_declarator";
    CompoundStatement = "compound_statement";
    StorageClassSpecifier = "storage_class_specifier";
    TypeQualifier = "type_qualifier";
    AlignasQualifier = "alignas_qualifier";
    TypeSpecifier = "type_specifier";
    SizedTypeSpecifier = "sized_type_specifier";
    EnumSpecifier = "enum_specifier";
    EnumeratorList = "enumerator_list";
    StructSpecifier = "struct_specifier";
    UnionSpecifier = "union_specifier";
    FieldDeclarationList = "field_declaration_list";
    _FieldDeclarationListItem = "_field_declaration_list_item";
    FieldDeclaration = "field_declaration";
    _FieldDeclarationDeclarator = "_field_declaration_declarator";
    BitfieldClause = "bitfield_clause";
    Enumerator = "enumerator";
    VariadicParameter = "variadic_parameter";
    ParameterList = "parameter_list";
    ParameterList_ = "parameter_list";
    ParameterDeclaration = "parameter_declaration";
    AttributedStatement
    = "attributed_statement";
    Statement = "statement";
    _TopLevelStatement = "_top_level_statement";
    LabeledStatement = "labeled_statement";
    ExpressionStatement = "expression_statement";
    ExpressionStatement_ = "expression_statement";
    IfStatement = "if_statement";
    ElseClause = "else_clause";
    SwitchStatement = "switch_statement";
    CaseStatement = "case_statement";
    WhileStatement = "while_statement";
    DoStatement = "do_statement";
    ForStatement = "for_statement";
    _ForStatementBody = "_for_statement_body";
    ReturnStatement = "return_statement";
    BreakStatement = "break_statement";
    ContinueStatement = "continue_statement";
    GotoStatement = "goto_statement";
    SehTryStatement = "seh_try_statement";
    SehExceptClause = "seh_except_clause";
    SehFinallyClause = "seh_finally_clause";
    SehLeaveStatement = "seh_leave_statement";
    Expression = "expression";
    _String = "_string";
    CommaExpression = "comma_expression";
    ConditionalExpression = "conditional_expression";
    AssignmentExpression = "assignment_expression";
    PointerExpression = "pointer_expression";
    UnaryExpression_ = "unary_expression";
    BinaryExpression_ = "binary_expression";
    UpdateExpression = "update_expression";
    CastExpression = "cast_expression";
    TypeDescriptor = "type_descriptor";
    SizeofExpression = "sizeof_expression";
    AlignofExpression = "alignof_expression";
    OffsetofExpression = "offsetof_expression";
    GenericExpression = "generic_expression";
    SubscriptExpression = "subscript_expression";
    CallExpression_ = "call_expression";
    GnuAsmExpression = "gnu_asm_expression";
    GnuAsmQualifier = "gnu_asm_qualifier";
    GnuAsmOutputOperandList = "gnu_asm_output_operand_list";
    GnuAsmOutputOperand = "gnu_asm_output_operand";
    GnuAsmInputOperandList = "gnu_asm_input_operand_list";
    GnuAsmInputOperand = "gnu_asm_input_operand";
    GnuAsmClobberList = "gnu_asm_clobber_list";
    GnuAsmGotoList = "gnu_asm_goto_list";
    ExtensionExpression = "extension_expression";
    ArgumentList_ = "argument_list";
    FieldExpression = "field_expression";
    CompoundLiteralExpression = "compound_literal_expression";
    ParenthesizedExpression_ = "parenthesized_expression";
    InitializerList = "initializer_list";
    InitializerPair = "initializer_pair";
    SubscriptDesignator = "subscript_designator";
    SubscriptRangeDesignator = "subscript_range_designator";
    FieldDesignator = "field_designator";
    CharLiteral = "char_literal";
    ConcatenatedString = "concatenated_string";
    StringLiteral = "string_literal";
    Null = "null";
    _EmptyDeclaration = "_empty_declaration";
    MacroTypeSpecifier = "macro_type_specifier";
    TranslationUnitRepeat1 = "translation_unit_repeat1";
    PreprocParamsRepeat1 = "preproc_params_repeat1";
    PreprocIfRepeat1 = "preproc_if_repeat1";
    PreprocIfInFieldDeclarationListRepeat1 = "preproc_if_in_field_declaration_list_repeat1";
    PreprocIfInEnumeratorListRepeat1 = "preproc_if_in_enumerator_list_repeat1";
    PreprocIfInEnumeratorListNoCommaRepeat1 = "preproc_if_in_enumerator_list_no_comma_repeat1";
    PreprocArgumentListRepeat1 = "preproc_argument_list_repeat1";
    _OldStyleFunctionDefinitionRepeat1 = "_old_style_function_definition_repeat1";
    DeclarationRepeat1 = "declaration_repeat1";
    TypeDefinitionRepeat1 = "type_definition_repeat1";
    _TypeDefinitionTypeRepeat1 = "_type_definition_type_repeat1";
    _TypeDefinitionDeclaratorsRepeat1 = "_type_definition_declarators_repeat1";
    _DeclarationSpecifiersRepeat1 = "_declaration_specifiers_repeat1";
    AttributeDeclarationRepeat1 = "attribute_declaration_repeat1";
    AttributedDeclaratorRepeat1 = "attributed_declarator_repeat1";
    PointerDeclaratorRepeat1 = "pointer_declarator_repeat1";
    FunctionDeclaratorRepeat1 = "function_declarator_repeat1";
    ArrayDeclaratorRepeat1 = "array_declarator_repeat1";
    SizedTypeSpecifierRepeat1 = "sized_type_specifier_repeat1";
    EnumeratorListRepeat1 = "enumerator_list_repeat1";
    _FieldDeclarationDeclaratorRepeat1 = "_field_declaration_declarator_repeat1";
    ParameterListRepeat1 = "parameter_list_repeat1";
    _OldStyleParameterListRepeat1 = "_old_style_parameter_list_repeat1";
    CaseStatementRepeat1 = "case_statement_repeat1";
    GenericExpressionRepeat1 = "generic_expression_repeat1";
    GnuAsmExpressionRepeat1 = "gnu_asm_expression_repeat1";
    GnuAsmOutputOperandListRepeat1 = "gnu_asm_output_operand_list_repeat1";
    GnuAsmInputOperandListRepeat1 = "gnu_asm_input_operand_list_repeat1";
    GnuAsmClobberListRepeat1 = "gnu_asm_clobber_list_repeat1";
    GnuAsmGotoListRepeat1 = "gnu_asm_goto_list_repeat1";
    ArgumentListRepeat1 = "argument_list_repeat1";
    InitializerListRepeat1 = "initializer_list_repeat1";
    InitializerPairRepeat1 = "initializer_pair_repeat1";
    CharLiteralRepeat1 = "char_literal_repeat1";
    ConcatenatedStringRepeat1 = "concatenated_string_repeat1";
    StringLiteralRepeat1 = "string_literal_repeat1";
    FieldIdentifier = "field_identifier";
    StatementIdentifier = "statement_identifier";
    TypeIdentifier = "type_identifier";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self, End, PreprocIncludeToken2, _TopLevelItem, _BlockItem,
            _PreprocExpression, _TypeDefinitionType, _TypeDefinitionDeclarators,
            _DeclarationModifiers, _DeclarationSpecifiers, _Declarator,
            _DeclarationDeclarator, _FieldDeclarator, _TypeDeclarator,
            _AbstractDeclarator, TypeSpecifier, _FieldDeclarationListItem,
            _FieldDeclarationDeclarator, Statement, _TopLevelStatement,
            _ForStatementBody, Expression, _String, _EmptyDeclaration,
            TranslationUnitRepeat1, PreprocParamsRepeat1, PreprocIfRepeat1,
            PreprocIfInFieldDeclarationListRepeat1, PreprocIfInEnumeratorListRepeat1,
            PreprocIfInEnumeratorListNoCommaRepeat1, PreprocArgumentListRepeat1,
            _OldStyleFunctionDefinitionRepeat1, DeclarationRepeat1,
            TypeDefinitionRepeat1, _TypeDefinitionTypeRepeat1,
            _TypeDefinitionDeclaratorsRepeat1, _DeclarationSpecifiersRepeat1,
            AttributeDeclarationRepeat1, AttributedDeclaratorRepeat1,
            PointerDeclaratorRepeat1, FunctionDeclaratorRepeat1, ArrayDeclaratorRepeat1,
            SizedTypeSpecifierRepeat1, EnumeratorListRepeat1,
            _FieldDeclarationDeclaratorRepeat1, ParameterListRepeat1,
            _OldStyleParameterListRepeat1, CaseStatementRepeat1,
            GenericExpressionRepeat1, GnuAsmExpressionRepeat1,
            GnuAsmOutputOperandListRepeat1, GnuAsmInputOperandListRepeat1,
            GnuAsmClobberListRepeat1, GnuAsmGotoListRepeat1, ArgumentListRepeat1,
            InitializerListRepeat1, InitializerPairRepeat1, CharLiteralRepeat1,
            ConcatenatedStringRepeat1, StringLiteralRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(
            self, _Declarator, _FieldDeclarator, _TypeDeclarator, _AbstractDeclarator,
            TypeSpecifier, Statement, Expression,
        )
    }
    pub fn is_named(&self) -> bool {
        is!(
            self, Identifier, PreprocArg, PreprocDirective, MsRestrictModifier,
            MsUnsignedPtrModifier, MsSignedPtrModifier, PrimitiveType, NumberLiteral,
            Character, StringContent, EscapeSequence, SystemLibString, True, False,
            Comment, TranslationUnit, PreprocInclude, PreprocDef, PreprocFunctionDef,
            PreprocParams, PreprocCall, PreprocIf, PreprocIfdef, PreprocElse,
            PreprocElif, PreprocElifdef, PreprocIf_, PreprocIfdef_, PreprocElse_,
            PreprocElif_, PreprocElifdef_, PreprocIf__, PreprocIfdef__, PreprocElse__,
            PreprocElif__, PreprocElifdef__, PreprocIf___, PreprocIfdef___,
            PreprocElse___, PreprocElif___, PreprocElifdef___, ParenthesizedExpression,
            PreprocDefined, UnaryExpression, CallExpression, ArgumentList,
            BinaryExpression, FunctionDefinition, FunctionDefinition_, Declaration,
            TypeDefinition, LinkageSpecification, AttributeSpecifier, Attribute,
            AttributeDeclaration, MsDeclspecModifier, MsBasedModifier, MsCallModifier,
            MsUnalignedPtrModifier, MsPointerModifier, DeclarationList, _Declarator,
            _FieldDeclarator, _TypeDeclarator, _AbstractDeclarator,
            ParenthesizedDeclarator, ParenthesizedDeclarator_, ParenthesizedDeclarator__,
            AbstractParenthesizedDeclarator, AttributedDeclarator, AttributedDeclarator_,
            AttributedDeclarator__, PointerDeclarator, PointerDeclarator_,
            PointerDeclarator__, AbstractPointerDeclarator, FunctionDeclarator,
            FunctionDeclarator_, FunctionDeclarator__, FunctionDeclarator___,
            AbstractFunctionDeclarator, FunctionDeclarator____, ArrayDeclarator,
            ArrayDeclarator_, ArrayDeclarator__, AbstractArrayDeclarator, InitDeclarator,
            CompoundStatement, StorageClassSpecifier, TypeQualifier, AlignasQualifier,
            TypeSpecifier, SizedTypeSpecifier, EnumSpecifier, EnumeratorList,
            StructSpecifier, UnionSpecifier, FieldDeclarationList, FieldDeclaration,
            BitfieldClause, Enumerator, VariadicParameter, ParameterList, ParameterList_,
            ParameterDeclaration, AttributedStatement, Statement, LabeledStatement,
            ExpressionStatement, ExpressionStatement_, IfStatement, ElseClause,
            SwitchStatement, CaseStatement, WhileStatement, DoStatement, ForStatement,
            ReturnStatement, BreakStatement, ContinueStatement, GotoStatement,
            SehTryStatement, SehExceptClause, SehFinallyClause, SehLeaveStatement,
            Expression, CommaExpression, ConditionalExpression, AssignmentExpression,
            PointerExpression, UnaryExpression_, BinaryExpression_, UpdateExpression,
            CastExpression, TypeDescriptor, SizeofExpression, AlignofExpression,
            OffsetofExpression, GenericExpression, SubscriptExpression, CallExpression_,
            GnuAsmExpression, GnuAsmQualifier, GnuAsmOutputOperandList,
            GnuAsmOutputOperand, GnuAsmInputOperandList, GnuAsmInputOperand,
            GnuAsmClobberList, GnuAsmGotoList, ExtensionExpression, ArgumentList_,
            FieldExpression, CompoundLiteralExpression, ParenthesizedExpression_,
            InitializerList, InitializerPair, SubscriptDesignator,
            SubscriptRangeDesignator, FieldDesignator, CharLiteral, ConcatenatedString,
            StringLiteral, Null, MacroTypeSpecifier, FieldIdentifier,
            StatementIdentifier, TypeIdentifier,
        )
    }
}
