// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_tsquery --example regen_tsquery_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Dot = ".";
    DQuote = "\"";
    _StringToken1 = "_string_token1";
    EscapeSequence = "escape_sequence";
    Star = "*";
    Plus = "+";
    QMark = "?";
    Identifier = "identifier";
    Identifier_ = "identifier";
    Inderscore = "_";
    At = "@";
    Comment = "comment";
    LBracket = "[";
    RBracket = "]";
    LParen = "(";
    RParen = ")";
    Slash = "/";
    Colon = ":";
    Bang = "!";
    Sharp = "#";
    PredicateType = "predicate_type";
    Program = "program";
    _Definition = "_definition";
    _GroupExpression = "_group_expression";
    _NamedNodeExpression = "_named_node_expression";
    _String = "_string";
    Quantifier = "quantifier";
    _ImmediateIdentifier = "_immediate_identifier";
    _NodeIdentifier = "_node_identifier";
    Capture = "capture";
    String = "string";
    Parameters = "parameters";
    List = "list";
    Grouping = "grouping";
    AnonymousNode = "anonymous_node";
    NamedNode = "named_node";
    _FieldName = "_field_name";
    FieldDefinition = "field_definition";
    NegatedField = "negated_field";
    Predicate = "predicate";
    ProgramRepeat1 = "program_repeat1";
    _StringRepeat1 = "_string_repeat1";
    ParametersRepeat1 = "parameters_repeat1";
    ListRepeat1 = "list_repeat1";
    GroupingRepeat1 = "grouping_repeat1";
    NamedNodeRepeat1 = "named_node_repeat1";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self,
            End,
            _StringToken1,
            _Definition,
            _GroupExpression,
            _NamedNodeExpression,
            _String,
            _ImmediateIdentifier,
            _NodeIdentifier,
            _FieldName,
            ProgramRepeat1,
            _StringRepeat1,
            ParametersRepeat1,
            ListRepeat1,
            GroupingRepeat1,
            NamedNodeRepeat1,
        )
    }
    pub fn is_supertype(&self) -> bool {
        is!(self,)
    }
    pub fn is_named(&self) -> bool {
        is!(
            self,
            EscapeSequence,
            Identifier,
            Identifier_,
            Comment,
            PredicateType,
            Program,
            Quantifier,
            Capture,
            String,
            Parameters,
            List,
            Grouping,
            AnonymousNode,
            NamedNode,
            FieldDefinition,
            NegatedField,
            Predicate,
        )
    }
}
