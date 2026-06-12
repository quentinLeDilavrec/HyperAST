//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use super::Type;

impl Type {
    pub fn is_hidden(&self) -> bool {
        match self {
            Type::End => true,
            Type::_StringToken1 => true,
            Type::_Definition => true,
            Type::_GroupExpression => true,
            Type::_NamedNodeExpression => true,
            Type::_String => true,
            Type::_ImmediateIdentifier => true,
            Type::_NodeIdentifier => true,
            Type::_FieldName => true,
            Type::ProgramRepeat1 => true,
            Type::_StringRepeat1 => true,
            Type::ParametersRepeat1 => true,
            Type::ListRepeat1 => true,
            Type::GroupingRepeat1 => true,
            Type::NamedNodeRepeat1 => true,
            _ => false,
        }
    }
    pub fn is_supertype(&self) -> bool {
        false
    }
    pub fn is_named(&self) -> bool {
        match self {
            Type::EscapeSequence => true,
            Type::Identifier => true,
            Type::Identifier_ => true,
            Type::Comment => true,
            Type::PredicateType => true,
            Type::Program => true,
            Type::Quantifier => true,
            Type::Capture => true,
            Type::String => true,
            Type::Parameters => true,
            Type::List => true,
            Type::Grouping => true,
            Type::AnonymousNode => true,
            Type::NamedNode => true,
            Type::FieldDefinition => true,
            Type::NegatedField => true,
            Type::Predicate => true,
            _ => false,
        }
    }
    pub(crate) fn is_repeat(&self) -> bool {
        *self == Type::ProgramRepeat1
            || *self == Type::_StringRepeat1
            || *self == Type::ParametersRepeat1
            || *self == Type::ListRepeat1
            || *self == Type::GroupingRepeat1
            || *self == Type::NamedNodeRepeat1
    }
}
