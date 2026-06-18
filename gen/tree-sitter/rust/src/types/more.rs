//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use super::Type;
impl Type {
    pub fn is_repeat(&self) -> bool {
        is!(
            self,
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
}

macro_rules! is {
    ($e:expr, $($p:ident $(if $guard:expr)?, )*) => {
        match $e {$(
            Type::$p $(if $guard)? => true,)*
            _ => false
        }
    };
}
pub(super) use is;
