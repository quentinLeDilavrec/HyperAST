//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use super::Type;
impl Type {
    pub fn is_repeat(&self) -> bool {
        is!(
            self,
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
