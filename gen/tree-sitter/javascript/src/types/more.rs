//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use super::Type;
impl Type {
    pub fn is_repeat(&self) -> bool {
        is!(
            self,
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
