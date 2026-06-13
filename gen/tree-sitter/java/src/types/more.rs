//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use super::Type;
use hyperast::types::TypeTrait;

impl Type {
    pub(crate) fn is_error(&self) -> bool {
        self == &Self::ERROR || self == &Self::_ERROR
    }
    pub(crate) fn is_syntax(&self) -> bool {
        is!(
            self,
            LParen, // "(",
            Amp,    // "&",
            RParen, // ")",
            Eq,     // "=",
            // PlusEq, // "+=",
            // DashEq, // "-=",
            // StarEq, // "*=",
            // SlashEq, // "/=",
            // AmpEq, // "&=",
            // PipeEq, // "|=",
            // CaretEq, // "^=",
            // PercentEq, // "%=",
            // LtLtEq, // "<<=",
            // GtGtEq, // ">>=",
            // GtGtGtEq, // ">>>=",
            // GT, // ">",
            // LT, // "<",
            // GTEq, // ">=",
            // LTEq, // "<=",
            // EqEq, // "==",
            // BangEq, // "!=",
            // AmpAmp, // "&&",
            // PipePipe, // "||",
            // Plus, // "+",
            // Dash, // "-",
            // Star, // "*",
            // Slash, // "/",
            // Pipe, // "|",
            // Caret, // "^",
            // Percent, // "%",
            // LtLt, // "<<",
            // GtGt, // ">>",
            // GtGtGt, // ">>>",
            // Instanceof, // "instanceof",
            // DashGt, // "->",
            Comma, // ",",
            QMark, // "?",
            Colon, // ":",
            // Bang, // "!",
            // Tilde, // "~",
            // PlusPlus, // "++",
            // DashDash, // "--",
            // New, // "new",
            LBracket, // "[",
            RBracket, // "]",
            Dot,      // ".",
            // Class, // "class",
            ColonColon, // "::",
            // Extends, // "extends",
            Switch, // "switch",
            LBrace, // "{",
            RBrace, // "}",
            // Case, // "case",
            // Default, // "default",
            SemiColon, // ";",
            Assert,    // "assert",
            Do,        // "do",
            While,     // "while",
            // Break, // "break",
            // Continue, // "continue",
            Return,       // "return",
            Yield,        // "yield",
            Synchronized, // "synchronized",
            Throw,        // "throw",
            Try,          // "try",
            // Catch, // "catch",
            // Finally, // "finally",
            If, // "if",
            Else,
            // "else",
            // For, // "for",
            // At, // "@",
            // Open, // "open",
            // Module, // "module",
            // Requires, // "requires",
            // Exports, // "exports",
            // To, // "to",
            // Opens, // "opens",
            // Uses, // "uses",
            // Provides, // "provides",
            // With, // "with",
            // Transitive, // "transitive",
            // Static, // "static",
            // Package, // "package",
            // Import, // "import",
            // Enum, // "enum",
            // Public, // "public",
            // Protected, // "protected",
            // Private, // "private",
            // Abstract, // "abstract",
            // Final, // "final",
            // Strictfp, // "strictfp",
            // Native, // "native",
            // Transient, // "transient",
            // Volatile, // "volatile",
            // Implements, // "implements",
            // Record, // "record",
            // TS0, // "@interface",
            // Interface, // "interface",
            // Byte, // "byte",
            // Short, // "short",
            // Int, // "int",
            // Long, // "long",
            // Char, // "char",
            // Float, // "float",
            // Double, // "double",
            // BooleanType, // "boolean_type",
            // VoidType, // "void_type",
            // DotDotDot, // "...",
            // Throws, // "throws",
            // This, // "this",
            // Super, // "super",
        )
    }

    pub(crate) fn is_member(&self) -> bool {
        self.is_value_member() || self.is_executable_member() || self.is_type_declaration()
    }

    pub fn literal_type(&self) -> &str {
        // TODO make the difference btw int/long and float/double
        ty!(self,
            _Literal => panic!(),
            True => "boolean",
            False => "boolean",
            OctalIntegerLiteral => "int",
            BinaryIntegerLiteral => "int",
            DecimalIntegerLiteral => "int",
            HexFloatingPointLiteral => "float",
            DecimalFloatingPointLiteral => "float",
            HexIntegerLiteral => "float",
            // ClassLiteral => "class",
            StringLiteral => "String",
            CharacterLiteral => "char",
            NullLiteral => "null",
            _ => panic!(),
        )
    }

    pub(crate) fn is_repeat(&self) -> bool {
        is!(
            self,
            ProgramRepeat1,
            _StringLiteralRepeat1,
            _MultilineStringLiteralRepeat1,
            CastExpressionRepeat1,
            InferredParametersRepeat1,
            ArrayCreationExpressionRepeat1,
            ArgumentListRepeat1,
            TypeArgumentsRepeat1,
            DimensionsRepeat1,
            SwitchBlockRepeat1,
            SwitchBlockStatementGroupRepeat1,
            RecordPatternBodyRepeat1,
            TryStatementRepeat1,
            CatchTypeRepeat1,
            ResourceSpecificationRepeat1,
            ForStatementRepeat1,
            AnnotationArgumentListRepeat1,
            ElementValueArrayInitializerRepeat1,
            ModuleBodyRepeat1,
            RequiresModuleDirectiveRepeat1,
            ExportsModuleDirectiveRepeat1,
            ProvidesModuleDirectiveRepeat1,
            EnumBodyRepeat1,
            EnumBodyDeclarationsRepeat1,
            ModifiersRepeat1,
            TypeParametersRepeat1,
            TypeBoundRepeat1,
            TypeListRepeat1,
            AnnotationTypeBodyRepeat1,
            InterfaceBodyRepeat1,
            _VariableDeclaratorListRepeat1,
            ArrayInitializerRepeat1,
            FormalParametersRepeat1,
            ReceiverParameterRepeat1,
            ArrayCreationExpressionRepeat2,
            SwitchBlockRepeat2,
            SwitchBlockStatementGroupRepeat2,
            ForStatementRepeat2,
            _MultilineStringFragmentToken1,
            _MultilineStringFragmentToken2,
            _EscapeSequenceToken1,
        )
    }
}

impl TypeTrait for Type {
    type Lang = super::Java;

    fn is_fork(&self) -> bool {
        is!(
            self,
            TernaryExpression,
            IfStatement,
            ForStatement,
            EnhancedForStatement,
            WhileStatement,
            CatchClause,
            SwitchLabel,
            TryStatement,
            TryWithResourcesStatement,
            DoStatement,
        )
    }

    fn is_literal(&self) -> bool {
        is!(
            self,
            _Literal,
            True,
            False,
            OctalIntegerLiteral,
            BinaryIntegerLiteral,
            DecimalIntegerLiteral,
            HexFloatingPointLiteral,
            DecimalFloatingPointLiteral,
            ClassLiteral,
            StringLiteral,
            CharacterLiteral,
            HexIntegerLiteral,
            NullLiteral,
        )
    }
    fn is_primitive(&self) -> bool {
        is!(self, BooleanType, VoidType, FloatingPointType, IntegralType,)
    }
    fn is_type_declaration(&self) -> bool {
        is!(
            self,
            ClassDeclaration,
            EnumDeclaration,
            InterfaceDeclaration,
            AnnotationTypeDeclaration,
            EnumConstant, // TODO need more eval
        )
    }
    // fn primitive_to_str(&self) -> &str {
    //     match self {
    //         Self::BooleanType => "boolean",
    //         Self::VoidType => "void",
    //         Self::FloatingPointType => "float",
    //         Self::IntegralType => "int",
    //         _ => panic!(),
    //     }
    // }
    fn is_identifier(&self) -> bool {
        is!(
            self,
            Identifier,
            TypeIdentifier,
            ScopedIdentifier,
            ScopedTypeIdentifier,
        )
    }
    fn is_instance_ref(&self) -> bool {
        is!(self, This, Super,)
    }

    fn is_type_body(&self) -> bool {
        is!(
            self,
            ClassBody,
            InterfaceBody,
            AnnotationTypeBody,
            EnumBody,
            EnumBodyDeclarations,
        )
    }

    fn is_value_member(&self) -> bool {
        is!(
            self,
            FieldDeclaration,
            ConstantDeclaration,
            // EnumConstant,
            AnnotationTypeElementDeclaration,
        )
    }

    fn is_executable_member(&self) -> bool {
        self == &Type::MethodDeclaration || self == &Type::ConstructorDeclaration
    }

    fn is_statement(&self) -> bool {
        self.is_declarative_statement()
            || self.is_structural_statement()
            || self.is_simple_statement()
            || self.is_block_related()
    }

    fn is_declarative_statement(&self) -> bool {
        self == &Type::LocalVariableDeclaration
            || self == &Type::TryWithResourcesStatement
            || self == &Type::CatchClause
            || self == &Type::ForStatement
            || self == &Type::EnhancedForStatement
    }

    fn is_structural_statement(&self) -> bool {
        self == &Type::SwitchExpression
            || self == &Type::WhileStatement
            || self == &Type::DoStatement
            || self == &Type::IfStatement
            || self == &Type::TryStatement
            || self == &Type::FinallyClause
            || self == &Type::TryWithResourcesStatement
    }

    fn is_block_related(&self) -> bool {
        self == &Type::StaticInitializer
            || self == &Type::ConstructorBody
            || self == &Type::Block
            || self == &Type::SwitchBlock
            || self == &Type::SwitchBlockStatementGroup
    }

    fn is_simple_statement(&self) -> bool {
        self == &Type::ExpressionStatement
            || self == &Type::AssertStatement
            || self == &Type::ThrowStatement
            || self == &Type::ReturnStatement
            || self == &Type::LabeledStatement
            || self == &Type::SynchronizedStatement
            || self == &Type::ContinueStatement
            || self == &Type::BreakStatement
            || self == &Type::SynchronizedStatement
    }

    fn is_local_declare(&self) -> bool {
        self == &Type::LocalVariableDeclaration
        // || self == &Type::EnhancedForVariable // TODO trick used to group nodes
        || self == &Type::Resource
    }

    fn is_parameter(&self) -> bool {
        self == &Type::Resource
            || self == &Type::FormalParameter
            || self == &Type::SpreadParameter
            || self == &Type::CatchFormalParameter
            || self == &Type::TypeParameter
    }

    fn is_parameter_list(&self) -> bool {
        is!(
            self,
            ResourceSpecification,
            FormalParameters,
            TypeParameters,
        )
    }

    fn is_argument_list(&self) -> bool {
        is!(self, ArgumentList, TypeArguments, AnnotationArgumentList,)
    }

    fn is_expression(&self) -> bool {
        is!(
            self,
            TernaryExpression,
            BinaryExpression,
            UnaryExpression,
            AssignmentExpression,
            // VariableDeclarator,
            InstanceofExpression,
            ArrayCreationExpression,
            ObjectCreationExpression,
            LambdaExpression,
            CastExpression,
            UpdateExpression,
            ParenthesizedExpression,
            MethodInvocation,
            MethodReference,
            ExplicitConstructorInvocation,
            ClassLiteral,
            FieldAccess,
            ArrayAccess,
        )
    }
    fn is_comment(&self) -> bool {
        is!(self, LineComment, BlockComment,)
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
macro_rules! ty {
($e:expr,
    $([$g2:expr] => $v2:expr, )*
    $($p:ident => $v:expr,)*
    $(_ => $d:expr,)?
) => {
    match $e {
        $(_ if $g2 => $v2,)*
        $(Type::$p => $v,)*
        $(_ => $d)?
    }
}
}
pub(super) use ty;
