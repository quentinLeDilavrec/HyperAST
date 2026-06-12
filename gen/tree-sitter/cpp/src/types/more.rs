//! More information about node types.
//! Long term they should be gathered from other sources automatically.

use crate::types::Type;

macro_rules! is {
    ($e:expr, $($p:ident $(if $guard:expr)?, )*) => {
        match $e {$(
            Type::$p $(if $guard)? => true,)*
            _ => false
        }
    };
}

impl Type {
    pub fn is_repeat(&self) -> bool {
        is!(
            self,
            TranslationUnitRepeat1,
            PreprocParamsRepeat1,
            PreprocIfRepeat1,
            PreprocIfInFieldDeclarationListRepeat1,
            PreprocIfInEnumeratorListRepeat1,
            PreprocIfInEnumeratorListNoCommaRepeat1,
            PreprocArgumentListRepeat1,
            DeclarationRepeat1,
            TypeDefinitionRepeat1,
            _TypeDefinitionTypeRepeat1,
            _TypeDefinitionDeclaratorsRepeat1,
            _DeclarationSpecifiersRepeat1,
            AttributeDeclarationRepeat1,
            AttributedDeclaratorRepeat1,
            PointerDeclaratorRepeat1,
            ArrayDeclaratorRepeat1,
            SizedTypeSpecifierRepeat1,
            EnumeratorListRepeat1,
            FieldDeclarationRepeat1,
            ParameterListRepeat1,
            CaseStatementRepeat1,
            GenericExpressionRepeat1,
            GnuAsmExpressionRepeat1,
            GnuAsmOutputOperandListRepeat1,
            GnuAsmInputOperandListRepeat1,
            GnuAsmClobberListRepeat1,
            GnuAsmGotoListRepeat1,
            ArgumentListRepeat1,
            InitializerListRepeat1,
            InitializerPairRepeat1,
            CharLiteralRepeat1,
            ConcatenatedStringRepeat1,
            StringLiteralRepeat1,
            _ClassDeclarationRepeat1,
            BaseClassClauseRepeat1,
            TemplateParameterListRepeat1,
            FieldInitializerListRepeat1,
            OperatorCastDefinitionRepeat1,
            ConstructorTryStatementRepeat1,
            StructuredBindingDeclaratorRepeat1,
            _FunctionPostfixRepeat1,
            ThrowSpecifierRepeat1,
            TemplateArgumentListRepeat1,
            SubscriptArgumentListRepeat1,
            RequirementSeqRepeat1,
            RequiresParameterListRepeat1,
            LambdaCaptureSpecifierRepeat1,
        )
    }

    pub(crate) fn is_syntax(&self) -> bool {
        is!(
            self,
            HashInclude, // "#include",
            NewLine,     // "\n",
            HashDefine,  // "#define",
            LParen,      // "(",
            DotDotDot,   // "...",
            Comma,       // ",",
            RParen,      // ")",
            // HashIf, // "#if",
            // HashEndif, // "#endif",
            // HashIfdef, // "#ifdef",
            // HashIfndef, // "#ifndef",
            // HashElse, // "#else",
            // HashElif, // "#elif",
            Bang,      // "!",
            Tilde,     // "~",
            Dash,      // "-",
            Plus,      // "+",
            Star,      // "*",
            Slash,     // "/",
            Percent,   // "%",
            PipePipe,  // "||",
            AmpAmp,    // "&&",
            Pipe,      // "|",
            Caret,     // "^",
            Amp,       // "&",
            EqEq,      // "==",
            BangEq,    // "!=",
            GT,        // ">",
            GTEq,      // ">=",
            LTEq,      // "<=",
            LT,        // "<",
            LtLt,      // "<<",
            GtGt,      // ">>",
            SemiColon, // ";",
            // Typedef, // "typedef",
            Extern,     // "extern",
            TS1,        // "__attribute__",
            ColonColon, // "::",
            TS2,        // "[[",
            TS3,        // "]]",
            // TS4, // "__declspec",
            // TS5, // "__based",
            // TS6, // "__cdecl",
            // TS7, // "__clrcall",
            // TS8, // "__stdcall",
            // TS9, // "__fastcall",
            // TS10, // "__thiscall",
            // TS11, // "__vectorcall",
            // MsRestrictModifier, // "ms_restrict_modifier",
            // MsUnsignedPtrModifier, // "ms_unsigned_ptr_modifier",
            // MsSignedPtrModifier, // "ms_signed_ptr_modifier",
            // TS12, // "_unaligned",
            // TS13, // "__unaligned",
            LBrace,   // "{",
            RBrace,   // "}",
            LBracket, // "[",
            RBracket, // "]",
            Eq,       // "=",
            // Static, // "static",
            // Register, // "register",
            // Inline, // "inline",
            // ThreadLocal, // "thread_local",
            // Const, // "const",
            // Volatile, // "volatile",
            // Restrict, // "restrict",
            // TS14, // "_Atomic",
            // Mutable, // "mutable",
            // Constexpr, // "constexpr",
            // Constinit, // "constinit",
            // Consteval, // "consteval",
            // Signed, // "signed",
            // Unsigned, // "unsigned",
            // Long, // "long",
            // Short, // "short",
            Enum,    // "enum",
            Class,   // "class",
            Struct,  // "struct",
            Union,   // "union",
            Colon,   // ":",
            If,      // "if",
            Else,    // "else",
            Switch,  // "switch",
            Case,    // "case",
            Default, // "default",
            While,   // "while",
            Do,      // "do",
            For,     // "for",
            Return,  // "return",
            // Break, // "break",
            // Continue, // "continue",
            // Goto, // "goto",
            QMark, // "?",
            // StarEq, // "*=",
            // SlashEq, // "/=",
            // PercentEq, // "%=",
            // PlusEq, // "+=",
            // DashEq, // "-=",
            // LtLtEq, // "<<=",
            // GtGtEq, // ">>=",
            // AmpEq, // "&=",
            // CaretEq, // "^=",
            // PipeEq, // "|=",
            // AndEq, // "and_eq",
            // OrEq, // "or_eq",
            // XorEq, // "xor_eq",
            // Not, // "not",
            // Compl, // "compl",
            // TS15, // "<=>",
            // Or, // "or",
            // And, // "and",
            // Bitor, // "bitor",
            // Xor, // "xor",
            // Bitand, // "bitand",
            // NotEq, // "not_eq",
            // DashDash, // "--",
            // PlusPlus, // "++",
            // Sizeof, // "sizeof",
            // Asm, // "asm",
            Dot,       // ".",
            DashGt,    // "->",
            Namespace, // "namespace",
            Using,     // "using",
        )
    }

    pub(crate) fn is_identifier(&self) -> bool {
        self == &Self::Identifier
            || self == &Self::QualifiedIdentifier
            || self == &Self::QualifiedIdentifier_
            || self == &Self::QualifiedIdentifier__
            || self == &Self::QualifiedIdentifier___
    }
    pub(crate) fn is_literal(&self) -> bool {
        self == &Self::NumberLiteral
            || self == &Self::CharLiteral
            || self == &Self::StringLiteral
            || self == &Self::RawStringLiteral
    }

    pub(crate) fn is_error(&self) -> bool {
        self == &Self::ERROR || self == &Self::_ERROR
    }

    pub(crate) fn as_shared(&self) -> hyperast::types::Shared {
        use hyperast::types::Shared;
        if self.is_error() {
            return Shared::Error;
        }
        match self {
            Type::ClassSpecifier => Shared::TypeDeclaration,
            Type::EnumSpecifier => Shared::TypeDeclaration,
            // Type::_TypeSpecifier => Shared::TypeDeclaration, // abstract
            Type::Decltype => Shared::TypeDeclaration,
            Type::DependentType => Shared::TypeDeclaration,
            Type::PlaceholderTypeSpecifier => Shared::TypeDeclaration,
            Type::PrimitiveType => Shared::TypeDeclaration,
            Type::SizedTypeSpecifier => Shared::TypeDeclaration,
            Type::StructSpecifier => Shared::TypeDeclaration,
            Type::TemplateType => Shared::TypeDeclaration,
            Type::TypeIdentifier => Shared::TypeDeclaration,
            Type::UnionSpecifier => Shared::TypeDeclaration,
            Type::Comment => Shared::Comment,
            Type::Identifier => Shared::Identifier,
            Type::QualifiedIdentifier => Shared::Identifier,
            x if x.is_identifier() => Shared::Literal,
            x if x.is_literal() => Shared::Literal,
            _ => Shared::Other,
        }
    }
}
