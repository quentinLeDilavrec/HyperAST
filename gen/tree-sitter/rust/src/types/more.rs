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

    pub fn is_syntax(&self) -> bool {
        is!(
            self,
            SemiColon,                     // ";";
            TS0,                           // "macro_rules!";
            LParen,                        // "(";
            RParen,                        // ")";
            LBracket,                      // "[";
            RBracket,                      // "]";
            LBrace,                        // "{";
            RBrace,                        // "}";
            BigArrow,                      // "=>";
            Colon,                         // ":";
            TS1,                           // "$";
            Plus,                          // "+";
            Star,                          // "*";
            QMark,                         // "?";
            Block,                         // "block";
            Expr,                          // "expr";
            TS2,                           // "expr_2021";
            Ident,                         // "ident";
            Item,                          // "item";
            Lifetime,                      // "lifetime";
            Literal,                       // "literal";
            Meta,                          // "meta";
            Pat,                           // "pat";
            PatParam,                      // "pat_param";
            Path,                          // "path";
            Stmt,                          // "stmt";
            Tt,                            // "tt";
            Ty,                            // "ty";
            Vis,                           // "vis";
            PrimitiveType,                 // "primitive_type";
            PrimitiveType_,                // "primitive_type";
            PrimitiveType__,               // "primitive_type";
            PrimitiveType___,              // "primitive_type";
            PrimitiveType____,             // "primitive_type";
            PrimitiveType_____,            // "primitive_type";
            PrimitiveType______,           // "primitive_type";
            PrimitiveType_______,          // "primitive_type";
            PrimitiveType________,         // "primitive_type";
            PrimitiveType_________,        // "primitive_type";
            PrimitiveType__________,       // "primitive_type";
            PrimitiveType___________,      // "primitive_type";
            PrimitiveType____________,     // "primitive_type";
            PrimitiveType_____________,    // "primitive_type";
            PrimitiveType______________,   // "primitive_type";
            PrimitiveType_______________,  // "primitive_type";
            PrimitiveType________________, // "primitive_type";
            Dash,                          // "-";
            Slash,                         // "/";
            Percent,                       // "%";
            Caret,                         // "^";
            Bang,                          // "!";
            Amp,                           // "&";
            Pipe,                          // "|";
            AmpAmp,                        // "&&";
            PipePipe,                      // "||";
            LtLt,                          // "<<";
            GtGt,                          // ">>";
            PlusEq,                        // "+=";
            DashEq,                        // "-=";
            StarEq,                        // "*=";
            SlashEq,                       // "/=";
            PercentEq,                     // "%=";
            CaretEq,                       // "^=";
            AmpEq,                         // "&=";
            PipeEq,                        // "|=";
            LtLtEq,                        // "<<=";
            GtGtEq,                        // ">>=";
            Eq,                            // "=";
            EqEq,                          // "==";
            BangEq,                        // "!=";
            GT,                            // ">";
            LT,                            // "<";
            GTEq,                          // ">=";
            LTEq,                          // "<=";
            At,                            // "@";
            Inderscore,                    // "_";
            Dot,                           // ".";
            TS3,                           // "..";
            DotDotDot,                     // "...";
            TS4,                           // "..=";
            Comma,                         // ",";
            ColonColon,                    // "::";
            DashGt,                        // "->";
            Sharp,                         // "#";
            SQuote,                        // "'";
            As,                            // "as";
            Async,                         // "async";
            Await,                         // "await";
            Break,                         // "break";
            Const,                         // "const";
            Continue,                      // "continue";
            Default,                       // "default";
            Enum,                          // "enum";
            Fn,                            // "fn";
            For,                           // "for";
            Gen,                           // "gen";
            If,                            // "if";
            Impl,                          // "impl";
            Let,                           // "let";
            Loop,                          // "loop";
            Match,                         // "match";
            Mod,                           // "mod";
            Pub,                           // "pub";
            Return,                        // "return";
            Static,                        // "static";
            Struct,                        // "struct";
            Trait,                         // "trait";
            Type,                          // "type";
            Union,                         // "union";
            Unsafe,                        // "unsafe";
            Use,                           // "use";
            Where,                         // "where";
            While,                         // "while";
            Extern,                        // "extern";
            Ref,                           // "ref";
            Else,                          // "else";
            In,                            // "in";
            LT_,                           // "<";
            Dyn,                           // "dyn";
            MutableSpecifier,              // "mutable_specifier";
            Raw,                           // "raw";
            Yield,                         // "yield";
            Move,                          // "move";
            Try,                           // "try";
            IntegerLiteral,                // "integer_literal";
            DQuote,                        // "\"";
            DQuote_,                       // "\"";
            CharLiteral,                   // "char_literal";
            EscapeSequence,                // "escape_sequence";
            True,                          // "true";
            False,                         // "false";
            TS5,                           // "//";
            Bang_,                         // "!";
            Slash_,                        // "/";
            TS6,                           // "/*";
            TS7,                           // "*/";
            Shebang,                       // "shebang";
            TS8,                           // "self";
            Super,                         // "super";
            Crate,                         // "crate";
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
