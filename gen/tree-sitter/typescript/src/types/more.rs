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
            SequenceExpressionRepeat1,
            StringRepeat1,
            StringRepeat2,
            TemplateStringRepeat1,
            ClassBodyRepeat1,
            FormalParametersRepeat1,
            ExtendsClauseRepeat1,
            ImplementsClauseRepeat1,
            ExtendsTypeClauseRepeat1,
            EnumBodyRepeat1,
            TemplateLiteralTypeRepeat1,
            ObjectTypeRepeat1,
            TypeParametersRepeat1,
            TupleTypeRepeat1,
        )
    }
    pub fn is_syntax(&self) -> bool {
        is!(
            self,
            // Identifier,                // "identifier";
            HashBangLine,              // "hash_bang_line";
            Export,                    // "export";
            Star,                      // "*";
            Default,                   // "default";
            Type,                      // "type";
            Eq,                        // "=";
            As,                        // "as";
            Namespace,                 // "namespace";
            LBrace,                    // "{";
            Comma,                     // ",";
            RBrace,                    // "}";
            Typeof,                    // "typeof";
            Import,                    // "import";
            From,                      // "from";
            With,                      // "with";
            Assert,                    // "assert";
            Var,                       // "var";
            Let,                       // "let";
            Const,                     // "const";
            Bang,                      // "!";
            Else,                      // "else";
            If,                        // "if";
            Switch,                    // "switch";
            For,                       // "for";
            LParen,                    // "(";
            SemiColon,                 // ";";
            RParen,                    // ")";
            Await,                     // "await";
            In,                        // "in";
            Of,                        // "of";
            While,                     // "while";
            Do,                        // "do";
            Try,                       // "try";
            Break,                     // "break";
            Continue,                  // "continue";
            Debugger,                  // "debugger";
            Return,                    // "return";
            Throw,                     // "throw";
            Colon,                     // ":";
            Case,                      // "case";
            Catch,                     // "catch";
            Finally,                   // "finally";
            Yield,                     // "yield";
            LBracket,                  // "[";
            RBracket,                  // "]";
            Dot,                       // ".";
            Class,                     // "class";
            Async,                     // "async";
            Function,                  // "function";
            BigArrow,                  // "=>";
            QMarkDot,                  // "?.";
            New,                       // "new";
            Using,                     // "using";
            PlusEq,                    // "+=";
            DashEq,                    // "-=";
            StarEq,                    // "*=";
            SlashEq,                   // "/=";
            PercentEq,                 // "%=";
            CaretEq,                   // "^=";
            AmpEq,                     // "&=";
            PipeEq,                    // "|=";
            GtGtEq,                    // ">>=";
            GtGtGtEq,                  // ">>>=";
            LtLtEq,                    // "<<=";
            StarStarEq,                // "**=";
            AmpAmpEq,                  // "&&=";
            PipePipeEq,                // "||=";
            QMarkQMarkEq,              // "??=";
            DotDotDot,                 // "...";
            AmpAmp,                    // "&&";
            PipePipe,                  // "||";
            GtGt,                      // ">>";
            GtGtGt,                    // ">>>";
            LtLt,                      // "<<";
            Amp,                       // "&";
            Caret,                     // "^";
            Pipe,                      // "|";
            Plus,                      // "+";
            Dash,                      // "-";
            Slash,                     // "/";
            Percent,                   // "%";
            StarStar,                  // "**";
            LT,                        // "<";
            LTEq,                      // "<=";
            EqEq,                      // "==";
            EqEqEq,                    // "===";
            BangEq,                    // "!=";
            BangEqEq,                  // "!==";
            GTEq,                      // ">=";
            GT,                        // ">";
            QMarkQMark,                // "??";
            Instanceof,                // "instanceof";
            Tilde,                     // "~";
            Void,                      // "void";
            Delete,                    // "delete";
            PlusPlus,                  // "++";
            DashDash,                  // "--";
            DQuote,                    // "\"";
            SQuote,                    // "'";
            StringFragment,            // "string_fragment";
            StringFragment_,           // "string_fragment";
            EscapeSequence,            // "escape_sequence";
            Comment,                   // "comment";
            BQuote,                    // "`";
            DollarLBrace,              // "${";
            Slash_,                    // "/";
            RegexPattern,              // "regex_pattern";
            RegexFlags,                // "regex_flags";
            Number,                    // "number";
            PrivatePropertyIdentifier, // "private_property_identifier";
            Target,                    // "target";
            Meta,                      // "meta";
            This,                      // "this";
            Super,                     // "super";
            TS0,                       // "true";
            TS1,                       // "false";
            Null,                      // "null";
            Undefined,                 // "undefined";
            At,                        // "@";
            Static,                    // "static";
            Readonly,                  // "readonly";
            Get,                       // "get";
            Set,                       // "set";
            QMark,                     // "?";
            Declare,                   // "declare";
            Public,                    // "public";
            Private,                   // "private";
            Protected,                 // "protected";
            Override,                  // "override";
            Module,                    // "module";
            Any,                       // "any";
            Number_,                   // "number";
            Boolean,                   // "boolean";
            String,                    // "string";
            Symbol,                    // "symbol";
            Object,                    // "object";
            Abstract,                  // "abstract";
            Accessor,                  // "accessor";
            Satisfies,                 // "satisfies";
            Require,                   // "require";
            Extends,                   // "extends";
            Implements,                // "implements";
            Global,                    // "global";
            Interface,                 // "interface";
            Enum,                      // "enum";
            MinusQMarkColon,           // "-?:";
            PlusQMarkColon,            // "+?:";
            QMarkColon,                // "?:";
            Asserts,                   // "asserts";
            Infer,                     // "infer";
            Is,                        // "is";
            Keyof,                     // "keyof";
            TS2,                       // "unique symbol";
            Unknown,                   // "unknown";
            Never,                     // "never";
            LBracePipe,                // "{|";
            PipeRBrace,                // "|}";
            _AutomaticSemicolon,       // "_automatic_semicolon";
            StringFragment__,          // "string_fragment";
            QMark_,                    // "?";
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
