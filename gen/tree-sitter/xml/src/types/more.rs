use super::Type;

impl Type {
    pub(crate) fn is_repeat(&self) -> bool {
        is!(
            self,
            DocumentRepeat1,
            TS42,
            ContentRepeat1,
            TS43,
            TS44,
            TS45,
            TS46,
            TS47,
            _ChoiceRepeat1,
            _ChoiceRepeat2,
            TS48,
            TS49,
            TS50,
            TS51,
            TS52,
        )
    }

    pub fn is_syntax(&self) -> bool {
        is!(
            self, TS2, // " ",
            // Nmtoken, // "Nmtoken",
            TS3, // "\"",
            TS4, // "'",
            // TS5, // "Sep1_token1",
            // TS6, // "Sep2_token1",
            // TS7, // "Sep3_token1",
            // SystemLiteral, // "SystemLiteral",
            // PubidLiteral, // "PubidLiteral",
            // CharData, // "CharData",
            // Comment, // "Comment",
            TS8,    // "<?",
            TS9,    // "?>",
            CdSect, // "CDSect",
            TS10,   // "<?xml",
            // Version, // "version",
            Eq, // "=",
            // VersionNum, // "VersionNum",
            TS11,     // "<!DOCTYPE",
            LBracket, // "[",
            RBracket, // "]",
            GT,       // ">",
            // Standalone, // "standalone",
            // Yes, // "yes",
            // No, // "no",
            LT,   // "<",
            TS12, // "</",
            TS13, // "/>",
            TS14, // "<!ELEMENT",
            // TS15, // "EMPTY",
            // TS16, // "ANY",
            QMark,  // "?",
            Star,   // "*",
            Plus,   // "+",
            LParen, // "(",
            Pipe,   // "|",
            RParen, // ")",
            Comma,  // ",",
            // TS17, // "#PCDATA",
            TS18,       // ")*",
            TS19,       // "<!ATTLIST",
            StringType, // "StringType",
            // TS20, // "ID",
            // TS21, // "IDREF",
            // TS22, // "IDREFS",
            // TS23, // "ENTITY",
            // TS24, // "ENTITIES",
            // TS25, // "NMTOKEN",
            // TS26, // "NMTOKENS",
            // TS27, // "NOTATION",
            // TS28, // "#REQUIRED",
            // TS29, // "#IMPLIED",
            // TS30, // "#FIXED",
            // CharRef, // "CharRef",
            Amp,       // "&",
            SemiColon, // ";",
            Percent,   // "%",
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
