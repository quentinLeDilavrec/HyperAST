// This is generated code! regenerate using:
// `cargo run -p hyperast_gen_ts_xml --example regen_xml_types --no-default-features --features ts`

declare_type! {
    End = "end";
    Name = "Name";
    TS0 = "<?";
    Xml = "xml";
    TS1 = "?>";
    Standalone = "standalone";
    SQuote = "'";
    Yes = "yes";
    No = "no";
    DQuote = "\"";
    TS2 = "<!";
    TS3 = "DOCTYPE";
    LBracket = "[";
    RBracket = "]";
    GT = ">";
    LT = "<";
    TS4 = "/>";
    TS5 = "</";
    TS6 = "]]>";
    TS7 = "<![";
    TS8 = "CDATA";
    TS9 = "xml-stylesheet";
    TS10 = "xml-model";
    TS11 = "PseudoAttValue_token1";
    TS12 = "PseudoAttValue_token2";
    TS13 = "ELEMENT";
    TS14 = "EMPTY";
    TS15 = "ANY";
    LParen = "(";
    TS16 = "#PCDATA";
    Pipe = "|";
    RParen = ")";
    Star = "*";
    QMark = "?";
    Plus = "+";
    Comma = ",";
    TS17 = "ATTLIST";
    TokenizedType = "TokenizedType";
    TS18 = "NOTATION";
    TS19 = "#REQUIRED";
    TS20 = "#IMPLIED";
    TS21 = "#FIXED";
    TS22 = "ENTITY";
    Percent = "%";
    TS23 = "EntityValue_token1";
    TS24 = "EntityValue_token2";
    TS25 = "NDATA";
    SemiColon = ";";
    TS26 = "_S";
    Nmtoken = "Nmtoken";
    Amp = "&";
    TS27 = "&#";
    TS28 = "CharRef_token1";
    TS29 = "&#x";
    TS30 = "CharRef_token2";
    TS31 = "SYSTEM";
    TS32 = "PUBLIC";
    Uri = "URI";
    Uri_ = "URI";
    TS33 = "PubidLiteral_token1";
    TS34 = "PubidLiteral_token2";
    Version = "version";
    VersionNum = "VersionNum";
    Encoding = "encoding";
    EncName = "EncName";
    Eq
    = "=";
    PiTarget = "PITarget";
    _PiContent = "_pi_content";
    Comment = "Comment";
    CharData = "CharData";
    CData = "CData";
    Name_ = "Name";
    Name__ = "Name";
    _ErroneousEndName = "_erroneous_end_name";
    Document = "document";
    Prolog = "prolog";
    TS35 = "_Misc";
    XmlDecl = "XMLDecl";
    TS36 = "_SDDecl";
    Doctypedecl = "doctypedecl";
    TS37 = "_intSubset";
    Element = "element";
    EmptyElemTag = "EmptyElemTag";
    Attribute = "Attribute";
    STag = "STag";
    ETag = "ETag";
    Content = "content";
    CdSect = "CDSect";
    CdStart = "CDStart";
    StyleSheetPi = "StyleSheetPI";
    XmlModelPi = "XmlModelPI";
    PseudoAtt = "PseudoAtt";
    PseudoAttValue = "PseudoAttValue";
    _Markupdecl = "_markupdecl";
    TS38 = "_DeclSep";
    Elementdecl = "elementdecl";
    Contentspec = "contentspec";
    Mixed = "Mixed";
    Children = "children";
    _Cp = "_cp";
    _Choice = "_choice";
    AttlistDecl = "AttlistDecl";
    AttDef = "AttDef";
    AttType = "_AttType";
    StringType = "StringType";
    EnumeratedType = "_EnumeratedType";
    NotationType = "NotationType";
    Enumeration = "Enumeration";
    DefaultDecl = "DefaultDecl";
    EntityDecl
    = "_EntityDecl";
    GeDecl = "GEDecl";
    PeDecl = "PEDecl";
    EntityValue = "EntityValue";
    NDataDecl = "NDataDecl";
    NotationDecl = "NotationDecl";
    PeReference = "PEReference";
    Reference = "_Reference";
    EntityRef = "EntityRef";
    CharRef = "CharRef";
    AttValue = "AttValue";
    ExternalId = "ExternalID";
    PublicId = "PublicID";
    SystemLiteral = "SystemLiteral";
    PubidLiteral = "PubidLiteral";
    TS39 = "_VersionInfo";
    TS40 = "_EncodingDecl";
    Pi = "PI";
    TS41 = "_Eq";
    DocumentRepeat1 = "document_repeat1";
    TS42
    = "EmptyElemTag_repeat1";
    ContentRepeat1 = "content_repeat1";
    TS43 = "StyleSheetPI_repeat1";
    TS44 = "PseudoAttValue_repeat1";
    TS45 = "PseudoAttValue_repeat2";
    TS46 = "Mixed_repeat1";
    TS47 = "Mixed_repeat2";
    _ChoiceRepeat1 = "_choice_repeat1";
    _ChoiceRepeat2 = "_choice_repeat2";
    TS48 = "AttlistDecl_repeat1";
    TS49 = "NotationType_repeat1";
    TS50 = "Enumeration_repeat1";
    TS51 = "EntityValue_repeat1";
    TS52 = "EntityValue_repeat2";
}
impl Type {
    pub fn is_hidden(&self) -> bool {
        is!(
            self,
            End,
            TS11,
            TS12,
            TS23,
            TS24,
            TS26,
            TS28,
            TS30,
            TS33,
            TS34,
            _PiContent,
            _ErroneousEndName,
            TS35,
            TS36,
            TS37,
            _Markupdecl,
            TS38,
            _Cp,
            _Choice,
            AttType,
            EnumeratedType,
            EntityDecl,
            Reference,
            TS39,
            TS40,
            TS41,
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
    pub fn is_supertype(&self) -> bool {
        is!(
            self,
            _Markupdecl,
            AttType,
            EnumeratedType,
            EntityDecl,
            Reference,
        )
    }
    pub fn is_named(&self) -> bool {
        is!(
            self,
            Name,
            TokenizedType,
            Nmtoken,
            Uri,
            Uri_,
            VersionNum,
            EncName,
            PiTarget,
            Comment,
            CharData,
            CData,
            Name_,
            Name__,
            Document,
            Prolog,
            XmlDecl,
            Doctypedecl,
            Element,
            EmptyElemTag,
            Attribute,
            STag,
            ETag,
            Content,
            CdSect,
            CdStart,
            StyleSheetPi,
            XmlModelPi,
            PseudoAtt,
            PseudoAttValue,
            _Markupdecl,
            Elementdecl,
            Contentspec,
            Mixed,
            Children,
            AttlistDecl,
            AttDef,
            AttType,
            StringType,
            EnumeratedType,
            NotationType,
            Enumeration,
            DefaultDecl,
            EntityDecl,
            GeDecl,
            PeDecl,
            EntityValue,
            NDataDecl,
            NotationDecl,
            PeReference,
            Reference,
            EntityRef,
            CharRef,
            AttValue,
            ExternalId,
            PublicId,
            SystemLiteral,
            PubidLiteral,
            Pi,
        )
    }
}
