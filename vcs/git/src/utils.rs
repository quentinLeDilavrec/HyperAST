pub fn resolve_language(language: &str) -> Option<tree_sitter::Language> {
    match language {
        #[cfg(feature = "java")]
        "Java" | "java" => Some(hyperast_gen_ts_java::language()),
        #[cfg(feature = "cpp")]
        "Cpp" | "cpp" => Some(hyperast_gen_ts_cpp::language()),
        _ => None,
    }
}

pub(crate) enum ParseErr {
    NotUtf8(std::str::Utf8Error),
    IllFormed,
}

impl std::fmt::Debug for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotUtf8(e) => e.fmt(f),
            Self::IllFormed => write!(f, "IllFormed"),
        }
    }
}

impl From<std::str::Utf8Error> for ParseErr {
    fn from(value: std::str::Utf8Error) -> Self {
        ParseErr::NotUtf8(value)
    }
}

pub(crate) struct FailedParsing<D = std::time::Duration> {
    pub parsing_time: D,
    pub tree: tree_sitter::Tree,
    pub error: &'static str,
}

pub(crate) struct SuccessProcessing<N, D = std::time::Duration> {
    pub parsing_time: D,
    pub processing_time: D,
    pub node: N,
}

pub(crate) type FileProcessingResult<N, D = std::time::Duration> =
    Result<SuccessProcessing<N, D>, FailedParsing<D>>;

pub fn auto_configured_line_break(text: &str) -> Vec<u8> {
    _auto_configured_line_break(text.as_bytes())
}

pub(super) fn _auto_configured_line_break(text: &[u8]) -> Vec<u8> {
    if text.contains(&b'\r') {
        "\r\n".as_bytes().to_vec()
    } else {
        "\n".as_bytes().to_vec()
    }
}
