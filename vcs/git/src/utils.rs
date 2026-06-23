use std::path::PathBuf;

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

impl From<FailedParsing> for ParseErr {
    fn from(_: FailedParsing) -> Self {
        ParseErr::IllFormed
    }
}

pub(crate) struct SuccessProcessing<N, D = std::time::Duration> {
    pub parsing_time: D,
    pub processing_time: D,
    pub node: N,
}

impl<N, D> SuccessProcessing<N, D> {
    pub fn map<N2>(self, f: impl FnOnce(N) -> N2) -> SuccessProcessing<N2, D> {
        SuccessProcessing {
            parsing_time: self.parsing_time,
            processing_time: self.processing_time,
            node: f(self.node),
        }
    }
}

// waiting for residual stabilization https://github.com/rust-lang/rust/issues/84277
// see after the temporary solution
// It is also limiting the usability with more variants
// enum FileProcessingResult<N, D = Duration> {
//     FailedParsing {
//         parsing_time: D,
//         tree: tree_sitter::Tree,
//         error: &'static str,
//     },
//     // ParsingTimedout(D),
//     // FailedProcessing {
//     //     parsing_time: D,
//     //     processing_time: D,
//     //     node: N,
//     // },
//     Success {
//         parsing_time: D,
//         processing_time: D,
//         node: N,
//     },
// }
/// Fancy return type for file processing
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

pub(crate) type Str = std::sync::Arc<str>;

pub(crate) fn drain_filter_strip(path: &mut Option<Vec<PathBuf>>, name: &[u8]) -> Vec<PathBuf> {
    let mut new_paths = vec![];
    let name = std::str::from_utf8(&name).unwrap();
    if let Some(paths) = path {
        vec_extract_if_polyfill::MakeExtractIf::extract_if(paths, |x| x.starts_with(name))
            .for_each(|x| {
                let x = x.strip_prefix(name).unwrap().to_owned();
                new_paths.push(x);
            });
    }
    new_paths
}
