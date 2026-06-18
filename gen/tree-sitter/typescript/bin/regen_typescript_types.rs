//! Regenerates node types
//!
//! Compiled as an example binary to access polyglot as a dev-dependencies,
//! while using the exact same language definition as the generator library.
//!
//! NOTE This is meant to help update the node types when the language definition changes,
//! not to handle multiple versions.
//! The idea is to use sufficiently relaxed grammars to handle any version of the language.
//! Then if you want to differentiate between different versions, you should do it on the AST.

use std::path::PathBuf;

use polyglot::LanguageCompo;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env!("CARGO_MANIFEST_DIR").parse::<PathBuf>().unwrap();
    let path = path.join("src/types");
    if !path.exists() && !path.is_dir() {
        panic!("types directory does not exist");
    }
    let path = path.join("generated.rs");

    let lang = Lang;
    let command = format!(
        "cargo run -p {} --example {} --no-default-features --features ts",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_BIN_NAME"),
    );
    polyglot::generate_with_macros(lang, path, &command)
}

struct Lang;

impl LanguageCompo for Lang {
    fn language(&self) -> tree_sitter::Language {
        hyperast_gen_ts_typescript::language()
    }

    fn name(&self) -> &str {
        "Ts"
    }

    fn node_types(&self) -> &str {
        hyperast_gen_ts_typescript::node_types()
    }
}
