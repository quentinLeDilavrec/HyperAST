//! Fully compress all subtrees from a C++ syntax tree produced with tree-sitter.
//!
//! # Example
//! without any additional derived data
//! ```rust
//! # use hyperast::tree_gen::utils_ts::tree_sitter_parse;
//! # use hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen;
//! # let language = hyperast_gen_ts_cpp::language();
//! # let name = b"";
//! # let text = "class G {};";
//! let tree = tree_sitter_parse(text.as_bytes(), &language);
//! let mut stores = Default::default();
//! let mut g = CppTreeGen::bare(&mut stores);
//! let f = g.generate_file(name, text.as_bytes(), tree.walk());
//! use hyperast::nodes::SyntaxSerializer;
//! println!("{}", SyntaxSerializer::new(&stores, f.local.compressed_node));
//! # assert_eq!(text, hyperast::nodes::TextSerializer::new(&stores, f.local.compressed_node).to_string());
//! ```
//! # Example
//! precomputing patterns as defined by TreeSitter Queries
//! ```rust
//! # use hyperast::tree_gen::utils_ts::tree_sitter_parse;
//! # use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra;
//! # use hyperast_tsquery::{PreparedQuerying, Query};
//! # use hyperast_gen_ts_cpp::legion_ts_simp::CppTreeGen;
//! # use hyperast_gen_ts_cpp::{Type, TStore};
//! # type Acc = hyperast::tree_gen::zipped_ts_extra::Acc<Type>;
//! # let language = hyperast_gen_ts_cpp::language();
//! # let name = b"";
//! # let text = "class G {};";
//! # let tree = tree_sitter_parse(text.as_bytes(), &language);
//! # let query = "(identifier)\n(type_identifier)\n(primitive_type)";
//! # let precomputeds = ["(identifier)", "(type_identifier)"].as_slice();
//! # let mut stores = Default::default();
//! let (precomp, _q) = Query::with_precomputed(query, language, precomputeds).unwrap_or_else(|e| panic!("\n{e}"));
//! let more = PreparedQuerying::<_, TStore, Acc>::from(&precomp);
//! let mut extra = PatternPrecompExtra::<_, Acc, _>::from(more);
//! let mut g = CppTreeGen::new(&mut stores, &mut extra);
//! let f = g.generate_file(name, text.as_bytes(), tree.walk());
//! # use hyperast::nodes::SyntaxSerializer;
//! println!("{}", SyntaxSerializer::new(&stores, f.local.compressed_node));
//! println!("{:?}", f.extra);
//! # assert_eq!(text, hyperast::nodes::TextSerializer::new(&stores, f.local.compressed_node).to_string());
//! # assert_eq!(0b10, f.extra.0)
//! ```

use hyperast::tree_gen::zipped_ts_extra::TsTreeGen;

pub type CppTreeGen<'store, 'b, Extra> = TsTreeGen<'store, 'b, crate::TStore, Extra, true>;
