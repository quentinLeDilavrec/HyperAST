use hyperast::position::structural_pos::CursorHead;
use tree_sitter::Parser;

use crate::legion::tree_sitter_parse;
use crate::tests::{EX_ISSUE_MISSING_NODE2, EX_ISSUE_MISSING_NODE3};

use super::EX_ISSUE_MISSING_NODE;
use super::{CppTreeGen, SimpleStores};

static EX: &str = r#"
void read_string(char *buf) {
    scanf("%s ", buf);
}"#;

#[test]
pub(crate) fn cpp_preproc_call_decl_test() {
    let text = EX.as_bytes();
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // assert_eq!(
    //     hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node).to_string(),
    //     tree.root_node().to_sexp().to_string()
    // );
    use hyperast::nodes;
    let id = x.compressed_node;
    println!("{}", nodes::SyntaxSerializer::new(&stores, id));
    println!("{}", nodes::TextSerializer::new(&stores, id));
    println!("{}", nodes::SexpSerializer::new(&stores, id));
    println!("{}", nodes::SyntaxWithFieldsSerializer::new(&stores, id));

    let query = Q;
    let precomp: &[&str] = &["(translation_unit)"];
    let (_, query) =
        hyperast_tsquery::Query::with_precomputed(query, crate::language(), precomp).unwrap();
    let mut stores = hyperast::store::SimpleStores::<crate::TStore>::default();
    let mut md_cache = Default::default();
    let mut cpp_tree_gen = crate::legion::CppTreeGen::new(&mut stores, &mut md_cache);

    let tree = crate::legion::tree_sitter_parse(text);
    log::trace!("sexp:\n{}", tree.root_node().to_sexp());
    let full_node = cpp_tree_gen.generate_file(b"", text, tree.walk());
    log::trace!(
        "syntax ser:\n{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, full_node.local.compressed_node)
    );
    let (query, stores, code) = (query, stores, full_node.local.compressed_node);
    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(code);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&stores, pos);
    let mut matches = query.matches(cursor);
    let Some(_) = matches.next() else {
        panic!();
    };
}

static EX2: &str = r#"
    extern int foo;

    // this one has a comment
    extern int bar;

    // this one has two comments
    // ooOoOoOOOo
    extern int baz;
"#;

/// https://github.com/tree-sitter/tree-sitter/issues/3683
#[test]
pub(crate) fn cpp_multi_repeat_incomplete_test() {
    let text = EX2.as_bytes();
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    use hyperast::nodes;
    let id = x.compressed_node;
    println!("{}", nodes::SyntaxSerializer::new(&stores, id));
    println!("{}", nodes::TextSerializer::new(&stores, id));
    println!("{}", nodes::SexpSerializer::new(&stores, id));
    println!("{}", nodes::SyntaxWithFieldsSerializer::new(&stores, id));

    let query = r#"
        (translation_unit
          ((comment)*
          .
          (declaration)) @decl
        )
    "#;
    let query = hyperast_tsquery::Query::new(query, crate::language()).unwrap();
    let cid = query.capture_index_for_name("decl").unwrap();
    // let cid2 = query.capture_index_for_name("comment").unwrap();

    let pos = hyperast::position::structural_pos::CursorWithPersistence::new(x.compressed_node);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&stores, pos);
    let mut matches = query.matches(cursor);
    while let Some(x) = matches.next() {
        dbg!();
        // for c in x.nodes_for_capture_index(cid2) {
        //     let id = c.pos.node();
        //     dbg!();
        //     println!("{}", nodes::TextSerializer::new(&stores, id));
        // }
        for c in x.nodes_for_capture_index(cid) {
            let id = c.pos.node();
            dbg!();
            println!("{}", nodes::TextSerializer::new(&stores, id));
        }
    }
}
#[test_log::test]
pub(crate) fn cpp_parsing_error_test() {
    let text = EX_ISSUE_MISSING_NODE.as_bytes();
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    use hyperast::nodes;
    let id = x.compressed_node;
    println!("{}", nodes::SyntaxSerializer::new(&stores, id));
    println!("{}", nodes::TextSerializer::new(&stores, id));
    println!("{}", nodes::SexpSerializer::new(&stores, id));
    println!("{}", nodes::SyntaxWithFieldsSerializer::new(&stores, id));
}

#[test_log::test]
pub(crate) fn cpp_parsing_missing_systemd_dbus_cred_c_test() {
    let text = EX_ISSUE_MISSING_NODE2.as_bytes();
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    use hyperast::nodes;
    let id = x.compressed_node;
    println!("{}", nodes::SyntaxSerializer::new(&stores, id));
    println!("{}", nodes::TextSerializer::new(&stores, id));
    println!("{}", nodes::SexpSerializer::new(&stores, id));
    println!("{}", nodes::SyntaxWithFieldsSerializer::new(&stores, id));
}
#[test_log::test]
pub(crate) fn cpp_parsing_missing_systemd_pam_systemd_c_test() {
    let text = EX_ISSUE_MISSING_NODE3.as_bytes();
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    use hyperast::nodes;
    let id = x.compressed_node;
    println!("{}", nodes::SyntaxSerializer::new(&stores, id));
    println!("{}", nodes::TextSerializer::new(&stores, id));
    println!("{}", nodes::SexpSerializer::new(&stores, id));
    println!("{}", nodes::SyntaxWithFieldsSerializer::new(&stores, id));
}
#[allow(unused)]
static Q2: &str = r#"
(translation_unit
    (function_definition
        type: (primitive_type)
        declarator: (function_declarator
            declarator: (identifier)
            parameters: (parameter_list (parameter_declaration
                type: (primitive_type)
                declarator: (pointer_declarator declarator: (identifier)))))
        body: (compound_statement
            (preproc_call_declaration
                function: (identifier)
                arguments: (argument_list (string_literal (string_content)) (identifier))))))
"#;

static Q: &str = r#"
(function_definition
  body: (compound_statement
    (preproc_call_declaration
        function:
           (identifier)
        arguments: (argument_list (string_literal (string_content)) (identifier))
    )
  )
) @CVE-1900-0000_0
"#;

#[test]
pub(crate) fn cpp_tree_sitter_simple() {
    let mut parser = Parser::new();

    {
        parser.set_language(&crate::language()).unwrap();
    }

    let text = {
        let source_code1 = r#"#include <aaaa.h>
struct TSLanguage {
    uint32_t version;
    uint32_t symbol_count;
    uint32_t alias_count;
    uint32_t token_count;
    uint32_t external_token_count;
    uint32_t state_count;
    uint32_t large_state_count;
    uint32_t production_id_count;
    uint32_t field_count;
    uint16_t max_alias_sequence_length;
    const uint16_t *parse_table;
    const uint16_t *small_parse_table;
    const uint32_t *small_parse_table_map;
    const TSParseActionEntry *parse_actions;
    const char * const *symbol_names;
    const char * const *field_names;
    const TSFieldMapSlice *field_map_slices;
    const TSFieldMapEntry *field_map_entries;
    const TSSymbolMetadata *symbol_metadata;
    const TSSymbol *public_symbol_map;
    const uint16_t *alias_map;
    const TSSymbol *alias_sequences;
    const TSLexMode *lex_modes;
    bool (*lex_fn)(TSLexer *, TSStateId);
    bool (*keyword_lex_fn)(TSLexer *, TSStateId);
    TSSymbol keyword_capture_token;
    struct {
        const bool *states;
        const TSSymbol *symbol_map;
        void *(*create)(void);
        void (*destroy)(void *);
        bool (*scan)(void *, TSLexer *, const bool *symbol_whitelist);
        unsigned (*serialize)(void *, char *);
        void (*deserialize)(void *, const char *, unsigned);
    } external_scanner;
};

int main(int argl, int* argv) {
    printf("%i",42);
}"#;
        source_code1.as_bytes()
    };
    let tree = parser.parse(text, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
}

#[test]
pub(crate) fn cpp_simple_test() {
    let text = {
        let source_code1 = r#"
int main(int argl, int* argv) {
    printf("a %1 a",42);
}"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_systemd_test() {
    let text = {
        let source_code1 = r#"
ACK(aa, int, LOG_FAC(~0));
"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_issue_stockfish_movegen_test() {
    let text = {
        let source_code1 = r#"
void f() {
f.value < s;
}
"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_template_test() {
    let text = {
        let source_code1 = r#"
void f() {
    x.g<T>();
}
"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_explicit_cast_test() {
    let text = {
        let source_code1 = r#"
void f() {
    ((unsigned(b) ^ b) * DeBruijn32);
}
"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_issue_stockfish_endgame_test() {
    let text = {
        let source_code1 = r#"


template<>
ScaleFactor ScalingFunction<KNPK>::apply() const {
}

"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_issue_stockfish_types_test() {
    let text = {
        let source_code1 = r#"
#ifndef TYPES_H_INCLUDED
#define TYPES_H_INCLUDED


#define ENABLE_FULL_OPERATORS_ON(T)                             \
inline T operator+(T d1, T d2) { return T(int(d1) + int(d2)); } \
inline T operator-(T d1, T d2) { return T(int(d1) - int(d2)); } \
inline T operator*(int i, T d) { return T(i * int(d)); }        \
inline T& operator/=(T& d, int i) { return d = T(int(d) / i); }

ENABLE_FULL_OPERATORS_ON(Value)
ENABLE_FULL_OPERATORS_ON(PieceType)
ENABLE_FULL_OPERATORS_ON(Rank)
ENABLE_BASE_OPERATORS_ON(Score)

void f() {}

#endif // #ifndef TYPES_H_INCLUDED

"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_issue_stockfish_tbcore_test() {
    let text = {
        let source_code1 = r#"
#ifndef TBCORE_H
#define TBCORE_H

struct PairsData {
char *indextable;
ushort *sizetable;
base_t base[1]; // C++ complains about base[]...
};

struct TBEntry {
char *data;
ubyte symmetric;
ubyte has_pawns;
}
#ifndef _WIN32
__attribute__((__may_alias__))
#endif
;

#endif
"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[allow(non_snake_case)]
#[test]
pub(crate) fn cpp_issue_stockfish_TranslationUnitRepeat1_test() {
    let text = {
        let source_code1 = r#"
#ifndef TBCORE_H
#define TBCORE_H

struct TBEntry {};

#endif
"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // print_tree_syntax(&stores.node_store, &stores.label_store, &x.compressed_node);
    // println!("{}", tree.root_node().to_sexp());
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
}

#[test]
pub(crate) fn cpp_def_bl_test() {
    let text = {
        let source_code1 = CODE_DEF_BL_SHRINK;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    // println!("{}", tree.root_node().to_sexp());
}

pub(crate) const CODE_DEF_BL_SHRINK: &str = r#"
#if   defined(__AVX512BW__) || \
     defined(__AVX512F__)

"#;

#[test]

pub(crate) fn cpp_char_literal_test() {
    let text = {
        let source_code1 = CODE_CHAR_LIT;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    let entity = x.compressed_node;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, entity)
    );
    println!(
        "{}",
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node)
    );
    println!(
        "{}",
        hyperast::nodes::SexpSerializer::new(&stores, x.compressed_node)
    );
    // println!("{}", tree.root_node().to_sexp());
}

pub(crate) const CODE_CHAR_LIT: &str = r#"

// format_cp_aligned_dot() converts a Value into (centi)pawns, always keeping two decimals.
static void format_cp_aligned_dot(Value v, std::stringstream &stream) {
    const double cp = 1.0 * std::abs(int(v)) / UCI::NormalizeToPawnValue;

  stream << (v < 0 ? '-' : v > 0 ? '+' : ' ')
         << std::setiosflags(std::ios::fixed)
         << std::setw(6)
         << std::setprecision(2)
         << cp;
}


std::string trace(Position& pos) {
    const int x = ((int)file) * 8;
    const int y = (7 - (int)rank) * 3;
    for (int i = 1; i < 8; ++i)
        board[y][x+i] = board[y+3][x+i] = '-';
    }

"#;

#[test]
pub(crate) fn cpp_asm_test() {
    let text = {
        let source_code1 = CODE_ASM;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    // println!("{}", tree.root_node().to_sexp());
}

pub(crate) const CODE_ASM: &str = r#"
int f() {
    asm(
        "vpmaddwd    %[tmp0], %[ones], %[tmp0]\n\t"
        "vpmaddwd    %[tmp1], %[ones], %[tmp1]\n\t"
        "vpaddd      %[tmp0], %[tmp1], %[tmp0]\n\t"
        "vpaddd      %[acc], %[tmp0], %[acc]\n\t"
        : [acc]"+v"(acc), [tmp0]"+&v"(tmp0), [tmp1]"+&v"(tmp1)
        : [ones]"v"(_mm512_set1_epi16(1))
    );
}
"#;

#[test]
pub(crate) fn cpp_op_test() {
    let text = {
        let source_code1 = CODE_OP;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    // println!("{}", tree.root_node().to_sexp());
}

// https://github.com/official-stockfish/Stockfish/blob/d55a5a4d81b613e5a82e428770347b06fbd2d9a8/src/position.cpp
pub(crate) const CODE_OP: &str = r#"

namespace {

// Bonus for having the side to move (modified by Joona Kiiski)
const Score TempoValue = make_score(48, 22);

bool isZero(char c) { return c == '0'; }

struct PieceLetters : public std::map<char, Piece> {

  PieceLetters() {
      operator[]('K') = WK; operator[]('k') = BK;
      operator[]('Q') = WQ; operator[]('q') = BQ;
      operator[]('R') = WR; operator[]('r') = BR;
      operator[]('B') = WB; operator[]('b') = BB;
      operator[]('N') = WN; operator[]('n') = BN;
      operator[]('P') = WP; operator[]('p') = BP;
      operator[](' ') = PIECE_NONE; operator[]('.') = PIECE_NONE_DARK_SQ;
    }
} pieceLetters;
}
"#;

#[test]
pub(crate) fn issue_npy_nditer_test() {
    let text = CODE_NPY_NDITER_CONSTR;
    let tree = tree_sitter_parse(text.as_bytes());
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    use crate::legion_ts_simp::CppTreeGen;
    let mut tree_gen = CppTreeGen::bare(&mut stores);
    // let mut md_cache = Default::default();
    // let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let n = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = n.node.local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    pretty_assertions::assert_eq!(
        text,
        hyperast::nodes::TextSerializer::new(&stores, x.compressed_node).to_string()
    );
    // println!("{}", tree.root_node().to_sexp());
}

// https://github.com/numpy/numpy/blob/main/numpy/_core/src/multiarray/nditer_constr.c
pub(crate) const CODE_NPY_NDITER_CONSTR: &str = r#"NPY_NO_EXPORT NpyIter *
NpyIter_AdvancedNew()
{
    NIT_ITERINDEX(iter) = 0;
    memset(NIT_BASEOFFSETS(iter), 0, (nop+1)*NPY_SIZEOF_INTP);
}"#;

#[test]
pub(crate) fn issue_npy_nditer_test2() {
    let text = CODE_NPY_NDITER_CONSTR2;
    let tree = tree_sitter_parse(text.as_bytes());
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    use crate::legion_ts_simp::CppTreeGen;
    let mut tree_gen = CppTreeGen::bare(&mut stores);
    // let mut md_cache = Default::default();
    // let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let n = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = n.node.local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    let res = hyperast::nodes::TextSerializer::new(&stores, x.compressed_node).to_string();
    dbg!(&text, &res);
    pretty_assertions::assert_eq!(text, res);
    // println!("{}", tree.root_node().to_sexp());
}

// https://github.com/numpy/numpy/blob/main/numpy/_core/src/multiarray/nditer_constr.c
pub(crate) const CODE_NPY_NDITER_CONSTR2: &str = r#"/*
 * This file implements the construction, copying, and destruction
 * aspects of NumPy's nditer.
 *
 * Copyright (c) 2010-2011 by Mark Wiebe (mwwiebe@gmail.com)
 * The University of British Columbia
 *
 * Copyright (c) 2011 Enthought, Inc
 *
 * See LICENSE.txt for the license.
 */
#define NPY_NO_DEPRECATED_API NPY_API_VERSION

/* Allow this .c file to include nditer_impl.h */
#define NPY_ITERATOR_IMPLEMENTATION_CODE

#include "alloc.h"
#include "nditer_impl.h"
#include "arrayobject.h"
#include "array_coercion.h"
#include "templ_common.h"
#include "array_assign.h"
#include "dtype_traversal.h"
"#;

#[test]
pub(crate) fn issue_npy_nditer_test3() {
    let text = CODE_NPY_NDITER_CONSTR3;
    let tree = tree_sitter_parse(text.as_bytes());
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    use crate::legion_ts_simp::CppTreeGen;
    let mut tree_gen = CppTreeGen::bare(&mut stores);
    // let mut md_cache = Default::default();
    // let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let n = tree_gen.generate_file(b"", text.as_bytes(), tree.walk());
    let x = n.node.local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    let res = hyperast::nodes::TextSerializer::new(&stores, x.compressed_node).to_string();
    pretty_assertions::assert_eq!(text[0..1000], res[0..1000]);
    pretty_assertions::assert_eq!(text[1000..2000], res[1000..2000]);
    pretty_assertions::assert_eq!(text[2000..], res[2000..]);
    // println!("{}", tree.root_node().to_sexp());
}

// https://github.com/numpy/numpy/blob/main/numpy/_core/src/multiarray/nditer_constr.c
pub(crate) const CODE_NPY_NDITER_CONSTR3: &str = r#"/*
 * This file implements the construction, copying, and destruction
 * aspects of NumPy's nditer.
 *
 * Copyright (c) 2010-2011 by Mark Wiebe (mwwiebe@gmail.com)
 * The University of British Columbia
 *
 * Copyright (c) 2011 Enthought, Inc
 *
 * See LICENSE.txt for the license.
 */
#define NPY_NO_DEPRECATED_API NPY_API_VERSION

/* Allow this .c file to include nditer_impl.h */
#define NPY_ITERATOR_IMPLEMENTATION_CODE

#include "alloc.h"
#include "nditer_impl.h"
#include "arrayobject.h"
#include "array_coercion.h"
#include "templ_common.h"
#include "array_assign.h"
#include "dtype_traversal.h"


/* Internal helper functions private to this file */
static int
npyiter_check_global_flags(npy_uint32 flags, npy_uint32* itflags);
static int
npyiter_check_op_axes(int nop, int oa_ndim, int **op_axes,
                        const npy_intp *itershape);
static int
npyiter_calculate_ndim(int nop, PyArrayObject **op_in,
                       int oa_ndim);
static int
npyiter_check_per_op_flags(npy_uint32 flags, npyiter_opitflags *op_itflags);
static int
npyiter_prepare_one_operand(PyArrayObject **op,
                        char **op_dataptr,
                        PyArray_Descr *op_request_dtype,
                        PyArray_Descr** op_dtype,
                        npy_uint32 flags,
                        npy_uint32 op_flags, npyiter_opitflags *op_itflags);
static int
npyiter_prepare_operands(int nop,
                    PyArrayObject **op_in,
                    PyArrayObject **op,
                    char **op_dataptr,
                    PyArray_Descr **op_request_dtypes,
                    PyArray_Descr **op_dtype,
                    npy_uint32 flags,
                    npy_uint32 *op_flags, npyiter_opitflags *op_itflags,
                    int *out_maskop);
static int
npyiter_check_casting(int nop, PyArrayObject **op,
                    PyArray_Descr **op_dtype,
                    NPY_CASTING casting,
                    npyiter_opitflags *op_itflags);
static int
npyiter_fill_axisdata(NpyIter *iter, npy_uint32 flags, npyiter_opitflags *op_itflags,
                    char **op_dataptr,
                    const npy_uint32 *op_flags, int **op_axes,
                    npy_intp const *itershape);
static inline int
npyiter_get_op_axis(int axis, npy_bool *reduction_axis);
static void
npyiter_replace_axisdata(
        NpyIter *iter, int iop, PyArrayObject *op,
        int orig_op_ndim, const int *op_axes);
static void
npyiter_compute_index_strides(NpyIter *iter, npy_uint32 flags);
static void
npyiter_apply_forced_iteration_order(NpyIter *iter, NPY_ORDER order);
static void
npyiter_flip_negative_strides(NpyIter *iter);
static void
npyiter_reverse_axis_ordering(NpyIter *iter);
static void
npyiter_find_best_axis_ordering(NpyIter *iter);
static PyArray_Descr *
npyiter_get_common_dtype(int nop, PyArrayObject **op,
                        const npyiter_opitflags *op_itflags, PyArray_Descr **op_dtype,
                        PyArray_Descr **op_request_dtypes,
                        int only_inputs);
static PyArrayObject *
npyiter_new_temp_array(NpyIter *iter, PyTypeObject *subtype,
                npy_uint32 flags, npyiter_opitflags *op_itflags,
                int op_ndim, npy_intp const *shape,
                PyArray_Descr *op_dtype, const int *op_axes);
static int
npyiter_allocate_arrays(NpyIter *iter,
                        npy_uint32 flags,
                        PyArray_Descr **op_dtype, PyTypeObject *subtype,
                        const npy_uint32 *op_flags, npyiter_opitflags *op_itflags,
                        int **op_axes);
static void
npyiter_get_priority_subtype(int nop, PyArrayObject **op,
                            const npyiter_opitflags *op_itflags,
                            double *subtype_priority, PyTypeObject **subtype);
static int
npyiter_allocate_transfer_functions(NpyIter *iter);

static int
npyiter_find_buffering_setup(NpyIter *iter, npy_intp buffersize);

/*NUMPY_API
 * Allocate a new iterator for multiple array objects, and advanced
 * options for controlling the broadcasting, shape, and buffer size.
 */
NPY_NO_EXPORT NpyIter *
NpyIter_AdvancedNew(int nop, PyArrayObject **op_in, npy_uint32 flags,
                 NPY_ORDER order, NPY_CASTING casting,
                 npy_uint32 *op_flags,
                 PyArray_Descr **op_request_dtypes,
                 int oa_ndim, int **op_axes, npy_intp *itershape,
                 npy_intp buffersize)
{
    npy_uint32 itflags = NPY_ITFLAG_IDENTPERM;
    int idim, ndim;
    int iop;

    /* The iterator being constructed */
    NpyIter *iter;

    /* Per-operand values */
    PyArrayObject **op;
    PyArray_Descr **op_dtype;
    npyiter_opitflags *op_itflags;
    char **op_dataptr;

    npy_int8 *perm;
    NpyIter_BufferData *bufferdata = NULL;
    int any_allocate = 0, any_missing_dtypes = 0, need_subtype = 0;

    /* The subtype for automatically allocated outputs */
    double subtype_priority = NPY_PRIORITY;
    PyTypeObject *subtype = &PyArray_Type;

#if NPY_IT_CONSTRUCTION_TIMING
    npy_intp c_temp,
            c_start,
            c_check_op_axes,
            c_check_global_flags,
            c_calculate_ndim,
            c_malloc,
            c_prepare_operands,
            c_fill_axisdata,
            c_compute_index_strides,
            c_apply_forced_iteration_order,
            c_find_best_axis_ordering,
            c_get_priority_subtype,
            c_find_output_common_dtype,
            c_check_casting,
            c_allocate_arrays,
            c_coalesce_axes,
            c_prepare_buffers;
#endif

    NPY_IT_TIME_POINT(c_start);

    /*
     * Before 1.8, if `oa_ndim == 0`, this meant `op_axes != NULL` was an error.
     * With 1.8, `oa_ndim == -1` takes this role, while op_axes in that case
     * enforces a 0-d iterator. Using `oa_ndim == 0` with `op_axes == NULL`
     * is thus an error in 1.13 after deprecation.
     */
    if ((oa_ndim == 0) && (op_axes == NULL)) {
        PyErr_Format(PyExc_ValueError,
            "Using `oa_ndim == 0` when `op_axes` is NULL. "
            "Use `oa_ndim == -1` or the MultiNew "
            "iterator for NumPy <1.8 compatibility");
        return NULL;
    }

    /* Error check 'oa_ndim' and 'op_axes', which must be used together */
    if (!npyiter_check_op_axes(nop, oa_ndim, op_axes, itershape)) {
        return NULL;
    }

    NPY_IT_TIME_POINT(c_check_op_axes);

    /* Check the global iterator flags */
    if (!npyiter_check_global_flags(flags, &itflags)) {
        return NULL;
    }

    NPY_IT_TIME_POINT(c_check_global_flags);

    /* Calculate how many dimensions the iterator should have */
    ndim = npyiter_calculate_ndim(nop, op_in, oa_ndim);

    NPY_IT_TIME_POINT(c_calculate_ndim);

    /* Allocate memory for the iterator */
    iter = (NpyIter*)
                PyMem_Malloc(NIT_SIZEOF_ITERATOR(itflags, ndim, nop));
    if (iter == NULL) {
        return NULL;
    }

    NPY_IT_TIME_POINT(c_malloc);

    /* Fill in the basic data */
    NIT_ITFLAGS(iter) = itflags;
    NIT_NDIM(iter) = ndim;
    NIT_NOP(iter) = nop;
    NIT_MASKOP(iter) = -1;
    NIT_ITERINDEX(iter) = 0;
    memset(NIT_BASEOFFSETS(iter), 0, (nop+1)*NPY_SIZEOF_INTP);

    op = NIT_OPERANDS(iter);
    op_dtype = NIT_DTYPES(iter);
    op_itflags = NIT_OPITFLAGS(iter);
    op_dataptr = NIT_RESETDATAPTR(iter);

    /* Prepare all the operands */
    if (!npyiter_prepare_operands(nop, op_in, op, op_dataptr,
                        op_request_dtypes, op_dtype,
                        flags,
                        op_flags, op_itflags,
                        &NIT_MASKOP(iter))) {
        PyMem_Free(iter);
        return NULL;
    }
    /* Set resetindex to zero as well (it's just after the resetdataptr) */
    op_dataptr[nop] = 0;

    NPY_IT_TIME_POINT(c_prepare_operands);

    /*
     * Initialize buffer data (must set the buffers and transferdata
     * to NULL before we might deallocate the iterator).
     */
    if (itflags & NPY_ITFLAG_BUFFER) {
        bufferdata = NIT_BUFFERDATA(iter);
        NBF_SIZE(bufferdata) = 0;
        memset(NBF_BUFFERS(bufferdata), 0, nop*NPY_SIZEOF_INTP);
        /* Ensure that the transferdata/auxdata is NULLed */
        memset(NBF_TRANSFERINFO(bufferdata), 0, nop * sizeof(NpyIter_TransferInfo));
    }

    /* Fill in the AXISDATA arrays and set the ITERSIZE field */
    if (!npyiter_fill_axisdata(iter, flags, op_itflags, op_dataptr,
                                        op_flags, op_axes, itershape)) {
        NpyIter_Deallocate(iter);
        return NULL;
    }

    NPY_IT_TIME_POINT(c_fill_axisdata);

    /*
     * If an index was requested, compute the strides for it.
     * Note that we must do this before changing the order of the
     * axes
     */
    npyiter_compute_index_strides(iter, flags);

    NPY_IT_TIME_POINT(c_compute_index_strides);

    /* Initialize the perm to the identity */
    perm = NIT_PERM(iter);
    for(idim = 0; idim < ndim; ++idim) {
        perm[idim] = (npy_int8)idim;
    }

    /*
     * If an iteration order is being forced, apply it.
     */
    npyiter_apply_forced_iteration_order(iter, order);
    itflags = NIT_ITFLAGS(iter);

    NPY_IT_TIME_POINT(c_apply_forced_iteration_order);

    /* Set some flags for allocated outputs */
    for (iop = 0; iop < nop; ++iop) {
        if (op[iop] == NULL) {
            /* Flag this so later we can avoid flipping axes */
            any_allocate = 1;
            /* If a subtype may be used, indicate so */
            if (!(op_flags[iop] & NPY_ITER_NO_SUBTYPE)) {
                need_subtype = 1;
            }
            /*
             * If the data type wasn't provided, will need to
             * calculate it.
             */
            if (op_dtype[iop] == NULL) {
                any_missing_dtypes = 1;
            }
        }
    }

    /*
     * If the ordering was not forced, reorder the axes
     * and flip negative strides to find the best one.
     */
    if (!(itflags & NPY_ITFLAG_FORCEDORDER)) {
        if (ndim > 1) {
            npyiter_find_best_axis_ordering(iter);
        }
        /*
         * If there's an output being allocated, we must not negate
         * any strides.
         */
        if (!any_allocate && !(flags & NPY_ITER_DONT_NEGATE_STRIDES)) {
            npyiter_flip_negative_strides(iter);
        }
        itflags = NIT_ITFLAGS(iter);
    }

    NPY_IT_TIME_POINT(c_find_best_axis_ordering);

    if (need_subtype) {
        npyiter_get_priority_subtype(nop, op, op_itflags,
                                     &subtype_priority, &subtype);
    }

    NPY_IT_TIME_POINT(c_get_priority_subtype);

    /*
     * If an automatically allocated output didn't have a specified
     * dtype, we need to figure it out now, before allocating the outputs.
     */
    if (any_missing_dtypes || (flags & NPY_ITER_COMMON_DTYPE)) {
        PyArray_Descr *dtype;
        int only_inputs = !(flags & NPY_ITER_COMMON_DTYPE);

        op = NIT_OPERANDS(iter);
        op_dtype = NIT_DTYPES(iter);

        dtype = npyiter_get_common_dtype(nop, op,
                                    op_itflags, op_dtype,
                                    op_request_dtypes,
                                    only_inputs);
        if (dtype == NULL) {
            NpyIter_Deallocate(iter);
            return NULL;
        }
        if (flags & NPY_ITER_COMMON_DTYPE) {
            NPY_IT_DBG_PRINT("Iterator: Replacing all data types\n");
            /* Replace all the data types */
            for (iop = 0; iop < nop; ++iop) {
                if (op_dtype[iop] != dtype) {
                    Py_XDECREF(op_dtype[iop]);
                    Py_INCREF(dtype);
                    op_dtype[iop] = dtype;
                }
            }
        }
        else {
            NPY_IT_DBG_PRINT("Iterator: Setting unset output data types\n");
            /* Replace the NULL data types */
            for (iop = 0; iop < nop; ++iop) {
                if (op_dtype[iop] == NULL) {
                    Py_INCREF(dtype);
                    op_dtype[iop] = dtype;
                }
            }
        }
        Py_DECREF(dtype);
    }

    NPY_IT_TIME_POINT(c_find_output_common_dtype);

    /*
     * All of the data types have been settled, so it's time
     * to check that data type conversions are following the
     * casting rules.
     */
    if (!npyiter_check_casting(nop, op, op_dtype, casting, op_itflags)) {
        NpyIter_Deallocate(iter);
        return NULL;
    }

    NPY_IT_TIME_POINT(c_check_casting);

    /*
     * At this point, the iteration order has been finalized. so
     * any allocation of ops that were NULL, or any temporary
     * copying due to casting/byte order/alignment can be
     * done now using a memory layout matching the iterator.
     */
    if (!npyiter_allocate_arrays(iter, flags, op_dtype, subtype, op_flags,
                            op_itflags, op_axes)) {
        NpyIter_Deallocate(iter);
        return NULL;
    }

    NPY_IT_TIME_POINT(c_allocate_arrays);

    /*
     * Finally, if a multi-index wasn't requested,
     * it may be possible to coalesce some axes together.
     */
    if (ndim > 1 && !(itflags & NPY_ITFLAG_HASMULTIINDEX)) {
        npyiter_coalesce_axes(iter);
        /*
         * The operation may have changed the layout, so we have to
         * get the internal pointers again.
         */
        itflags = NIT_ITFLAGS(iter);
        ndim = NIT_NDIM(iter);
        op = NIT_OPERANDS(iter);
        op_dtype = NIT_DTYPES(iter);
        op_itflags = NIT_OPITFLAGS(iter);
        op_dataptr = NIT_RESETDATAPTR(iter);
    }

    NPY_IT_TIME_POINT(c_coalesce_axes);

    /*
     * Now that the axes are finished, check whether we can apply
     * the single iteration optimization to the iternext function.
     */
    if (!(itflags & NPY_ITFLAG_BUFFER)) {
        NpyIter_AxisData *axisdata = NIT_AXISDATA(iter);
        if (itflags & NPY_ITFLAG_EXLOOP) {
            if (NIT_ITERSIZE(iter) == NAD_SHAPE(axisdata)) {
                NIT_ITFLAGS(iter) |= NPY_ITFLAG_ONEITERATION;
            }
        }
        else if (NIT_ITERSIZE(iter) == 1) {
            NIT_ITFLAGS(iter) |= NPY_ITFLAG_ONEITERATION;
        }
    }

    /* If buffering is set prepare it */
    if (itflags & NPY_ITFLAG_BUFFER) {
        if (npyiter_find_buffering_setup(iter, buffersize) < 0) {
            NpyIter_Deallocate(iter);
            return NULL;
        }

        /*
         * Initialize for use in FirstVisit, which may be called before
         * the buffers are filled and the reduce pos is updated.
         */
        NBF_REDUCE_POS(bufferdata) = 0;

        if (!npyiter_allocate_transfer_functions(iter)) {
            NpyIter_Deallocate(iter);
            return NULL;
        }
        if (!(itflags & NPY_ITFLAG_DELAYBUF)) {
            /* Allocate the buffers if that is not delayed */
            if (!npyiter_allocate_buffers(iter, NULL)) {
                NpyIter_Deallocate(iter);
                return NULL;
            }

            /* Prepare the next buffers and set iterend/size */
            if (npyiter_copy_to_buffers(iter, NULL) < 0) {
                NpyIter_Deallocate(iter);
                return NULL;
            }
        }
    }
    else if (itflags&NPY_ITFLAG_EXLOOP)  {
        /* make sure to update the user pointers (when buffering, it does this). */
        assert(!(itflags & NPY_ITFLAG_HASINDEX));
        memcpy(NIT_USERPTRS(iter), NIT_DATAPTRS(iter), nop * sizeof(void *));
    }

    NPY_IT_TIME_POINT(c_prepare_buffers);

#if NPY_IT_CONSTRUCTION_TIMING
    printf("\nIterator construction timing:\n");
    NPY_IT_PRINT_TIME_START(c_start);
    NPY_IT_PRINT_TIME_VAR(c_check_op_axes);
    NPY_IT_PRINT_TIME_VAR(c_check_global_flags);
    NPY_IT_PRINT_TIME_VAR(c_calculate_ndim);
    NPY_IT_PRINT_TIME_VAR(c_malloc);
    NPY_IT_PRINT_TIME_VAR(c_prepare_operands);
    NPY_IT_PRINT_TIME_VAR(c_fill_axisdata);
    NPY_IT_PRINT_TIME_VAR(c_compute_index_strides);
    NPY_IT_PRINT_TIME_VAR(c_apply_forced_iteration_order);
    NPY_IT_PRINT_TIME_VAR(c_find_best_axis_ordering);
    NPY_IT_PRINT_TIME_VAR(c_get_priority_subtype);
    NPY_IT_PRINT_TIME_VAR(c_find_output_common_dtype);
    NPY_IT_PRINT_TIME_VAR(c_check_casting);
    NPY_IT_PRINT_TIME_VAR(c_allocate_arrays);
    NPY_IT_PRINT_TIME_VAR(c_coalesce_axes);
    NPY_IT_PRINT_TIME_VAR(c_prepare_buffers);
    printf("\n");
#endif

    return iter;
}

/*NUMPY_API
 * Allocate a new iterator for more than one array object, using
 * standard NumPy broadcasting rules and the default buffer size.
 */
NPY_NO_EXPORT NpyIter *
NpyIter_MultiNew(int nop, PyArrayObject **op_in, npy_uint32 flags,
                 NPY_ORDER order, NPY_CASTING casting,
                 npy_uint32 *op_flags,
                 PyArray_Descr **op_request_dtypes)
{
    return NpyIter_AdvancedNew(nop, op_in, flags, order, casting,
                            op_flags, op_request_dtypes,
                            -1, NULL, NULL, 0);
}

/*NUMPY_API
 * Allocate a new iterator for one array object.
 */
NPY_NO_EXPORT NpyIter *
NpyIter_New(PyArrayObject *op, npy_uint32 flags,
                  NPY_ORDER order, NPY_CASTING casting,
                  PyArray_Descr* dtype)
{
    /* Split the flags into separate global and op flags */
    npy_uint32 op_flags = flags & NPY_ITER_PER_OP_FLAGS;
    flags &= NPY_ITER_GLOBAL_FLAGS;

    return NpyIter_AdvancedNew(1, &op, flags, order, casting,
                            &op_flags, &dtype,
                            -1, NULL, NULL, 0);
}

/*NUMPY_API
 * Makes a copy of the iterator
 */
NPY_NO_EXPORT NpyIter *
NpyIter_Copy(NpyIter *iter)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int ndim = NIT_NDIM(iter);
    int iop, nop = NIT_NOP(iter);
    int out_of_memory = 0;

    npy_intp size;
    NpyIter *newiter;
    PyArrayObject **objects;
    PyArray_Descr **dtypes;

    /* Allocate memory for the new iterator */
    size = NIT_SIZEOF_ITERATOR(itflags, ndim, nop);
    newiter = (NpyIter*)PyMem_Malloc(size);
    if (newiter == NULL) {
        PyErr_NoMemory();
        return NULL;
    }

    /* Copy the raw values to the new iterator */
    memcpy(newiter, iter, size);

    /* Take ownership of references to the operands and dtypes */
    objects = NIT_OPERANDS(newiter);
    dtypes = NIT_DTYPES(newiter);
    for (iop = 0; iop < nop; ++iop) {
        Py_INCREF(objects[iop]);
        Py_INCREF(dtypes[iop]);
    }

    /* Allocate buffers and make copies of the transfer data if necessary */
    if (itflags & NPY_ITFLAG_BUFFER) {
        NpyIter_BufferData *bufferdata;
        npy_intp buffersize, itemsize;
        char **buffers;

        bufferdata = NIT_BUFFERDATA(newiter);
        buffers = NBF_BUFFERS(bufferdata);
        buffersize = NBF_BUFFERSIZE(bufferdata);
        NpyIter_TransferInfo *transferinfo = NBF_TRANSFERINFO(bufferdata);

        for (iop = 0; iop < nop; ++iop) {
            if (buffers[iop] != NULL) {
                if (out_of_memory) {
                    buffers[iop] = NULL;
                }
                else {
                    itemsize = dtypes[iop]->elsize;
                    buffers[iop] = PyArray_malloc(itemsize*buffersize);
                    if (buffers[iop] == NULL) {
                        out_of_memory = 1;
                    }
                    else {
                        if (PyDataType_FLAGCHK(dtypes[iop], NPY_NEEDS_INIT)) {
                            memset(buffers[iop], '\0', itemsize*buffersize);
                        }
                    }
                }
            }

            if (transferinfo[iop].read.func != NULL) {
                if (out_of_memory) {
                    transferinfo[iop].read.func = NULL;  /* No cleanup */
                }
                else {
                    if (NPY_cast_info_copy(&transferinfo[iop].read,
                                           &transferinfo[iop].read) < 0) {
                        out_of_memory = 1;
                    }
                }
            }

            if (transferinfo[iop].write.func != NULL) {
                if (out_of_memory) {
                    transferinfo[iop].write.func = NULL;  /* No cleanup */
                }
                else {
                    if (NPY_cast_info_copy(&transferinfo[iop].write,
                                           &transferinfo[iop].write) < 0) {
                        out_of_memory = 1;
                    }
                }
            }

            if (transferinfo[iop].clear.func != NULL) {
                if (out_of_memory) {
                    transferinfo[iop].clear.func = NULL;  /* No cleanup */
                }
                else {
                    if (NPY_traverse_info_copy(&transferinfo[iop].clear,
                                               &transferinfo[iop].clear) < 0) {
                        out_of_memory = 1;
                    }
                }
            }
        }

        /* Initialize the buffers to the current iterindex */
        if (!out_of_memory && NBF_SIZE(bufferdata) > 0) {
            npyiter_goto_iterindex(newiter, NIT_ITERINDEX(newiter));

            /* Prepare the next buffers and set iterend/size */
            npyiter_copy_to_buffers(newiter, NULL);
        }
    }

    if (out_of_memory) {
        NpyIter_Deallocate(newiter);
        PyErr_NoMemory();
        return NULL;
    }

    return newiter;
}

/*NUMPY_API
 * Deallocate an iterator.
 *
 * To correctly work when an error is in progress, we have to check
 * `PyErr_Occurred()`. This is necessary when buffers are not finalized
 * or WritebackIfCopy is used. We could avoid that check by exposing a new
 * function which is passed in whether or not a Python error is already set.
 */
NPY_NO_EXPORT int
NpyIter_Deallocate(NpyIter *iter)
{
    int success = PyErr_Occurred() == NULL;

    npy_uint32 itflags;
    /*int ndim = NIT_NDIM(iter);*/
    int iop, nop;
    PyArray_Descr **dtype;
    PyArrayObject **object;
    npyiter_opitflags *op_itflags;

    if (iter == NULL) {
        return success;
    }

    itflags = NIT_ITFLAGS(iter);
    nop = NIT_NOP(iter);
    dtype = NIT_DTYPES(iter);
    object = NIT_OPERANDS(iter);
    op_itflags = NIT_OPITFLAGS(iter);

    /* Deallocate any buffers and buffering data */
    if (itflags & NPY_ITFLAG_BUFFER) {
        /* Ensure no data is held by the buffers before they are cleared */
        if (success) {
            if (npyiter_copy_from_buffers(iter) < 0) {
                success = NPY_FAIL;
            }
        }
        else {
            npyiter_clear_buffers(iter);
        }

        NpyIter_BufferData *bufferdata = NIT_BUFFERDATA(iter);
        char **buffers;

        /* buffers */
        buffers = NBF_BUFFERS(bufferdata);
        for (iop = 0; iop < nop; ++iop, ++buffers) {
            PyArray_free(*buffers);
        }

        NpyIter_TransferInfo *transferinfo = NBF_TRANSFERINFO(bufferdata);
        /* read bufferdata */
        for (iop = 0; iop < nop; ++iop, ++transferinfo) {
            NPY_cast_info_xfree(&transferinfo->read);
            NPY_cast_info_xfree(&transferinfo->write);
            NPY_traverse_info_xfree(&transferinfo->clear);
        }
    }

    /*
     * Deallocate all the dtypes and objects that were iterated and resolve
     * any writeback buffers created by the iterator.
     */
    for (iop = 0; iop < nop; ++iop, ++dtype, ++object) {
        if (op_itflags[iop] & NPY_OP_ITFLAG_HAS_WRITEBACK) {
            if (success && PyArray_ResolveWritebackIfCopy(*object) < 0) {
                success = 0;
            }
            else {
                PyArray_DiscardWritebackIfCopy(*object);
            }
        }
        Py_XDECREF(*dtype);
        Py_XDECREF(*object);
    }

    /* Deallocate the iterator memory */
    PyMem_Free(iter);
    return success;
}


/* Checks 'flags' for (C|F)_ORDER_INDEX, MULTI_INDEX, and EXTERNAL_LOOP,
 * setting the appropriate internal flags in 'itflags'.
 *
 * Returns 1 on success, 0 on error.
 */
static int
npyiter_check_global_flags(npy_uint32 flags, npy_uint32* itflags)
{
    if ((flags & NPY_ITER_PER_OP_FLAGS) != 0) {
        PyErr_SetString(PyExc_ValueError,
                    "A per-operand flag was passed as a global flag "
                    "to the iterator constructor");
        return 0;
    }

    /* Check for an index */
    if (flags & (NPY_ITER_C_INDEX | NPY_ITER_F_INDEX)) {
        if ((flags & (NPY_ITER_C_INDEX | NPY_ITER_F_INDEX)) ==
                    (NPY_ITER_C_INDEX | NPY_ITER_F_INDEX)) {
            PyErr_SetString(PyExc_ValueError,
                    "Iterator flags C_INDEX and "
                    "F_INDEX cannot both be specified");
            return 0;
        }
        (*itflags) |= NPY_ITFLAG_HASINDEX;
    }
    /* Check if a multi-index was requested */
    if (flags & NPY_ITER_MULTI_INDEX) {
        /*
         * This flag primarily disables dimension manipulations that
         * would produce an incorrect multi-index.
         */
        (*itflags) |= NPY_ITFLAG_HASMULTIINDEX;
    }
    /* Check if the caller wants to handle inner iteration */
    if (flags & NPY_ITER_EXTERNAL_LOOP) {
        if ((*itflags) & (NPY_ITFLAG_HASINDEX | NPY_ITFLAG_HASMULTIINDEX)) {
            PyErr_SetString(PyExc_ValueError,
                    "Iterator flag EXTERNAL_LOOP cannot be used "
                    "if an index or multi-index is being tracked");
            return 0;
        }
        (*itflags) |= NPY_ITFLAG_EXLOOP;
    }
    /* Ranged */
    if (flags & NPY_ITER_RANGED) {
        (*itflags) |= NPY_ITFLAG_RANGE;
        if ((flags & NPY_ITER_EXTERNAL_LOOP) &&
                                    !(flags & NPY_ITER_BUFFERED)) {
            PyErr_SetString(PyExc_ValueError,
                    "Iterator flag RANGED cannot be used with "
                    "the flag EXTERNAL_LOOP unless "
                    "BUFFERED is also enabled");
            return 0;
        }
    }
    /* Buffering */
    if (flags & NPY_ITER_BUFFERED) {
        (*itflags) |= NPY_ITFLAG_BUFFER;
        if (flags & NPY_ITER_GROWINNER) {
            (*itflags) |= NPY_ITFLAG_GROWINNER;
        }
        if (flags & NPY_ITER_DELAY_BUFALLOC) {
            (*itflags) |= NPY_ITFLAG_DELAYBUF;
        }
    }

    return 1;
}

static int
npyiter_check_op_axes(int nop, int oa_ndim, int **op_axes,
                        const npy_intp *itershape)
{
    char axes_dupcheck[NPY_MAXDIMS];
    int iop, idim;

    if (oa_ndim < 0) {
        /*
         * If `oa_ndim < 0`, `op_axes` and `itershape` are signalled to
         * be unused and should be NULL. (Before NumPy 1.8 this was
         * signalled by `oa_ndim == 0`.)
         */
        if (op_axes != NULL || itershape != NULL) {
            PyErr_Format(PyExc_ValueError,
                    "If 'op_axes' or 'itershape' is not NULL in the iterator "
                    "constructor, 'oa_ndim' must be zero or greater");
            return 0;
        }
        return 1;
    }
    if (oa_ndim > NPY_MAXDIMS) {
        PyErr_Format(PyExc_ValueError,
                "Cannot construct an iterator with more than %d dimensions "
                "(%d were requested for op_axes)",
                NPY_MAXDIMS, oa_ndim);
        return 0;
    }
    if (op_axes == NULL) {
        PyErr_Format(PyExc_ValueError,
                "If 'oa_ndim' is zero or greater in the iterator "
                "constructor, then op_axes cannot be NULL");
        return 0;
    }

    /* Check that there are no duplicates in op_axes */
    for (iop = 0; iop < nop; ++iop) {
        int *axes = op_axes[iop];
        if (axes != NULL) {
            memset(axes_dupcheck, 0, NPY_MAXDIMS);
            for (idim = 0; idim < oa_ndim; ++idim) {
                int i = npyiter_get_op_axis(axes[idim], NULL);

                if (i >= 0) {
                    if (i >= NPY_MAXDIMS) {
                        PyErr_Format(PyExc_ValueError,
                                "The 'op_axes' provided to the iterator "
                                "constructor for operand %d "
                                "contained invalid "
                                "values %d", iop, i);
                        return 0;
                    }
                    else if (axes_dupcheck[i] == 1) {
                        PyErr_Format(PyExc_ValueError,
                                "The 'op_axes' provided to the iterator "
                                "constructor for operand %d "
                                "contained duplicate "
                                "value %d", iop, i);
                        return 0;
                    }
                    else {
                        axes_dupcheck[i] = 1;
                    }
                }
            }
        }
    }

    return 1;
}

static int
npyiter_calculate_ndim(int nop, PyArrayObject **op_in,
                       int oa_ndim)
{
    /* If 'op_axes' is being used, force 'ndim' */
    if (oa_ndim >= 0 ) {
        return oa_ndim;
    }
    /* Otherwise it's the maximum 'ndim' from the operands */
    else {
        int ndim = 0, iop;

        for (iop = 0; iop < nop; ++iop) {
            if (op_in[iop] != NULL) {
                int ondim = PyArray_NDIM(op_in[iop]);
                if (ondim > ndim) {
                    ndim = ondim;
                }
            }

        }

        return ndim;
    }
}

/*
 * Checks the per-operand input flags, and fills in op_itflags.
 *
 * Returns 1 on success, 0 on failure.
 */
static int
npyiter_check_per_op_flags(npy_uint32 op_flags, npyiter_opitflags *op_itflags)
{
    if ((op_flags & NPY_ITER_GLOBAL_FLAGS) != 0) {
        PyErr_SetString(PyExc_ValueError,
                    "A global iterator flag was passed as a per-operand flag "
                    "to the iterator constructor");
        return 0;
    }

    /* Check the read/write flags */
    if (op_flags & NPY_ITER_READONLY) {
        /* The read/write flags are mutually exclusive */
        if (op_flags & (NPY_ITER_READWRITE|NPY_ITER_WRITEONLY)) {
            PyErr_SetString(PyExc_ValueError,
                    "Only one of the iterator flags READWRITE, "
                    "READONLY, and WRITEONLY may be "
                    "specified for an operand");
            return 0;
        }

        *op_itflags = NPY_OP_ITFLAG_READ;
    }
    else if (op_flags & NPY_ITER_READWRITE) {
        /* The read/write flags are mutually exclusive */
        if (op_flags & NPY_ITER_WRITEONLY) {
            PyErr_SetString(PyExc_ValueError,
                    "Only one of the iterator flags READWRITE, "
                    "READONLY, and WRITEONLY may be "
                    "specified for an operand");
            return 0;
        }

        *op_itflags = NPY_OP_ITFLAG_READ|NPY_OP_ITFLAG_WRITE;
    }
    else if(op_flags & NPY_ITER_WRITEONLY) {
        *op_itflags = NPY_OP_ITFLAG_WRITE;
    }
    else {
        PyErr_SetString(PyExc_ValueError,
                "None of the iterator flags READWRITE, "
                "READONLY, or WRITEONLY were "
                "specified for an operand");
        return 0;
    }

    /* Check the flags for temporary copies */
    if (((*op_itflags) & NPY_OP_ITFLAG_WRITE) &&
                (op_flags & (NPY_ITER_COPY |
                           NPY_ITER_UPDATEIFCOPY)) == NPY_ITER_COPY) {
        PyErr_SetString(PyExc_ValueError,
                "If an iterator operand is writeable, must use "
                "the flag UPDATEIFCOPY instead of "
                "COPY");
        return 0;
    }

    /* Check the flag for a write masked operands */
    if (op_flags & NPY_ITER_WRITEMASKED) {
        if (!((*op_itflags) & NPY_OP_ITFLAG_WRITE)) {
            PyErr_SetString(PyExc_ValueError,
                "The iterator flag WRITEMASKED may only "
                "be used with READWRITE or WRITEONLY");
            return 0;
        }
        if ((op_flags & NPY_ITER_ARRAYMASK) != 0) {
            PyErr_SetString(PyExc_ValueError,
                "The iterator flag WRITEMASKED may not "
                "be used together with ARRAYMASK");
            return 0;
        }
        *op_itflags |= NPY_OP_ITFLAG_WRITEMASKED;
    }

    if ((op_flags & NPY_ITER_VIRTUAL) != 0) {
        if ((op_flags & NPY_ITER_READWRITE) == 0) {
            PyErr_SetString(PyExc_ValueError,
                "The iterator flag VIRTUAL should be "
                "be used together with READWRITE");
            return 0;
        }
        *op_itflags |= NPY_OP_ITFLAG_VIRTUAL;
    }

    if (op_flags & NPY_ITER_CONTIG) {
        *op_itflags |= NPY_OP_ITFLAG_CONTIG;
    }

    return 1;
}

/*
 * Prepares a constructor operand.  Assumes a reference to 'op'
 * is owned, and that 'op' may be replaced.  Fills in 'op_dataptr',
 * 'op_dtype', and may modify 'op_itflags'.
 *
 * Returns 1 on success, 0 on failure.
 */
static int
npyiter_prepare_one_operand(PyArrayObject **op,
                        char **op_dataptr,
                        PyArray_Descr *op_request_dtype,
                        PyArray_Descr **op_dtype,
                        npy_uint32 flags,
                        npy_uint32 op_flags, npyiter_opitflags *op_itflags)
{
    /* NULL operands must be automatically allocated outputs */
    if (*op == NULL) {
        /* ALLOCATE or VIRTUAL should be enabled */
        if ((op_flags & (NPY_ITER_ALLOCATE|NPY_ITER_VIRTUAL)) == 0) {
            PyErr_SetString(PyExc_ValueError,
                    "Iterator operand was NULL, but neither the "
                    "ALLOCATE nor the VIRTUAL flag was specified");
            return 0;
        }

        if (op_flags & NPY_ITER_ALLOCATE) {
            /* Writing should be enabled */
            if (!((*op_itflags) & NPY_OP_ITFLAG_WRITE)) {
                PyErr_SetString(PyExc_ValueError,
                        "Automatic allocation was requested for an iterator "
                        "operand, but it wasn't flagged for writing");
                return 0;
            }
            /*
             * Reading should be disabled if buffering is enabled without
             * also enabling NPY_ITER_DELAY_BUFALLOC.  In all other cases,
             * the caller may initialize the allocated operand to a value
             * before beginning iteration.
             */
            if (((flags & (NPY_ITER_BUFFERED |
                            NPY_ITER_DELAY_BUFALLOC)) == NPY_ITER_BUFFERED) &&
                    ((*op_itflags) & NPY_OP_ITFLAG_READ)) {
                PyErr_SetString(PyExc_ValueError,
                        "Automatic allocation was requested for an iterator "
                        "operand, and it was flagged as readable, but "
                        "buffering  without delayed allocation was enabled");
                return 0;
            }

            /* If a requested dtype was provided, use it, otherwise NULL */
            Py_XINCREF(op_request_dtype);
            *op_dtype = op_request_dtype;
        }
        else {
            *op_dtype = NULL;
        }

        /* Specify bool if no dtype was requested for the mask */
        if (op_flags & NPY_ITER_ARRAYMASK) {
            if (*op_dtype == NULL) {
                *op_dtype = PyArray_DescrFromType(NPY_BOOL);
                if (*op_dtype == NULL) {
                    return 0;
                }
            }
        }

        *op_dataptr = NULL;

        return 1;
    }

    /* VIRTUAL operands must be NULL */
    if (op_flags & NPY_ITER_VIRTUAL) {
        PyErr_SetString(PyExc_ValueError,
                "Iterator operand flag VIRTUAL was specified, "
                "but the operand was not NULL");
        return 0;
    }


    if (PyArray_Check(*op)) {

        if ((*op_itflags) & NPY_OP_ITFLAG_WRITE
            && PyArray_FailUnlessWriteable(*op, "operand array with iterator "
                                           "write flag set") < 0) {
            return 0;
        }
        if (!(flags & NPY_ITER_ZEROSIZE_OK) && PyArray_SIZE(*op) == 0) {
            PyErr_SetString(PyExc_ValueError,
                    "Iteration of zero-sized operands is not enabled");
            return 0;
        }
        *op_dataptr = PyArray_BYTES(*op);

        /*
         * Checking whether casts are valid is done later, once the
         * final data types have been selected.  For now, just store the
         * requested type.
         */
        if (op_request_dtype != NULL && op_request_dtype != PyArray_DESCR(*op)) {
            /* We just have a borrowed reference to op_request_dtype */
            *op_dtype = PyArray_AdaptDescriptorToArray(
                                            *op, NULL, op_request_dtype);
            if (*op_dtype == NULL) {
                return 0;
            }
        }
        else {
            *op_dtype = PyArray_DESCR(*op);
            Py_INCREF(*op_dtype);
        }

        /*
         * If references weren't specifically allowed, make sure there
         * are no references in the inputs or requested dtypes.
         */
        if (!(flags & NPY_ITER_REFS_OK)) {
            PyArray_Descr *dt = PyArray_DESCR(*op);
            if (((dt->flags & (NPY_ITEM_REFCOUNT |
                           NPY_ITEM_IS_POINTER)) != 0) ||
                    (dt != *op_dtype &&
                        (((*op_dtype)->flags & (NPY_ITEM_REFCOUNT |
                                             NPY_ITEM_IS_POINTER))) != 0)) {
                PyErr_SetString(PyExc_TypeError,
                        "Iterator operand or requested dtype holds "
                        "references, but the NPY_ITER_REFS_OK flag was not "
                        "enabled");
                return 0;
            }
        }

        /* Check if the operand is in the byte order requested */
        if (op_flags & NPY_ITER_NBO) {
            /* Check byte order */
            if (!PyArray_ISNBO((*op_dtype)->byteorder)) {
                /* Replace with a new descr which is in native byte order */
                Py_SETREF(*op_dtype,
                          PyArray_DescrNewByteorder(*op_dtype, NPY_NATIVE));
                if (*op_dtype == NULL) {
                    return 0;
                }
                NPY_IT_DBG_PRINT("Iterator: Setting NPY_OP_ITFLAG_CAST "
                                    "because of NPY_ITER_NBO\n");
                /* Indicate that byte order or alignment needs fixing */
                *op_itflags |= NPY_OP_ITFLAG_CAST;
            }
        }
        /* Check if the operand is aligned */
        if (op_flags & NPY_ITER_ALIGNED) {
            /* Check alignment */
            if (!PyArray_ISALIGNED(*op)) {
                NPY_IT_DBG_PRINT("Iterator: Setting NPY_OP_ITFLAG_CAST "
                                    "because of NPY_ITER_ALIGNED\n");
                *op_itflags |= NPY_OP_ITFLAG_CAST;
            }
        }
        /*
         * The check for NPY_ITER_CONTIG can only be done later,
         * once the final iteration order is settled.
         */
    }
    else {
        PyErr_SetString(PyExc_ValueError,
                "Iterator inputs must be ndarrays");
        return 0;
    }

    return 1;
}

/*
 * Process all the operands, copying new references so further processing
 * can replace the arrays if copying is necessary.
 */
static int
npyiter_prepare_operands(int nop, PyArrayObject **op_in,
                    PyArrayObject **op,
                    char **op_dataptr,
                    PyArray_Descr **op_request_dtypes,
                    PyArray_Descr **op_dtype,
                    npy_uint32 flags,
                    npy_uint32 *op_flags, npyiter_opitflags *op_itflags,
                    int *out_maskop)
{
    int iop, i;
    int maskop = -1;
    int any_writemasked_ops = 0;

    /*
     * Here we just prepare the provided operands.
     */
    for (iop = 0; iop < nop; ++iop) {
        op[iop] = op_in[iop];
        Py_XINCREF(op[iop]);
        op_dtype[iop] = NULL;

        /* Check the readonly/writeonly flags, and fill in op_itflags */
        if (!npyiter_check_per_op_flags(op_flags[iop], &op_itflags[iop])) {
            goto fail_iop;
        }

        /* Extract the operand which is for masked iteration */
        if ((op_flags[iop] & NPY_ITER_ARRAYMASK) != 0) {
            if (maskop != -1) {
                PyErr_SetString(PyExc_ValueError,
                        "Only one iterator operand may receive an "
                        "ARRAYMASK flag");
                goto fail_iop;
            }

            maskop = iop;
            *out_maskop = iop;
        }

        if (op_flags[iop] & NPY_ITER_WRITEMASKED) {
            any_writemasked_ops = 1;
        }

        /*
         * Prepare the operand.  This produces an op_dtype[iop] reference
         * on success.
         */
        if (!npyiter_prepare_one_operand(&op[iop],
                        &op_dataptr[iop],
                        op_request_dtypes ? op_request_dtypes[iop] : NULL,
                        &op_dtype[iop],
                        flags,
                        op_flags[iop], &op_itflags[iop])) {
            goto fail_iop;
        }
    }

    if (any_writemasked_ops && maskop < 0) {
        PyErr_SetString(PyExc_ValueError,
                "An iterator operand was flagged as WRITEMASKED, "
                "but no ARRAYMASK operand was given to supply "
                "the mask");
        goto fail_nop;
    }
    else if (!any_writemasked_ops && maskop >= 0) {
        PyErr_SetString(PyExc_ValueError,
                "An iterator operand was flagged as the ARRAYMASK, "
                "but no WRITEMASKED operands were given to use "
                "the mask");
        goto fail_nop;
    }

    return 1;

  fail_nop:
    iop = nop - 1;
  fail_iop:
    for (i = 0; i < iop+1; ++i) {
        Py_XDECREF(op[i]);
        Py_XDECREF(op_dtype[i]);
    }
    return 0;
}

static const char *
npyiter_casting_to_string(NPY_CASTING casting)
{
    switch (casting) {
        case NPY_NO_CASTING:
            return "'no'";
        case NPY_EQUIV_CASTING:
            return "'equiv'";
        case NPY_SAFE_CASTING:
            return "'safe'";
        case NPY_SAME_KIND_CASTING:
            return "'same_kind'";
        case NPY_UNSAFE_CASTING:
            return "'unsafe'";
        default:
            return "<unknown>";
    }
}


static int
npyiter_check_casting(int nop, PyArrayObject **op,
                    PyArray_Descr **op_dtype,
                    NPY_CASTING casting,
                    npyiter_opitflags *op_itflags)
{
    int iop;

    for(iop = 0; iop < nop; ++iop) {
        NPY_IT_DBG_PRINT1("Iterator: Checking casting for operand %d\n",
                            (int)iop);
#if NPY_IT_DBG_TRACING
        printf("op: ");
        if (op[iop] != NULL) {
            PyObject_Print((PyObject *)PyArray_DESCR(op[iop]), stdout, 0);
        }
        else {
            printf("<null>");
        }
        printf(", iter: ");
        PyObject_Print((PyObject *)op_dtype[iop], stdout, 0);
        printf("\n");
#endif
        /* If the types aren't equivalent, a cast is necessary */
        npy_intp view_offset = NPY_MIN_INTP;
        if (op[iop] != NULL && !(PyArray_SafeCast(
                    PyArray_DESCR(op[iop]), op_dtype[iop], &view_offset,
                    NPY_NO_CASTING, 1) && view_offset == 0)) {
            /* Check read (op -> temp) casting */
            if ((op_itflags[iop] & NPY_OP_ITFLAG_READ) &&
                        !PyArray_CanCastArrayTo(op[iop],
                                          op_dtype[iop],
                                          casting)) {
                PyErr_Format(PyExc_TypeError,
                        "Iterator operand %d dtype could not be cast from "
                        "%R to %R according to the rule %s",
                        iop, PyArray_DESCR(op[iop]), op_dtype[iop],
                        npyiter_casting_to_string(casting));
                return 0;
            }
            /* Check write (temp -> op) casting */
            if ((op_itflags[iop] & NPY_OP_ITFLAG_WRITE) &&
                        !PyArray_CanCastTypeTo(op_dtype[iop],
                                          PyArray_DESCR(op[iop]),
                                          casting)) {
                PyErr_Format(PyExc_TypeError,
                        "Iterator requested dtype could not be cast from "
                        "%R to %R, the operand %d dtype, "
                        "according to the rule %s",
                        op_dtype[iop], PyArray_DESCR(op[iop]), iop,
                        npyiter_casting_to_string(casting));
                return 0;
            }

            NPY_IT_DBG_PRINT("Iterator: Setting NPY_OP_ITFLAG_CAST "
                                "because the types aren't equivalent\n");
            /* Indicate that this operand needs casting */
            op_itflags[iop] |= NPY_OP_ITFLAG_CAST;
        }
    }

    return 1;
}

/*
 * Checks that the mask broadcasts to the WRITEMASK REDUCE
 * operand 'iop', but 'iop' never broadcasts to the mask.
 * If 'iop' broadcasts to the mask, the result would be more
 * than one mask value per reduction element, something which
 * is invalid.
 *
 * This check should only be called after all the operands
 * have been filled in.
 *
 * Returns 1 on success, 0 on error.
 */
static int
check_mask_for_writemasked_reduction(NpyIter *iter, int iop)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int nop = NIT_NOP(iter);
    int maskop = NIT_MASKOP(iter);

    NpyIter_AxisData *axisdata;
    npy_intp sizeof_axisdata;

    axisdata = NIT_AXISDATA(iter);
    sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);

    for(idim = 0; idim < ndim; ++idim) {
        npy_intp maskstride, istride;

        istride = NAD_STRIDES(axisdata)[iop];
        maskstride = NAD_STRIDES(axisdata)[maskop];

        /*
         * If 'iop' is being broadcast to 'maskop', we have
         * the invalid situation described above.
         */
        if (maskstride != 0 && istride == 0) {
            PyErr_SetString(PyExc_ValueError,
                    "Iterator reduction operand is WRITEMASKED, "
                    "but also broadcasts to multiple mask values. "
                    "There can be only one mask value per WRITEMASKED "
                    "element.");
            return 0;
        }

        NIT_ADVANCE_AXISDATA(axisdata, 1);
    }

    return 1;
}

/*
 * Check whether a reduction is OK based on the flags and the operand being
 * readwrite. This path is deprecated, since usually only specific axes
 * should be reduced. If axes are specified explicitly, the flag is
 * unnecessary.
 */
static int
npyiter_check_reduce_ok_and_set_flags(
        NpyIter *iter, npy_uint32 flags, npyiter_opitflags *op_itflags,
        int iop, int maskop, int dim) {
    /* If it's writeable, this means a reduction */
    if (op_itflags[iop] & NPY_OP_ITFLAG_WRITE) {
        if (!(flags & NPY_ITER_REDUCE_OK)) {
            PyErr_Format(PyExc_ValueError,
                    "output operand requires a reduction along dimension %d, "
                    "but the reduction is not enabled. The dimension size of 1 "
                    "does not match the expected output shape.", dim);
            return 0;
        }
        if (!(op_itflags[iop] & NPY_OP_ITFLAG_READ)) {
            PyErr_SetString(PyExc_ValueError,
                    "output operand requires a reduction, but is flagged as "
                    "write-only, not read-write");
            return 0;
        }
        /*
         * The ARRAYMASK can't be a reduction, because
         * it would be possible to write back to the
         * array once when the ARRAYMASK says 'True',
         * then have the reduction on the ARRAYMASK
         * later flip to 'False', indicating that the
         * write back should never have been done,
         * and violating the strict masking semantics
         */
        if (iop == maskop) {
            PyErr_SetString(PyExc_ValueError,
                    "output operand requires a "
                    "reduction, but is flagged as "
                    "the ARRAYMASK operand which "
                    "is not permitted to be the "
                    "result of a reduction");
            return 0;
        }
        NPY_IT_DBG_PRINT("Iterator: Indicating that a reduction is"
                         "occurring\n");

        NIT_ITFLAGS(iter) |= NPY_ITFLAG_REDUCE;
        op_itflags[iop] |= NPY_OP_ITFLAG_REDUCE;
    }
    return 1;
}

/**
 * Removes the (additive) NPY_ITER_REDUCTION_AXIS indication and sets
 * is_forced_broadcast to 1 if it is set. Otherwise to 0.
 *
 * @param axis The op_axes[i] to normalize.
 * @param reduction_axis Output 1 if a reduction axis, otherwise 0.
 * @returns The normalized axis (without reduce axis flag).
 */
static inline int
npyiter_get_op_axis(int axis, npy_bool *reduction_axis) {
    npy_bool forced_broadcast = axis >= NPY_ITER_REDUCTION_AXIS(-1);

    if (reduction_axis != NULL) {
        *reduction_axis = forced_broadcast;
    }
    if (forced_broadcast) {
        return axis - NPY_ITER_REDUCTION_AXIS(0);
    }
    return axis;
}

/*
 * Fills in the AXISDATA for the 'nop' operands, broadcasting
 * the dimensionas as necessary.  Also fills
 * in the ITERSIZE data member.
 *
 * If op_axes is not NULL, it should point to an array of ndim-sized
 * arrays, one for each op.
 *
 * Returns 1 on success, 0 on failure.
 */
static int
npyiter_fill_axisdata(NpyIter *iter, npy_uint32 flags, npyiter_opitflags *op_itflags,
                    char **op_dataptr,
                    const npy_uint32 *op_flags, int **op_axes,
                    npy_intp const *itershape)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int iop, nop = NIT_NOP(iter);
    int maskop = NIT_MASKOP(iter);

    int ondim;
    NpyIter_AxisData *axisdata;
    npy_intp sizeof_axisdata;
    PyArrayObject **op = NIT_OPERANDS(iter), *op_cur;
    npy_intp broadcast_shape[NPY_MAXDIMS];

    /* First broadcast the shapes together */
    if (itershape == NULL) {
        for (idim = 0; idim < ndim; ++idim) {
            broadcast_shape[idim] = 1;
        }
    }
    else {
        for (idim = 0; idim < ndim; ++idim) {
            broadcast_shape[idim] = itershape[idim];
            /* Negative shape entries are deduced from the operands */
            if (broadcast_shape[idim] < 0) {
                broadcast_shape[idim] = 1;
            }
        }
    }
    for (iop = 0; iop < nop; ++iop) {
        op_cur = op[iop];
        if (op_cur != NULL) {
            npy_intp *shape = PyArray_DIMS(op_cur);
            ondim = PyArray_NDIM(op_cur);

            if (op_axes == NULL || op_axes[iop] == NULL) {
                /*
                 * Possible if op_axes are being used, but
                 * op_axes[iop] is NULL
                 */
                if (ondim > ndim) {
                    PyErr_SetString(PyExc_ValueError,
                            "input operand has more dimensions than allowed "
                            "by the axis remapping");
                    return 0;
                }
                for (idim = 0; idim < ondim; ++idim) {
                    npy_intp bshape = broadcast_shape[idim+ndim-ondim];
                    npy_intp op_shape = shape[idim];

                    if (bshape == 1) {
                        broadcast_shape[idim+ndim-ondim] = op_shape;
                    }
                    else if (bshape != op_shape && op_shape != 1) {
                        goto broadcast_error;
                    }
                }
            }
            else {
                int *axes = op_axes[iop];
                for (idim = 0; idim < ndim; ++idim) {
                    int i = npyiter_get_op_axis(axes[idim], NULL);

                    if (i >= 0) {
                        if (i < ondim) {
                            npy_intp bshape = broadcast_shape[idim];
                            npy_intp op_shape = shape[i];

                            if (bshape == 1) {
                                broadcast_shape[idim] = op_shape;
                            }
                            else if (bshape != op_shape && op_shape != 1) {
                                goto broadcast_error;
                            }
                        }
                        else {
                            PyErr_Format(PyExc_ValueError,
                                    "Iterator input op_axes[%d][%d] (==%d) "
                                    "is not a valid axis of op[%d], which "
                                    "has %d dimensions ",
                                    iop, (ndim-idim-1), i,
                                    iop, ondim);
                            return 0;
                        }
                    }
                }
            }
        }
    }
    /*
     * If a shape was provided with a 1 entry, make sure that entry didn't
     * get expanded by broadcasting.
     */
    if (itershape != NULL) {
        for (idim = 0; idim < ndim; ++idim) {
            if (itershape[idim] == 1 && broadcast_shape[idim] != 1) {
                goto broadcast_error;
            }
        }
    }

    axisdata = NIT_AXISDATA(iter);
    sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);

    memcpy(NIT_DATAPTRS(iter), op_dataptr, nop * sizeof(void *));
    if (ndim == 0) {
        /* Need to fill the first axisdata, even if the iterator is 0-d */
        NAD_SHAPE(axisdata) = 1;
        NAD_INDEX(axisdata) = 0;
        memset(NAD_STRIDES(axisdata), 0, NPY_SIZEOF_INTP*nop);
    }

    /* Now process the operands, filling in the axisdata */
    for (idim = 0; idim < ndim; ++idim) {
        npy_intp bshape = broadcast_shape[ndim-idim-1];
        npy_intp *strides = NAD_STRIDES(axisdata);

        NAD_SHAPE(axisdata) = bshape;
        NAD_INDEX(axisdata) = 0;

        for (iop = 0; iop < nop; ++iop) {
            op_cur = op[iop];

            if (op_axes == NULL || op_axes[iop] == NULL) {
                if (op_cur == NULL) {
                    strides[iop] = 0;
                }
                else {
                    ondim = PyArray_NDIM(op_cur);
                    if (bshape == 1) {
                        strides[iop] = 0;
                        if (idim >= ondim &&
                                    (op_flags[iop] & NPY_ITER_NO_BROADCAST)) {
                            goto operand_different_than_broadcast;
                        }
                    }
                    else if (idim >= ondim ||
                                    PyArray_DIM(op_cur, ondim-idim-1) == 1) {
                        strides[iop] = 0;
                        if (op_flags[iop] & NPY_ITER_NO_BROADCAST) {
                            goto operand_different_than_broadcast;
                        }
                        /* If it's writeable, this means a reduction */
                        if (!npyiter_check_reduce_ok_and_set_flags(
                                iter, flags, op_itflags, iop, maskop, idim)) {
                            return 0;
                        }
                    }
                    else {
                        strides[iop] = PyArray_STRIDE(op_cur, ondim-idim-1);
                    }
                }
            }
            else {
                int *axes = op_axes[iop];
                npy_bool reduction_axis;
                int i;
                i = npyiter_get_op_axis(axes[ndim - idim - 1], &reduction_axis);

                if (reduction_axis) {
                    /* This is explicitly a reduction axis */
                    strides[iop] = 0;
                    NIT_ITFLAGS(iter) |= NPY_ITFLAG_REDUCE;
                    op_itflags[iop] |= NPY_OP_ITFLAG_REDUCE;

                    if (NPY_UNLIKELY((i >= 0) && (op_cur != NULL) &&
                            (PyArray_DIM(op_cur, i) != 1))) {
                        PyErr_Format(PyExc_ValueError,
                                "operand was set up as a reduction along axis "
                                "%d, but the length of the axis is %zd "
                                "(it has to be 1)",
                                i, (Py_ssize_t)PyArray_DIM(op_cur, i));
                        return 0;
                    }
                }
                else if (bshape == 1) {
                    /*
                     * If the full iterator shape is 1, zero always works.
                     * NOTE: We thus always allow broadcast dimensions (i = -1)
                     *       if the shape is 1.
                     */
                    strides[iop] = 0;
                }
                else if (i >= 0) {
                    if (op_cur == NULL) {
                        /* stride is filled later, shape will match `bshape` */
                        strides[iop] = 0;
                    }
                    else if (PyArray_DIM(op_cur, i) == 1) {
                        strides[iop] = 0;
                        if (op_flags[iop] & NPY_ITER_NO_BROADCAST) {
                            goto operand_different_than_broadcast;
                        }
                        if (!npyiter_check_reduce_ok_and_set_flags(
                                iter, flags, op_itflags, iop, maskop, i)) {
                            return 0;
                        }
                    }
                    else {
                        strides[iop] = PyArray_STRIDE(op_cur, i);
                    }
                }
                else {
                    strides[iop] = 0;
                    /*
                     * If deleting this axis produces a reduction, but
                     * reduction wasn't enabled, throw an error.
                     * NOTE: We currently always allow new-axis if the iteration
                     *       size is 1 (thus allowing broadcasting sometimes).
                     */
                    if (!npyiter_check_reduce_ok_and_set_flags(
                            iter, flags, op_itflags, iop, maskop, i)) {
                        return 0;
                    }
                }
            }
        }

        NIT_ADVANCE_AXISDATA(axisdata, 1);
    }

    /* Now fill in the ITERSIZE member */
    NIT_ITERSIZE(iter) = 1;
    for (idim = 0; idim < ndim; ++idim) {
        if (npy_mul_sizes_with_overflow(&NIT_ITERSIZE(iter),
                    NIT_ITERSIZE(iter), broadcast_shape[idim])) {
            if ((itflags & NPY_ITFLAG_HASMULTIINDEX) &&
                    !(itflags & NPY_ITFLAG_HASINDEX) &&
                    !(itflags & NPY_ITFLAG_BUFFER)) {
                /*
                 * If RemoveAxis may be called, the size check is delayed
                 * until either the multi index is removed, or GetIterNext
                 * is called.
                 */
                NIT_ITERSIZE(iter) = -1;
                break;
            }
            else {
                PyErr_SetString(PyExc_ValueError, "iterator is too large");
                return 0;
            }
        }
    }
    /* The range defaults to everything */
    NIT_ITERSTART(iter) = 0;
    NIT_ITEREND(iter) = NIT_ITERSIZE(iter);

    return 1;

broadcast_error: {
        npy_intp remdims[NPY_MAXDIMS];

        if (op_axes == NULL) {
            PyObject *shape1 = PyUnicode_FromString("");
            if (shape1 == NULL) {
                return 0;
            }
            for (iop = 0; iop < nop; ++iop) {
                if (op[iop] != NULL) {
                    int ndims = PyArray_NDIM(op[iop]);
                    npy_intp *dims = PyArray_DIMS(op[iop]);
                    PyObject *tmp = convert_shape_to_string(ndims, dims, " ");
                    if (tmp == NULL) {
                        Py_DECREF(shape1);
                        return 0;
                    }
                    Py_SETREF(shape1, PyUnicode_Concat(shape1, tmp));
                    Py_DECREF(tmp);
                    if (shape1 == NULL) {
                        return 0;
                    }
                }
            }
            if (itershape == NULL) {
                PyErr_Format(PyExc_ValueError,
                        "operands could not be broadcast together with "
                        "shapes %S", shape1);
                Py_DECREF(shape1);
                return 0;
            }
            else {
                PyObject *shape2 = convert_shape_to_string(ndim, itershape, "");
                if (shape2 == NULL) {
                    Py_DECREF(shape1);
                    return 0;
                }
                PyErr_Format(PyExc_ValueError,
                        "operands could not be broadcast together with "
                        "shapes %S and requested shape %S", shape1, shape2);
                Py_DECREF(shape1);
                Py_DECREF(shape2);
                return 0;
            }
        }
        else {
            PyObject *shape1 = PyUnicode_FromString("");
            if (shape1 == NULL) {
                return 0;
            }
            for (iop = 0; iop < nop; ++iop) {
                if (op[iop] != NULL) {
                    int *axes = op_axes[iop];
                    int ndims = PyArray_NDIM(op[iop]);
                    npy_intp *dims = PyArray_DIMS(op[iop]);
                    char *tmpstr = (axes == NULL) ? " " : "->";

                    PyObject *tmp = convert_shape_to_string(ndims, dims, tmpstr);
                    if (tmp == NULL) {
                        Py_DECREF(shape1);
                        return 0;
                    }
                    Py_SETREF(shape1, PyUnicode_Concat(shape1, tmp));
                    Py_DECREF(tmp);
                    if (shape1 == NULL) {
                        return 0;
                    }

                    if (axes != NULL) {
                        for (idim = 0; idim < ndim; ++idim) {
                            int i = npyiter_get_op_axis(axes[idim], NULL);

                            if (i >= 0 && i < PyArray_NDIM(op[iop])) {
                                remdims[idim] = PyArray_DIM(op[iop], i);
                            }
                            else {
                                remdims[idim] = -1;
                            }
                        }
                        PyObject *tmp = convert_shape_to_string(ndim, remdims, " ");
                        if (tmp == NULL) {
                            Py_DECREF(shape1);
                            return 0;
                        }
                        Py_SETREF(shape1, PyUnicode_Concat(shape1, tmp));
                        Py_DECREF(tmp);
                        if (shape1 == NULL) {
                            return 0;
                        }
                    }
                }
            }
            if (itershape == NULL) {
                PyErr_Format(PyExc_ValueError,
                        "operands could not be broadcast together with "
                        "remapped shapes [original->remapped]: %S", shape1);
                Py_DECREF(shape1);
                return 0;
            }
            else {
                PyObject *shape2 = convert_shape_to_string(ndim, itershape, "");
                if (shape2 == NULL) {
                    Py_DECREF(shape1);
                    return 0;
                }
                PyErr_Format(PyExc_ValueError,
                        "operands could not be broadcast together with "
                        "remapped shapes [original->remapped]: %S and "
                        "requested shape %S", shape1, shape2);
                Py_DECREF(shape1);
                Py_DECREF(shape2);
                return 0;
            }
        }
    }

operand_different_than_broadcast: {
        /* operand shape */
        int ndims = PyArray_NDIM(op[iop]);
        npy_intp *dims = PyArray_DIMS(op[iop]);
        PyObject *shape1 = convert_shape_to_string(ndims, dims, "");
        if (shape1 == NULL) {
            return 0;
        }

        /* Broadcast shape */
        PyObject *shape2 = convert_shape_to_string(ndim, broadcast_shape, "");
        if (shape2 == NULL) {
            Py_DECREF(shape1);
            return 0;
        }

        if (op_axes == NULL || op_axes[iop] == NULL) {
            /* operand shape not remapped */

            if (op_flags[iop] & NPY_ITER_READONLY) {
                PyErr_Format(PyExc_ValueError,
                    "non-broadcastable operand with shape %S doesn't "
                    "match the broadcast shape %S", shape1, shape2);
            }
            else {
                PyErr_Format(PyExc_ValueError,
                    "non-broadcastable output operand with shape %S doesn't "
                    "match the broadcast shape %S", shape1, shape2);
            }
            Py_DECREF(shape1);
            Py_DECREF(shape2);
            return 0;
        }
        else {
            /* operand shape remapped */

            npy_intp remdims[NPY_MAXDIMS];
            int *axes = op_axes[iop];
            for (idim = 0; idim < ndim; ++idim) {
                npy_intp i = axes[ndim - idim - 1];
                if (i >= 0 && i < PyArray_NDIM(op[iop])) {
                    remdims[idim] = PyArray_DIM(op[iop], i);
                }
                else {
                    remdims[idim] = -1;
                }
            }

            PyObject *shape3 = convert_shape_to_string(ndim, remdims, "");
            if (shape3 == NULL) {
                Py_DECREF(shape1);
                Py_DECREF(shape2);
                return 0;
            }

            if (op_flags[iop] & NPY_ITER_READONLY) {
                PyErr_Format(PyExc_ValueError,
                    "non-broadcastable operand with shape %S "
                    "[remapped to %S] doesn't match the broadcast shape %S",
                    shape1, shape3, shape2);
            }
            else {
                PyErr_Format(PyExc_ValueError,
                    "non-broadcastable output operand with shape %S "
                    "[remapped to %S] doesn't match the broadcast shape %S",
                    shape1, shape3, shape2);
            }
            Py_DECREF(shape1);
            Py_DECREF(shape2);
            Py_DECREF(shape3);
            return 0;
        }
    }
}


/*
 * At this point we (presumably) use a buffered iterator and here we want
 * to find out the best way to buffer the iterator in a fashion that we don't
 * have to figure out a lot of things on every outer iteration.
 *
 * How do we iterate?
 * ------------------
 * There are currently two modes of "buffered" iteration:
 * 1. The normal mode, where we either buffer each operand or not and
 *    then do a 1-D loop on those buffers (or operands).
 * 2. The "reduce" mode.  In reduce mode (ITFLAG_REDUCE) we internally use a
 *    a double iteration where for "reduce" operands we have:
 *      - One outer iteration with stride == 0 and a core with at least one
 *        stride != 0 (all of them if this is a true reduce/writeable operand).
 *      - One outer iteration with stride != 0 and a core of all strides == 0.
 *    This setup allows filling the buffer with only the stride != 0 and then
 *    doing the double loop.
 *    An Example for these two cases is:
 *        arr = np.ones((100, 10, 10))[::2, :, :]
 *        arr.sum(-1)
 *        arr.sum(-2)
 *    Where the slice prevents the iterator from collapsing axes and the
 *    result has stride 0 either along the last or the second to last axis.
 *    In both cases we can buffer 10x10 elements in reduce mode.
 *    (This iteration needs no buffer, add a cast to ensure actual buffering.)
 *
 * Only a writeable (reduce) operand require this reduce mode because for
 * reading it is OK if the buffer holds duplicated elements.
 * The benefit of the reduce mode is that it allows for larger core sizes and
 * buffers since the zero strides do not allow a single 1-d iteration.
 * If we use reduce-mode, we can apply it also to read-only operands as an
 * optimization.
 *
 * The function here finds the first "outer" dimension and it's "core" to use
 * that works with reductions.
 * While iterating, we will fill the buffers making sure that we:
 *   - Never buffer beyond the first outer dimension (optimize chance of re-use).
 *   - If the iterator is manually set to an offset into what is part of the
 *     core (see second example below), then we only fill the buffer to finish
 *     that one core.  This re-aligns us with the core and is necessary for
 *     reductions.  (Such manual setting should be rare or happens exactly once
 *     for splitting the iteration into worker chunks.)
 *
 * And examples for these two constraints:
 *   Given the iteration shape is (100, 10, 10) and the core size 10 with a
 *   buffer size of 60 (due to limits), making dimension 1 the "outer" one.
 *   The first iterations/buffers would then range (excluding end-point):
 *     - (0, 0, 0) -> (0, 6, 0)
 *     - (0, 6, 0) -> (1, 0, 0)  # Buffer only holds 40 of 60 possible elements.
 *     - (1, 0, 0) -> (1, 6, 0)
 *     - ...
 *   If the user limits to a range starting from 75, we use:
 *     - (0, 7, 5) -> (0, 8, 0)  # Only 5 elements to re-align with core.
 *     - (0, 8, 0) -> (1, 0, 0)
 *     - ...  # continue as above
 *
 * This means that the data stored in the buffer has always the same structure
 * (except when manually moved),  which allows us to fill the buffer more simply
 * and optimally in some cases, and makes it easier to determine whether buffer
 * content is re-usable (e.g., because it represents broadcasted operands).
 *
 * Best buffer and core size
 * -------------------------
 * To avoid having to figure out what to copy every time we fill buffers,
 * we here want to find the outer iteration dimension such that:
 *   - Its core size is <= the maximum buffersize if buffering is needed;
 *   - Reductions are possible (with or without reduce mode);
 *   - Iteration overhead is minimized.  We estimate the total overhead with
 *     the number "outer" iterations:
 *
 *        N_o = full_iterator_size / min(core_size * outer_dim_size, buffersize)
 *
 *     This is approximately how often `iternext()` is called when the user
 *     is using an external-loop and how often we would fill buffers.
 *     The total overhead is then estimated as:
 *
 *        (1 + n_buffers) * N_o
 *
 *     Since the iterator size is a constant, we can estimate the overhead as:
 *
 *        (1 + n_buffers) / min(core_size * outer_dim_size, buffersize)
 *
 *     And when comparing two options multiply by the others divisor/size to
 *     avoid the division.
 *
 * TODO: Probably should tweak or simplify?  The formula is clearly not
 *       the actual cost (Buffers add a constant total cost as well).
 *       Right now, it mostly rejects growing the core size when we are already
 *       close to the maximum buffersize (even overhead wise not worth it).
 *       That may be good enough, but maybe it can be spelled simpler?
 *
 * In theory, the reduction could also span multiple axes if other operands
 * are buffered.  We do not try to discover this.
 */
static int
npyiter_find_buffering_setup(NpyIter *iter, npy_intp buffersize)
{
    int nop = iter->nop;
    int ndim = iter->ndim;
    npy_uint32 itflags = iter->itflags;
    NpyIter_BufferData *bufferdata = NIT_BUFFERDATA(iter);

    /* Per operand space; could also reuse an iterator field initialized later */
    NPY_ALLOC_WORKSPACE(dim_scratch_space, int, 10, 2 * nop);
    if (dim_scratch_space == NULL) {
        return -1;
    }
    /*
     * We check two things here, first how many operand dimensions can be
     * iterated using a single stride (all dimensions are consistent),
     * and second, whether we found a reduce dimension for the operand.
     * That is an outer dimension a reduce would have to take place on.
     */
    int *op_single_stride_dims = dim_scratch_space;
    int *op_reduce_outer_dim = dim_scratch_space + nop;

    npy_intp sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);
    NpyIter_AxisData *axisdata = NIT_AXISDATA(iter);
    npyiter_opitflags *op_itflags = NIT_OPITFLAGS(iter);

    /*
     * We can only continue as long as we are within the maximum allowed size.
     * When no buffering is needed and GROWINNER is set, we don't have to
     * worry about this maximum.
     *
     * If the user passed no buffersize, default to one small enough that it
     * should be cache friendly and big enough to amortize overheads.
     */
    npy_intp maximum_size = buffersize <= 0 ? NPY_BUFSIZE : buffersize;

    /* The cost factor defined by: (1 + n_buffered) */
    int cost = 1;

    for (int iop = 0; iop < nop; ++iop) {
        op_single_stride_dims[iop] = 1;
        op_reduce_outer_dim[iop] = 0;
        if (op_itflags[iop] & NPY_OP_ITFLAG_CAST) {
            cost += 1;
        }
    }

    /*
     * Once a reduce operand reaches a ==0/!=0 stride flip, this dimension
     * becomes the outer reduce dimension.
     */
    int outer_reduce_dim = 0;

    npy_intp size = axisdata->shape;  /* the current total size */

    /* Note that there is always one axidata that we use (even with ndim =0) */
    int best_dim = 0;
    int best_cost = cost;
    /* The size of the "outer" iteration and all previous dimensions: */
    npy_intp best_size = size;
    npy_intp best_coresize = 1;

    NPY_IT_DBG_PRINT("Iterator: discovering best core size\n");
    for (int idim = 1; idim < ndim; idim++) {
        if (outer_reduce_dim) {
            /* Cannot currently expand beyond reduce dim! */
            break;
        }
        if (size >= maximum_size &&
                (cost > 1 || !(itflags & NPY_ITFLAG_GROWINNER))) {
            /* Exceeded buffer size, can only improve without buffers and growinner. */
            break;
        }

        npy_intp *prev_strides = NAD_STRIDES(axisdata);
        npy_intp prev_shape = NAD_SHAPE(axisdata);
        NIT_ADVANCE_AXISDATA(axisdata, 1);
        npy_intp *strides = NAD_STRIDES(axisdata);

        for (int iop = 0; iop < nop; iop++) {
            /*
             * Check that we set things up nicely so strides coalesc.  Except
             * for index operands, which currently disrupts coalescing.
             * NOTE(seberg): presumably `npyiter_compute_index_strides` should
             * not set the strides to 0, but this was safer for backporting.
             */
            assert((axisdata->shape != 1) || (prev_strides[iop] == strides[iop])
                   || (op_itflags[iop] & (NPY_ITER_C_INDEX|NPY_ITER_F_INDEX)));

            if (op_single_stride_dims[iop] == idim) {
                /*  Best case: the strides still collapse for this operand. */
                if (prev_strides[iop] * prev_shape == strides[iop]) {
                    op_single_stride_dims[iop] += 1;
                    continue;
                }

                /*
                 * Operand now requires buffering (if it was not already).
                 * NOTE: This is technically not true since we may still use
                 *       an outer reduce at this point.
                 *       So it prefers a non-reduce setup, which seems not
                 *       ideal, but OK.
                 */
                if (!(op_itflags[iop] & NPY_OP_ITFLAG_CAST)) {
                    cost += 1;
                }
            }

            /*
             * If this operand is a reduction operand and the stride swapped
             * between !=0 and ==0 then this is the `outer_reduce_dim` and
             * we will never continue further (see break at start of op loop).
             */
            if ((op_itflags[iop] & NPY_OP_ITFLAG_REDUCE)
                    && (strides[iop] == 0 || prev_strides[iop] == 0)) {
                assert(outer_reduce_dim == 0 || outer_reduce_dim == idim);
                op_reduce_outer_dim[iop] = idim;
                outer_reduce_dim = idim;
            }
            /* For clarity: op_reduce_outer_dim[iop] if set always matches. */
            assert(!op_reduce_outer_dim[iop] || op_reduce_outer_dim[iop] == outer_reduce_dim);
        }

        npy_intp coresize = size;  /* if we iterate here, this is the core */
        size *= axisdata->shape;
        if (size == 0) {
            break;  /* Avoid a zero coresize. */
        }

        double bufsize = (double)size;
        if (size > maximum_size &&
                (cost > 1 || !(itflags & NPY_ITFLAG_GROWINNER))) {
            /* If we need buffering, limit size in cost calculation. */
            bufsize = (double)maximum_size;
        }

        NPY_IT_DBG_PRINT("    dim=%d, n_buffered=%d, cost=%g @bufsize=%g (prev scaled cost=%g)\n",
                         idim, cost - 1, cost * (double)best_size, bufsize, best_cost * bufsize);

        /*
         * Compare cost (use double to avoid overflows), as explained above
         * the cost is compared via the other buffersize.
         */
        if (cost * (double)best_size <= best_cost * bufsize) {
            /* This dimension is better! */
            best_cost = cost;
            best_coresize = coresize;
            best_size = size;
            best_dim = idim;
        }
    }

    npy_bool using_reduce = outer_reduce_dim && (best_dim == outer_reduce_dim);
    npy_bool iterator_must_buffer = 0;

    /* We found the best chunking store the information */
    assert(best_coresize != 0);
    NIT_BUFFERDATA(iter)->coresize = best_coresize;
    NIT_BUFFERDATA(iter)->outerdim = best_dim;

    /*
     * We found the best dimensions to iterate on and now need to fill
     * in all the buffer information related to the iteration.
     * This includes filling in information about reduce outer dims
     * (we do this even if it is not a reduce for simplicity).
     */
    axisdata = NIT_AXISDATA(iter);
    NpyIter_AxisData *reduce_axisdata = NIT_INDEX_AXISDATA(axisdata, outer_reduce_dim);

    NPY_IT_DBG_PRINT("Iterator: Found core size=%zd, outer=%zd at dim=%d:\n",
                      best_coresize, reduce_axisdata->shape, best_dim);

    /* If we are not using a reduce axes mark it and shrink. */
    if (using_reduce) {
        assert(NIT_ITFLAGS(iter) & NPY_ITFLAG_REDUCE);
        NPY_IT_DBG_PRINT("    using reduce logic\n");
    }
    else {
        NIT_ITFLAGS(iter) &= ~NPY_ITFLAG_REDUCE;
        NPY_IT_DBG_PRINT("    not using reduce logic\n");
    }

    for (int iop = 0; iop < nop; iop++) {
        /* We need to fill in the following information */
        npy_bool is_reduce_op;
        npy_bool op_is_buffered = (op_itflags[iop]&NPY_OP_ITFLAG_CAST) != 0;

        /* If contig was requested and this is not writeable avoid zero strides */
        npy_bool avoid_zero_strides = (
                (op_itflags[iop] & NPY_OP_ITFLAG_CONTIG)
                && !(op_itflags[iop] & NPY_OP_ITFLAG_WRITE));

        /*
         * Figure out if this is iterated as a reduce op.  Even one marked
         * for reduction may not be iterated as one.
         */
        if (!using_reduce) {
            is_reduce_op = 0;
        }
        else if (op_reduce_outer_dim[iop] == best_dim) {
            /* This op *must* use reduce semantics. */
            is_reduce_op = 1;
        }
        else if (op_single_stride_dims[iop] == best_dim && !op_is_buffered) {
            /*
             * Optimization: This operand is not buffered and we might as well
             * iterate it as an unbuffered reduce operand.
             */
            is_reduce_op = 1;
        }
        else if (NAD_STRIDES(reduce_axisdata)[iop] == 0
                    && op_single_stride_dims[iop] <= best_dim
                    && !avoid_zero_strides) {
            /*
             * Optimization: If the outer (reduce) stride is 0 on the operand
             * then we can iterate this in a reduce way: buffer the core only
             * and repeat it in the "outer" dimension.
             * If user requested contig, we may have to avoid 0 strides, this
             * is incompatible with the reduce path.
             */
            is_reduce_op = 1;
        }
        else {
            is_reduce_op = 0;
        }

        /*
         * See if the operand is a single stride (if we use reduce logic)
         * we don't need to worry about the outermost dimension.
         * If it is not a single stride, we must buffer the operand.
         */
        if (op_single_stride_dims[iop] + is_reduce_op > best_dim) {
            NIT_OPITFLAGS(iter)[iop] |= NPY_OP_ITFLAG_BUF_SINGLESTRIDE;
        }
        else {
            op_is_buffered = 1;
        }

        npy_intp inner_stride;
        npy_intp reduce_outer_stride;
        if (op_is_buffered) {
            npy_intp itemsize = NIT_DTYPES(iter)[iop]->elsize;
            /*
             * A buffered operand has a stride of itemsize unless we use
             * reduce logic.  In that case, either the inner or outer stride
             * is 0.
             */
            if (is_reduce_op) {
                if (NAD_STRIDES(reduce_axisdata)[iop] == 0) {
                    inner_stride = itemsize;
                    reduce_outer_stride = 0;
                }
                else {
                    inner_stride = 0;
                    reduce_outer_stride = itemsize;
                }
            }
            else {
                if (NIT_OPITFLAGS(iter)[iop] & NPY_OP_ITFLAG_BUF_SINGLESTRIDE
                        && NAD_STRIDES(axisdata)[iop] == 0
                        && !avoid_zero_strides) {
                    /* This op is always 0 strides, so even the buffer is that. */
                    inner_stride = 0;
                    reduce_outer_stride = 0;
                }
                else {
                    /* normal buffered op */
                    inner_stride = itemsize;
                    reduce_outer_stride = itemsize * best_coresize;
                }
            }
        }
        else {
            inner_stride = NAD_STRIDES(axisdata)[iop];
            reduce_outer_stride = NAD_STRIDES(reduce_axisdata)[iop];
        }

        if (!using_reduce) {
            /* invalidate for now, since we should not use it */
            reduce_outer_stride = NPY_MIN_INTP;
        }

        NPY_IT_DBG_PRINT(
            "Iterator: op=%d (buffered=%d, reduce=%d, single-stride=%d):\n"
            "    inner stride: %zd\n"
            "    reduce outer stride: %zd  (if iterator uses reduce)\n",
            iop, op_is_buffered, is_reduce_op,
            (NIT_OPITFLAGS(iter)[iop] & NPY_OP_ITFLAG_BUF_SINGLESTRIDE) != 0,
            inner_stride, reduce_outer_stride);

        NBF_STRIDES(bufferdata)[iop] = inner_stride;
        NBF_REDUCE_OUTERSTRIDES(bufferdata)[iop] = reduce_outer_stride;

        /* The actual reduce usage may have changed! */
        if (is_reduce_op) {
            NIT_OPITFLAGS(iter)[iop] |= NPY_OP_ITFLAG_REDUCE;
        }
        else {
            NIT_OPITFLAGS(iter)[iop] &= ~NPY_OP_ITFLAG_REDUCE;
        }

        if (!op_is_buffered) {
            NIT_OPITFLAGS(iter)[iop] |= NPY_OP_ITFLAG_BUFNEVER;
        }
        else {
            iterator_must_buffer = 1;
        }
    }

    /*
     * If we buffer or do not have grow-inner, make sure that the size is
     * below the maximum_size, but a multiple of the coresize.
     */
    if (iterator_must_buffer || !(itflags & NPY_ITFLAG_GROWINNER)) {
        if (maximum_size < best_size) {
            best_size = best_coresize * (maximum_size / best_coresize);
        }
    }
    NIT_BUFFERDATA(iter)->buffersize = best_size;
    /* Core starts at 0 initially, if needed it is set in goto index. */
    NIT_BUFFERDATA(iter)->coreoffset = 0;

    npy_free_workspace(dim_scratch_space);
    return 0;
}


/*
 * Replaces the AXISDATA for the iop'th operand, broadcasting
 * the dimensions as necessary.  Assumes the replacement array is
 * exactly the same shape as the original array used when
 * npy_fill_axisdata was called.
 *
 * If op_axes is not NULL, it should point to an ndim-sized
 * array.
 */
static void
npyiter_replace_axisdata(
        NpyIter *iter, int iop, PyArrayObject *op,
        int orig_op_ndim, const int *op_axes)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int nop = NIT_NOP(iter);
    char *op_dataptr = PyArray_DATA(op);

    NpyIter_AxisData *axisdata0, *axisdata;
    npy_intp sizeof_axisdata;
    npy_int8 *perm;
    npy_intp baseoffset = 0;

    perm = NIT_PERM(iter);
    axisdata0 = NIT_AXISDATA(iter);
    sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);

    /*
     * Replace just the strides which were non-zero, and compute
     * the base data address.
     */
    axisdata = axisdata0;

    if (op_axes != NULL) {
        for (idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, 1)) {
            int i;
            npy_bool axis_flipped;
            npy_intp shape;

            /* Apply perm to get the original axis, and check if its flipped */
            i = npyiter_undo_iter_axis_perm(idim, ndim, perm, &axis_flipped);

            i = npyiter_get_op_axis(op_axes[i], NULL);
            assert(i < orig_op_ndim);
            if (i >= 0) {
                shape = PyArray_DIM(op, i);
                if (shape != 1) {
                    npy_intp stride = PyArray_STRIDE(op, i);
                    if (axis_flipped) {
                        NAD_STRIDES(axisdata)[iop] = -stride;
                        baseoffset += stride*(shape-1);
                    }
                    else {
                        NAD_STRIDES(axisdata)[iop] = stride;
                    }
                }
            }
        }
    }
    else {
        for (idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, 1)) {
            int i;
            npy_bool axis_flipped;
            npy_intp shape;

            i = npyiter_undo_iter_axis_perm(
                    idim, orig_op_ndim, perm, &axis_flipped);

            if (i >= 0) {
                shape = PyArray_DIM(op, i);
                if (shape != 1) {
                    npy_intp stride = PyArray_STRIDE(op, i);
                    if (axis_flipped) {
                        NAD_STRIDES(axisdata)[iop] = -stride;
                        baseoffset += stride*(shape-1);
                    }
                    else {
                        NAD_STRIDES(axisdata)[iop] = stride;
                    }
                }
            }
        }
    }

    op_dataptr += baseoffset;

    /* Now the base data pointer is calculated, set it everywhere it's needed */
    NIT_RESETDATAPTR(iter)[iop] = op_dataptr;
    NIT_BASEOFFSETS(iter)[iop] = baseoffset;
    NIT_DATAPTRS(iter)[iop] = op_dataptr;
}

/*
 * Computes the iterator's index strides and initializes the index values
 * to zero.
 *
 * This must be called before the axes (i.e. the AXISDATA array) may
 * be reordered.
 */
static void
npyiter_compute_index_strides(NpyIter *iter, npy_uint32 flags)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int nop = NIT_NOP(iter);

    npy_intp indexstride;
    NpyIter_AxisData *axisdata;
    npy_intp sizeof_axisdata;

    NIT_DATAPTRS(iter)[nop] = 0;
    /*
     * If there is only one element being iterated, we just have
     * to touch the first set the "dataptr".
     * This also initializes the data for the 0-d case.
     */
    if (NIT_ITERSIZE(iter) == 1) {
        if (itflags & NPY_ITFLAG_HASINDEX) {
            axisdata = NIT_AXISDATA(iter);
        }
        return;
    }

    if (flags & NPY_ITER_C_INDEX) {
        sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);
        axisdata = NIT_AXISDATA(iter);
        indexstride = 1;
        for(idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, 1)) {
            npy_intp shape = NAD_SHAPE(axisdata);

            if (shape == 1) {
                NAD_STRIDES(axisdata)[nop] = 0;
            }
            else {
                NAD_STRIDES(axisdata)[nop] = indexstride;
            }
            indexstride *= shape;
        }
    }
    else if (flags & NPY_ITER_F_INDEX) {
        sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);
        axisdata = NIT_INDEX_AXISDATA(NIT_AXISDATA(iter), ndim-1);
        indexstride = 1;
        for(idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, -1)) {
            npy_intp shape = NAD_SHAPE(axisdata);

            if (shape == 1) {
                NAD_STRIDES(axisdata)[nop] = 0;
            }
            else {
                NAD_STRIDES(axisdata)[nop] = indexstride;
            }
            indexstride *= shape;
        }
    }
}

/*
 * If the order is NPY_KEEPORDER, lets the iterator find the best
 * iteration order, otherwise forces it.  Indicates in the itflags that
 * whether the iteration order was forced.
 */
static void
npyiter_apply_forced_iteration_order(NpyIter *iter, NPY_ORDER order)
{
    /*npy_uint32 itflags = NIT_ITFLAGS(iter);*/
    int ndim = NIT_NDIM(iter);
    int iop, nop = NIT_NOP(iter);

    switch (order) {
    case NPY_CORDER:
        NIT_ITFLAGS(iter) |= NPY_ITFLAG_FORCEDORDER;
        break;
    case NPY_FORTRANORDER:
        NIT_ITFLAGS(iter) |= NPY_ITFLAG_FORCEDORDER;
        /* Only need to actually do something if there is more than 1 dim */
        if (ndim > 1) {
            npyiter_reverse_axis_ordering(iter);
        }
        break;
    case NPY_ANYORDER:
        NIT_ITFLAGS(iter) |= NPY_ITFLAG_FORCEDORDER;
        /* Only need to actually do something if there is more than 1 dim */
        if (ndim > 1) {
            PyArrayObject **op = NIT_OPERANDS(iter);
            int forder = 1;

            /* Check that all the array inputs are fortran order */
            for (iop = 0; iop < nop; ++iop, ++op) {
                if (*op && !PyArray_CHKFLAGS(*op, NPY_ARRAY_F_CONTIGUOUS)) {
                    forder = 0;
                    break;
                }
            }

            if (forder) {
                npyiter_reverse_axis_ordering(iter);
            }
        }
        break;
    case NPY_KEEPORDER:
        /* Don't set the forced order flag here... */
        break;
    }
}

/*
 * This function negates any strides in the iterator
 * which are negative.  When iterating more than one
 * object, it only flips strides when they are all
 * negative or zero.
 */
static void
npyiter_flip_negative_strides(NpyIter *iter)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int iop, nop = NIT_NOP(iter);

    npy_intp istrides, nstrides = NAD_NSTRIDES();
    NpyIter_AxisData *axisdata;
    npy_intp *baseoffsets;
    npy_intp sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);
    int any_flipped = 0;

    axisdata = NIT_AXISDATA(iter);
    baseoffsets = NIT_BASEOFFSETS(iter);
    for (idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, 1)) {
        npy_intp *strides = NAD_STRIDES(axisdata);
        int any_negative = 0;

        /*
         * Check the signs of all the operand strides.
         */
        for (iop = 0; iop < nop; ++iop) {
            if (strides[iop] < 0) {
                any_negative = 1;
            }
            else if (strides[iop] != 0) {
                break;
            }
        }
        /*
         * If at least one stride is negative and none are positive,
         * flip all the strides for this dimension.
         */
        if (any_negative && iop == nop) {
            npy_intp shapem1 = NAD_SHAPE(axisdata) - 1;

            for (istrides = 0; istrides < nstrides; ++istrides) {
                npy_intp stride = strides[istrides];

                /* Adjust the base pointers to start at the end */
                baseoffsets[istrides] += shapem1 * stride;
                /* Flip the stride */
                strides[istrides] = -stride;
            }
            /*
             * Make the perm entry negative so get_multi_index
             * knows it's flipped
             */
            NIT_PERM(iter)[idim] = -1-NIT_PERM(iter)[idim];

            any_flipped = 1;
        }
    }

    /*
     * If any strides were flipped, the base pointers were adjusted
     * in the first AXISDATA, and need to be copied to all the rest
     */
    if (any_flipped) {
        char **resetdataptr = NIT_RESETDATAPTR(iter);

        for (istrides = 0; istrides < nstrides; ++istrides) {
            resetdataptr[istrides] += baseoffsets[istrides];
            NIT_DATAPTRS(iter)[istrides] = resetdataptr[istrides];
        }
        /*
         * Indicate that some of the perm entries are negative,
         * and that it's not (strictly speaking) the identity perm.
         */
        NIT_ITFLAGS(iter) = (NIT_ITFLAGS(iter)|NPY_ITFLAG_NEGPERM) &
                            ~NPY_ITFLAG_IDENTPERM;
    }
}

static void
npyiter_reverse_axis_ordering(NpyIter *iter)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int ndim = NIT_NDIM(iter);
    int nop = NIT_NOP(iter);

    npy_intp i, temp, size;
    npy_intp *first, *last;
    npy_int8 *perm;

    size = NIT_AXISDATA_SIZEOF(itflags, ndim, nop)/NPY_SIZEOF_INTP;
    first = (npy_intp*)NIT_AXISDATA(iter);
    last = first + (ndim-1)*size;

    /* This loop reverses the order of the AXISDATA array */
    while (first < last) {
        for (i = 0; i < size; ++i) {
            temp = first[i];
            first[i] = last[i];
            last[i] = temp;
        }
        first += size;
        last -= size;
    }

    /* Store the perm we applied */
    perm = NIT_PERM(iter);
    for(i = ndim-1; i >= 0; --i, ++perm) {
        *perm = (npy_int8)i;
    }

    NIT_ITFLAGS(iter) &= ~NPY_ITFLAG_IDENTPERM;
}

static inline npy_intp
intp_abs(npy_intp x)
{
    return (x < 0) ? -x : x;
}

static void
npyiter_find_best_axis_ordering(NpyIter *iter)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int iop, nop = NIT_NOP(iter);

    npy_intp ax_i0, ax_i1, ax_ipos;
    npy_int8 ax_j0, ax_j1;
    npy_int8 *perm;
    NpyIter_AxisData *axisdata = NIT_AXISDATA(iter);
    npy_intp sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);
    int permuted = 0;

    perm = NIT_PERM(iter);

    /*
     * Do a custom stable insertion sort.  Note that because
     * the AXISDATA has been reversed from C order, this
     * is sorting from smallest stride to biggest stride.
     */
    for (ax_i0 = 1; ax_i0 < ndim; ++ax_i0) {
        npy_intp *strides0;

        /* 'ax_ipos' is where perm[ax_i0] will get inserted */
        ax_ipos = ax_i0;
        ax_j0 = perm[ax_i0];

        strides0 = NAD_STRIDES(NIT_INDEX_AXISDATA(axisdata, ax_j0));
        for (ax_i1 = ax_i0-1; ax_i1 >= 0; --ax_i1) {
            int ambig = 1, shouldswap = 0;
            npy_intp *strides1;

            ax_j1 = perm[ax_i1];

            strides1 = NAD_STRIDES(NIT_INDEX_AXISDATA(axisdata, ax_j1));

            for (iop = 0; iop < nop; ++iop) {
                if (strides0[iop] != 0 && strides1[iop] != 0) {
                    if (intp_abs(strides1[iop]) <=
                                            intp_abs(strides0[iop])) {
                        /*
                         * Set swap even if it's not ambiguous already,
                         * because in the case of conflicts between
                         * different operands, C-order wins.
                         */
                        shouldswap = 0;
                    }
                    else {
                        /* Only set swap if it's still ambiguous */
                        if (ambig) {
                            shouldswap = 1;
                        }
                    }

                    /*
                     * A comparison has been done, so it's
                     * no longer ambiguous
                     */
                    ambig = 0;
                }
            }
            /*
             * If the comparison was unambiguous, either shift
             * 'ax_ipos' to 'ax_i1' or stop looking for an insertion
             * point
             */
            if (!ambig) {
                if (shouldswap) {
                    ax_ipos = ax_i1;
                }
                else {
                    break;
                }
            }
        }

        /* Insert perm[ax_i0] into the right place */
        if (ax_ipos != ax_i0) {
            for (ax_i1 = ax_i0; ax_i1 > ax_ipos; --ax_i1) {
                perm[ax_i1] = perm[ax_i1-1];
            }
            perm[ax_ipos] = ax_j0;
            permuted = 1;
        }
    }

    /* Apply the computed permutation to the AXISDATA array */
    if (permuted == 1) {
        npy_intp i, size = sizeof_axisdata/NPY_SIZEOF_INTP;
        NpyIter_AxisData *ad_i;

        /* Use the index as a flag, set each to 1 */
        ad_i = axisdata;
        for (idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(ad_i, 1)) {
            NAD_INDEX(ad_i) = 1;
        }
        /* Apply the permutation by following the cycles */
        for (idim = 0; idim < ndim; ++idim) {
            ad_i = NIT_INDEX_AXISDATA(axisdata, idim);

            /* If this axis hasn't been touched yet, process it */
            if (NAD_INDEX(ad_i) == 1) {
                npy_int8 pidim = perm[idim];
                npy_intp tmp;
                NpyIter_AxisData *ad_p, *ad_q;

                if (pidim != idim) {
                    /* Follow the cycle, copying the data */
                    for (i = 0; i < size; ++i) {
                        pidim = perm[idim];
                        ad_q = ad_i;
                        tmp = *((npy_intp*)ad_q + i);
                        while (pidim != idim) {
                            ad_p = NIT_INDEX_AXISDATA(axisdata, pidim);
                            *((npy_intp*)ad_q + i) = *((npy_intp*)ad_p + i);

                            ad_q = ad_p;
                            pidim = perm[(int)pidim];
                        }
                        *((npy_intp*)ad_q + i) = tmp;
                    }
                    /* Follow the cycle again, marking it as done */
                    pidim = perm[idim];

                    while (pidim != idim) {
                        NAD_INDEX(NIT_INDEX_AXISDATA(axisdata, pidim)) = 0;
                        pidim = perm[(int)pidim];
                    }
                }
                NAD_INDEX(ad_i) = 0;
            }
        }
        /* Clear the identity perm flag */
        NIT_ITFLAGS(iter) &= ~NPY_ITFLAG_IDENTPERM;
    }
}

/*
 * Calculates a dtype that all the types can be promoted to, using the
 * ufunc rules.  If only_inputs is 1, it leaves any operands that
 * are not read from out of the calculation.
 */
static PyArray_Descr *
npyiter_get_common_dtype(int nop, PyArrayObject **op,
                        const npyiter_opitflags *op_itflags, PyArray_Descr **op_dtype,
                        PyArray_Descr **op_request_dtypes,
                        int only_inputs)
{
    int iop;
    npy_intp narrs = 0, ndtypes = 0;
    PyArray_Descr *ret;
    NPY_ALLOC_WORKSPACE(arrs_and_dtypes, void *, 2 * 4, 2 * nop);
    if (arrs_and_dtypes == NULL) {
        return NULL;
    }

    PyArrayObject **arrs = (PyArrayObject **)arrs_and_dtypes;
    PyArray_Descr **dtypes = (PyArray_Descr **)arrs_and_dtypes + nop;

    NPY_IT_DBG_PRINT("Iterator: Getting a common data type from operands\n");

    for (iop = 0; iop < nop; ++iop) {
        if (op_dtype[iop] != NULL &&
                    (!only_inputs || (op_itflags[iop] & NPY_OP_ITFLAG_READ))) {
            /* If no dtype was requested and the op is a scalar, pass the op */
            if ((op_request_dtypes == NULL ||
                            op_request_dtypes[iop] == NULL) &&
                                            PyArray_NDIM(op[iop]) == 0) {
                arrs[narrs++] = op[iop];
            }
            /* Otherwise just pass in the dtype */
            else {
                dtypes[ndtypes++] = op_dtype[iop];
            }
        }
    }

    if (narrs == 0) {
        npy_intp i;
        ret = dtypes[0];
        for (i = 1; i < ndtypes; ++i) {
            if (ret != dtypes[i])
                break;
        }
        if (i == ndtypes) {
            if (ndtypes == 1 || PyArray_ISNBO(ret->byteorder)) {
                Py_INCREF(ret);
            }
            else {
                ret = PyArray_DescrNewByteorder(ret, NPY_NATIVE);
            }
        }
        else {
            ret = PyArray_ResultType(narrs, arrs, ndtypes, dtypes);
        }
    }
    else {
        ret = PyArray_ResultType(narrs, arrs, ndtypes, dtypes);
    }

    npy_free_workspace(arrs_and_dtypes);
    return ret;
}

/*
 * Allocates a temporary array which can be used to replace op
 * in the iteration.  Its dtype will be op_dtype.
 *
 * The result array has a memory ordering which matches the iterator,
 * which may or may not match that of op.  The parameter 'shape' may be
 * NULL, in which case it is filled in from the iterator's shape.
 *
 * This function must be called before any axes are coalesced.
 */
static PyArrayObject *
npyiter_new_temp_array(NpyIter *iter, PyTypeObject *subtype,
                npy_uint32 flags, npyiter_opitflags *op_itflags,
                int op_ndim, npy_intp const *shape,
                PyArray_Descr *op_dtype, const int *op_axes)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int idim, ndim = NIT_NDIM(iter);
    int used_op_ndim;
    int nop = NIT_NOP(iter);

    npy_int8 *perm = NIT_PERM(iter);
    npy_intp new_shape[NPY_MAXDIMS], strides[NPY_MAXDIMS];
    npy_intp stride = op_dtype->elsize;
    NpyIter_AxisData *axisdata;
    npy_intp sizeof_axisdata;
    int i;

    PyArrayObject *ret;

    /*
     * There is an interaction with array-dtypes here, which
     * generally works. Let's say you make an nditer with an
     * output dtype of a 2-double array. All-scalar inputs
     * will result in a 1-dimensional output with shape (2).
     * Everything still works out in the nditer, because the
     * new dimension is always added on the end, and it cares
     * about what happens at the beginning.
     */

    /* If it's a scalar, don't need to check the axes */
    if (op_ndim == 0) {
        Py_INCREF(op_dtype);
        ret = (PyArrayObject *)PyArray_NewFromDescr(subtype, op_dtype, 0,
                               NULL, NULL, NULL, 0, NULL);

        return ret;
    }

    axisdata = NIT_AXISDATA(iter);
    sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);

    /* Initialize the strides to invalid values */
    for (i = 0; i < op_ndim; ++i) {
        strides[i] = NPY_MAX_INTP;
    }

    if (op_axes != NULL) {
        used_op_ndim = 0;
        for (idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, 1)) {
            npy_bool reduction_axis;

            /* Apply the perm to get the original axis */
            i = npyiter_undo_iter_axis_perm(idim, ndim, perm, NULL);
            i = npyiter_get_op_axis(op_axes[i], &reduction_axis);

            /*
             * If i < 0, this is a new axis (the operand does not have it)
             * so we can ignore it here.  The iterator setup will have
             * ensured already that a potential reduction/broadcast is valid.
             */
            if (i >= 0) {
                NPY_IT_DBG_PRINT3("Iterator: Setting allocated stride %d "
                                    "for iterator dimension %d to %d\n", (int)i,
                                    (int)idim, (int)stride);
                used_op_ndim += 1;
                strides[i] = stride;
                if (shape == NULL) {
                    if (reduction_axis) {
                        /* reduction axes always have a length of 1 */
                        new_shape[i] = 1;
                    }
                    else {
                        new_shape[i] = NAD_SHAPE(axisdata);
                    }
                    stride *= new_shape[i];
                    if (i >= ndim) {
                        PyErr_Format(PyExc_ValueError,
                                "automatically allocated output array "
                                "specified with an inconsistent axis mapping; "
                                "the axis mapping cannot include dimension %d "
                                "which is too large for the iterator dimension "
                                "of %d.", i, ndim);
                        return NULL;
                    }
                }
                else {
                    assert(!reduction_axis || shape[i] == 1);
                    stride *= shape[i];
                }
            }
        }
    }
    else {
        used_op_ndim = ndim;
        for (idim = 0; idim < ndim; ++idim, NIT_ADVANCE_AXISDATA(axisdata, 1)) {
            /* Apply the perm to get the original axis */
            i = npyiter_undo_iter_axis_perm(idim, op_ndim, perm, NULL);

            if (i >= 0) {
                NPY_IT_DBG_PRINT3("Iterator: Setting allocated stride %d "
                                    "for iterator dimension %d to %d\n", (int)i,
                                    (int)idim, (int)stride);
                strides[i] = stride;
                if (shape == NULL) {
                    new_shape[i] = NAD_SHAPE(axisdata);
                    stride *= new_shape[i];
                }
                else {
                    stride *= shape[i];
                }
            }
        }
    }

    if (shape == NULL) {
        /* If shape was NULL, use the shape we calculated */
        op_ndim = used_op_ndim;
        shape = new_shape;
        /*
         * If there's a gap in the array's dimensions, it's an error.
         * For instance, if op_axes [0, 2] is specified, there will a place
         * in the strides array where the value is not set.
         */
        for (i = 0; i < op_ndim; i++) {
            if (strides[i] == NPY_MAX_INTP) {
                PyErr_Format(PyExc_ValueError,
                        "automatically allocated output array "
                        "specified with an inconsistent axis mapping; "
                        "the axis mapping is missing an entry for "
                        "dimension %d.", i);
                return NULL;
            }
        }
    }
    else if (used_op_ndim < op_ndim) {
        /*
         * If custom axes were specified, some dimensions may not have
         * been used. These are additional axes which are ignored in the
         * iterator but need to be handled here.
         */
        npy_intp factor, itemsize, new_strides[NPY_MAXDIMS];

        /* Fill in the missing strides in C order */
        factor = 1;
        itemsize = op_dtype->elsize;
        for (i = op_ndim-1; i >= 0; --i) {
            if (strides[i] == NPY_MAX_INTP) {
                new_strides[i] = factor * itemsize;
                factor *= shape[i];
            }
        }

        /*
         * Copy the missing strides, and multiply the existing strides
         * by the calculated factor.  This way, the missing strides
         * are tighter together in memory, which is good for nested
         * loops.
         */
        for (i = 0; i < op_ndim; ++i) {
            if (strides[i] == NPY_MAX_INTP) {
                strides[i] = new_strides[i];
            }
            else {
                strides[i] *= factor;
            }
        }
    }

    /* Allocate the temporary array */
    Py_INCREF(op_dtype);
    ret = (PyArrayObject *)PyArray_NewFromDescr(subtype, op_dtype, op_ndim,
                               shape, strides, NULL, 0, NULL);
    if (ret == NULL) {
        return NULL;
    }

    /* Double-check that the subtype didn't mess with the dimensions */
    if (subtype != &PyArray_Type) {
        /*
         * TODO: the dtype could have a subarray, which adds new dimensions
         *       to `ret`, that should typically be fine, but will break
         *       in this branch.
         */
        if (PyArray_NDIM(ret) != op_ndim ||
                    !PyArray_CompareLists(shape, PyArray_DIMS(ret), op_ndim)) {
            PyErr_SetString(PyExc_RuntimeError,
                    "Iterator automatic output has an array subtype "
                    "which changed the dimensions of the output");
            Py_DECREF(ret);
            return NULL;
        }
    }

    return ret;
}

static int
npyiter_allocate_arrays(NpyIter *iter,
                        npy_uint32 flags,
                        PyArray_Descr **op_dtype, PyTypeObject *subtype,
                        const npy_uint32 *op_flags, npyiter_opitflags *op_itflags,
                        int **op_axes)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    int ndim = NIT_NDIM(iter);
    int iop, nop = NIT_NOP(iter);

    int check_writemasked_reductions = 0;

    PyArrayObject **op = NIT_OPERANDS(iter);

    if (flags & NPY_ITER_COPY_IF_OVERLAP) {
        /*
         * Perform operand memory overlap checks, if requested.
         *
         * If any write operand has memory overlap with any read operand,
         * eliminate all overlap by making temporary copies, by enabling
         * NPY_OP_ITFLAG_FORCECOPY for the write operand to force WRITEBACKIFCOPY.
         *
         * Operands with NPY_ITER_OVERLAP_ASSUME_ELEMENTWISE enabled are not
         * considered overlapping if the arrays are exactly the same. In this
         * case, the iterator loops through them in the same order element by
         * element.  (As usual, the user-provided inner loop is assumed to be
         * able to deal with this level of simple aliasing.)
         */
        for (iop = 0; iop < nop; ++iop) {
            int may_share_memory = 0;
            int iother;

            if (op[iop] == NULL) {
                /* Iterator will always allocate */
                continue;
            }

            if (!(op_itflags[iop] & NPY_OP_ITFLAG_WRITE)) {
                /*
                 * Copy output operands only, not inputs.
                 * A more sophisticated heuristic could be
                 * substituted here later.
                 */
                continue;
            }

            for (iother = 0; iother < nop; ++iother) {
                if (iother == iop || op[iother] == NULL) {
                    continue;
                }

                if (!(op_itflags[iother] & NPY_OP_ITFLAG_READ)) {
                    /* No data dependence for arrays not read from */
                    continue;
                }

                if (op_itflags[iother] & NPY_OP_ITFLAG_FORCECOPY) {
                    /* Already copied */
                    continue;
                }

                /*
                 * If the arrays are views to exactly the same data, no need
                 * to make copies, if the caller (eg ufunc) says it accesses
                 * data only in the iterator order.
                 *
                 * However, if there is internal overlap (e.g. a zero stride on
                 * a non-unit dimension), a copy cannot be avoided.
                 */
                if ((op_flags[iop] & NPY_ITER_OVERLAP_ASSUME_ELEMENTWISE) &&
                    (op_flags[iother] & NPY_ITER_OVERLAP_ASSUME_ELEMENTWISE) &&
                    PyArray_BYTES(op[iop]) == PyArray_BYTES(op[iother]) &&
                    PyArray_NDIM(op[iop]) == PyArray_NDIM(op[iother]) &&
                    PyArray_CompareLists(PyArray_DIMS(op[iop]),
                                         PyArray_DIMS(op[iother]),
                                         PyArray_NDIM(op[iop])) &&
                    PyArray_CompareLists(PyArray_STRIDES(op[iop]),
                                         PyArray_STRIDES(op[iother]),
                                         PyArray_NDIM(op[iop])) &&
                    PyArray_DESCR(op[iop]) == PyArray_DESCR(op[iother]) &&
                    solve_may_have_internal_overlap(op[iop], 1) == 0) {

                    continue;
                }

                /*
                 * Use max work = 1. If the arrays are large, it might
                 * make sense to go further.
                 */
                may_share_memory = solve_may_share_memory(op[iop],
                                                          op[iother],
                                                          1);

                if (may_share_memory) {
                    op_itflags[iop] |= NPY_OP_ITFLAG_FORCECOPY;
                    break;
                }
            }
        }
    }

    for (iop = 0; iop < nop; ++iop) {
        /*
         * Check whether there are any WRITEMASKED REDUCE operands
         * which should be validated after all the strides are filled
         * in.
         */
        if ((op_itflags[iop] &
                (NPY_OP_ITFLAG_WRITEMASKED | NPY_OP_ITFLAG_REDUCE)) ==
                        (NPY_OP_ITFLAG_WRITEMASKED | NPY_OP_ITFLAG_REDUCE)) {
            check_writemasked_reductions = 1;
        }

        /* NULL means an output the iterator should allocate */
        if (op[iop] == NULL) {
            PyArrayObject *out;
            PyTypeObject *op_subtype;

            /* Check whether the subtype was disabled */
            op_subtype = (op_flags[iop] & NPY_ITER_NO_SUBTYPE) ?
                                                &PyArray_Type : subtype;

            /*
             * Allocate the output array.
             *
             * Note that here, ndim is always correct if no op_axes was given
             * (but the actual dimension of op can be larger). If op_axes
             * is given, ndim is not actually used.
             */
            out = npyiter_new_temp_array(iter, op_subtype,
                                        flags, &op_itflags[iop],
                                        ndim,
                                        NULL,
                                        op_dtype[iop],
                                        op_axes ? op_axes[iop] : NULL);
            if (out == NULL) {
                return 0;
            }

            op[iop] = out;

            /*
             * Now we need to replace the pointers and strides with values
             * from the new array.
             */
            npyiter_replace_axisdata(iter, iop, op[iop], ndim,
                    op_axes ? op_axes[iop] : NULL);

            /* New arrays need no cast */
            op_itflags[iop] &= ~NPY_OP_ITFLAG_CAST;
        }
        /*
         * If casting is required, the operand is read-only, and
         * it's an array scalar, make a copy whether or not the
         * copy flag is enabled.
         */
        else if ((op_itflags[iop] & (NPY_OP_ITFLAG_CAST |
                         NPY_OP_ITFLAG_READ |
                         NPY_OP_ITFLAG_WRITE)) == (NPY_OP_ITFLAG_CAST |
                                                   NPY_OP_ITFLAG_READ) &&
                          PyArray_NDIM(op[iop]) == 0) {
            PyArrayObject *temp;
            Py_INCREF(op_dtype[iop]);
            temp = (PyArrayObject *)PyArray_NewFromDescr(
                                        &PyArray_Type, op_dtype[iop],
                                        0, NULL, NULL, NULL, 0, NULL);
            if (temp == NULL) {
                return 0;
            }
            if (PyArray_CopyInto(temp, op[iop]) != 0) {
                Py_DECREF(temp);
                return 0;
            }
            Py_DECREF(op[iop]);
            op[iop] = temp;

            /*
             * Now we need to replace the pointers and strides with values
             * from the temporary array.
             */
            npyiter_replace_axisdata(iter, iop, op[iop], 0, NULL);

            /* New arrays need no cast */
            op_itflags[iop] &= ~NPY_OP_ITFLAG_CAST;
        }
        /*
         * Make a temporary copy if,
         * 1. If casting is required and permitted, or,
         * 2. If force-copy is requested
         */
        else if (((op_itflags[iop] & NPY_OP_ITFLAG_CAST) &&
                        (op_flags[iop] &
                        (NPY_ITER_COPY|NPY_ITER_UPDATEIFCOPY))) ||
                 (op_itflags[iop] & NPY_OP_ITFLAG_FORCECOPY)) {
            PyArrayObject *temp;
            int ondim = PyArray_NDIM(op[iop]);

            /* Allocate the temporary array, if possible */
            temp = npyiter_new_temp_array(iter, &PyArray_Type,
                                        flags, &op_itflags[iop],
                                        ondim,
                                        PyArray_DIMS(op[iop]),
                                        op_dtype[iop],
                                        op_axes ? op_axes[iop] : NULL);
            if (temp == NULL) {
                return 0;
            }

            /*
             * If the data will be read, copy it into temp.
             * TODO: It might be possible to do a view into
             *       op[iop]'s mask instead here.
             */
            if (op_itflags[iop] & NPY_OP_ITFLAG_READ) {
                if (PyArray_CopyInto(temp, op[iop]) != 0) {
                    Py_DECREF(temp);
                    return 0;
                }
            }
            /* If the data will be written to, set WRITEBACKIFCOPY
               and require a context manager */
            if (op_itflags[iop] & NPY_OP_ITFLAG_WRITE) {
                Py_INCREF(op[iop]);
                if (PyArray_SetWritebackIfCopyBase(temp, op[iop]) < 0) {
                    Py_DECREF(temp);
                    return 0;
                }
                op_itflags[iop] |= NPY_OP_ITFLAG_HAS_WRITEBACK;
            }

            Py_DECREF(op[iop]);
            op[iop] = temp;

            /*
             * Now we need to replace the pointers and strides with values
             * from the temporary array.
             */
            npyiter_replace_axisdata(iter, iop, op[iop], ondim,
                    op_axes ? op_axes[iop] : NULL);

            /* The temporary copy needs no cast */
            op_itflags[iop] &= ~NPY_OP_ITFLAG_CAST;
        }
        else {
            /*
             * Buffering must be enabled for casting/conversion if copy
             * wasn't specified.
             */
            if ((op_itflags[iop] & NPY_OP_ITFLAG_CAST) &&
                                  !(itflags & NPY_ITFLAG_BUFFER)) {
                PyErr_SetString(PyExc_TypeError,
                        "Iterator operand required copying or buffering, "
                        "but neither copying nor buffering was enabled");
                return 0;
            }
        }

        /* Here we can finally check for contiguous iteration */
        if (op_itflags[iop] & NPY_OP_ITFLAG_CONTIG) {
            NpyIter_AxisData *axisdata = NIT_AXISDATA(iter);
            npy_intp stride = NAD_STRIDES(axisdata)[iop];

            if (stride != op_dtype[iop]->elsize) {
                /*
                 * Need to copy to buffer (cast) to ensure contiguous
                 * NOTE: This is the wrong place in case of axes reorder
                 *       (there is an xfailing test for this).
                 */
                NPY_IT_DBG_PRINT("Iterator: Setting NPY_OP_ITFLAG_CAST "
                                    "because of NPY_ITER_CONTIG\n");
                op_itflags[iop] |= NPY_OP_ITFLAG_CAST;
                if (!(itflags & NPY_ITFLAG_BUFFER)) {
                    PyErr_SetString(PyExc_TypeError,
                            "Iterator operand required buffering, "
                            "to be contiguous as requested, but "
                            "buffering is not enabled");
                    return 0;
                }
            }
        }
    }

    if (check_writemasked_reductions) {
        for (iop = 0; iop < nop; ++iop) {
            /*
             * Check whether there are any WRITEMASKED REDUCE operands
             * which should be validated now that all the strides are filled
             * in.
             */
            if ((op_itflags[iop] &
                    (NPY_OP_ITFLAG_WRITEMASKED | NPY_OP_ITFLAG_REDUCE)) ==
                        (NPY_OP_ITFLAG_WRITEMASKED | NPY_OP_ITFLAG_REDUCE)) {
                /*
                 * If the ARRAYMASK has 'bigger' dimensions
                 * than this REDUCE WRITEMASKED operand,
                 * the result would be more than one mask
                 * value per reduction element, something which
                 * is invalid. This function provides validation
                 * for that.
                 */
                if (!check_mask_for_writemasked_reduction(iter, iop)) {
                    return 0;
                }
            }
        }
    }

    return 1;
}

/*
 * The __array_priority__ attribute of the inputs determines
 * the subtype of any output arrays.  This function finds the
 * subtype of the input array with highest priority.
 */
static void
npyiter_get_priority_subtype(int nop, PyArrayObject **op,
                            const npyiter_opitflags *op_itflags,
                            double *subtype_priority,
                            PyTypeObject **subtype)
{
    int iop;

    for (iop = 0; iop < nop; ++iop) {
        if (op[iop] != NULL && op_itflags[iop] & NPY_OP_ITFLAG_READ) {
            double priority = PyArray_GetPriority((PyObject *)op[iop], 0.0);
            if (priority > *subtype_priority) {
                *subtype_priority = priority;
                *subtype = Py_TYPE(op[iop]);
            }
        }
    }
}

static int
npyiter_allocate_transfer_functions(NpyIter *iter)
{
    npy_uint32 itflags = NIT_ITFLAGS(iter);
    /*int ndim = NIT_NDIM(iter);*/
    int iop = 0, nop = NIT_NOP(iter);

    npy_intp i;
    npyiter_opitflags *op_itflags = NIT_OPITFLAGS(iter);
    NpyIter_BufferData *bufferdata = NIT_BUFFERDATA(iter);
    NpyIter_AxisData *axisdata = NIT_AXISDATA(iter);
    PyArrayObject **op = NIT_OPERANDS(iter);
    PyArray_Descr **op_dtype = NIT_DTYPES(iter);
    npy_intp *strides = NAD_STRIDES(axisdata), op_stride;
    NpyIter_TransferInfo *transferinfo = NBF_TRANSFERINFO(bufferdata);

    npy_intp sizeof_axisdata = NIT_AXISDATA_SIZEOF(itflags, ndim, nop);
    NpyIter_AxisData *reduce_axisdata = NIT_INDEX_AXISDATA(axisdata, bufferdata->outerdim);
    npy_intp *reduce_strides = NAD_STRIDES(reduce_axisdata);

    /* combined cast flags, the new cast flags for each cast: */
    NPY_ARRAYMETHOD_FLAGS cflags = PyArrayMethod_MINIMAL_FLAGS;
    NPY_ARRAYMETHOD_FLAGS nc_flags;

    for (iop = 0; iop < nop; ++iop) {
        npyiter_opitflags flags = op_itflags[iop];

        /*
         * Reduce operands buffer the outer stride if it is nonzero; compare
         * `npyiter_fill_buffercopy_params`.
         * (Inner strides cannot _all_ be zero if the outer is, but some.)
         */
        if ((op_itflags[iop] & NPY_OP_ITFLAG_REDUCE) && reduce_strides[iop] != 0) {
            op_stride = reduce_strides[iop];
        }
        else {
            op_stride = strides[iop];
        }

        /*
         * If we have determined that a buffer may be needed,
         * allocate the appropriate transfer functions
         */
        if (!(flags & NPY_OP_ITFLAG_BUFNEVER)) {
            int aligned = IsUintAligned(op[iop]);
            if (flags & NPY_OP_ITFLAG_READ) {
                int move_references = 0;
                if (PyArray_GetDTypeTransferFunction(
                                        aligned,
                                        op_stride,
                                        op_dtype[iop]->elsize,
                                        PyArray_DESCR(op[iop]),
                                        op_dtype[iop],
                                        move_references,
                                        &transferinfo[iop].read,
                                        &nc_flags) != NPY_SUCCEED) {
                    iop -= 1;  /* This one cannot be cleaned up yet. */
                    goto fail;
                }
                cflags = PyArrayMethod_COMBINED_FLAGS(cflags, nc_flags);
            }
            else {
                transferinfo[iop].read.func = NULL;
            }
            if (flags & NPY_OP_ITFLAG_WRITE) {
                int move_references = 1;

                /* If the operand is WRITEMASKED, use a masked transfer fn */
                if (flags & NPY_OP_ITFLAG_WRITEMASKED) {
                    int maskop = NIT_MASKOP(iter);
                    PyArray_Descr *mask_dtype = PyArray_DESCR(op[maskop]);

                    /*
                     * If the mask's stride is contiguous, use it, otherwise
                     * the mask may or may not be buffered, so the stride
                     * could be inconsistent.
                     */
                    if (PyArray_GetMaskedDTypeTransferFunction(
                            aligned,
                            op_dtype[iop]->elsize,
                            op_stride,
                            (strides[maskop] == mask_dtype->elsize) ?
                                mask_dtype->elsize : NPY_MAX_INTP,
                            op_dtype[iop],
                            PyArray_DESCR(op[iop]),
                            mask_dtype,
                            move_references,
                            &transferinfo[iop].write,
                            &nc_flags) != NPY_SUCCEED) {
                        goto fail;
                    }
                    cflags = PyArrayMethod_COMBINED_FLAGS(cflags, nc_flags);
                }
                else {
                    if (PyArray_GetDTypeTransferFunction(
                            aligned,
                            op_dtype[iop]->elsize,
                            op_stride,
                            op_dtype[iop],
                            PyArray_DESCR(op[iop]),
                            move_references,
                            &transferinfo[iop].write,
                            &nc_flags) != NPY_SUCCEED) {
                        goto fail;
                    }
                    cflags = PyArrayMethod_COMBINED_FLAGS(cflags, nc_flags);
                }
            }
            else {
                transferinfo[iop].write.func = NULL;
            }

            /* Get the decref function, used for no-writeback and on error */
            if (PyDataType_REFCHK(op_dtype[iop])) {
                /*
                 * By passing NULL to dst_type and setting move_references
                 * to 1, we get back a function that just decrements the
                 * src references.
                 */
                if (PyArray_GetClearFunction(
                        aligned,
                        op_dtype[iop]->elsize, op_dtype[iop],
                        &transferinfo[iop].clear, &nc_flags) < 0) {
                    goto fail;
                }
                cflags = PyArrayMethod_COMBINED_FLAGS(cflags, nc_flags);
            }
            else {
                transferinfo[iop].clear.func = NULL;
            }
        }
        else {
            transferinfo[iop].read.func = NULL;
            transferinfo[iop].write.func = NULL;
            transferinfo[iop].clear.func = NULL;
        }
    }

    /* Store the combined transfer flags on the iterator */
    NIT_ITFLAGS(iter) |= cflags << NPY_ITFLAG_TRANSFERFLAGS_SHIFT;
    assert(NIT_ITFLAGS(iter) >> NPY_ITFLAG_TRANSFERFLAGS_SHIFT == cflags);

    return 1;

fail:
    for (i = 0; i < iop+1; ++i) {
        NPY_cast_info_xfree(&transferinfo[iop].read);
        NPY_cast_info_xfree(&transferinfo[iop].write);
    }
    return 0;
}

#undef NPY_ITERATOR_IMPLEMENTATION_CODE
"#;

#[test]
pub(crate) fn cpp_3_test() {
    let text = {
        let source_code1 = CODE_MACRO_SEMICOLON;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::default();
    let mut md_cache = Default::default();
    let mut tree_gen = CppTreeGen::new(&mut stores, &mut md_cache);
    let x = tree_gen.generate_file(b"", text, tree.walk()).local;
    println!(
        "{}",
        hyperast::nodes::SyntaxSerializer::new(&stores, x.compressed_node)
    );
    // println!("{}", tree.root_node().to_sexp());
}

pub(crate) const CODE_MACRO_SEMICOLON: &str = r#"

enum Color {
};

ENABLE_OPERATORS_ON(PieceType)
ENABLE_OPERATORS_ON(Piece)
ENABLE_OPERATORS_ON(Color)

/// Important: If the material values are changed, one must also

const Value PawnValueMidgame   = Value(0x0C6);
const Value PawnValueEndgame   = Value(0x102);

int main() {
    printf("MapViewOfFile() failed, name = %s%s, error = %lu.\n", name, suffix, GetLastError());
}

"#;

#[allow(unused)]
pub(crate) const CODE_3: &str = r#"/*
Stockfish, a UCI chess playing engine derived from Glaurung 2.1
Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
Copyright (C) 2008-2010 Marco Costalba, Joona Kiiski, Tord Romstad

Stockfish is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Stockfish is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#if !defined(PIECE_H_INCLUDED)
#define PIECE_H_INCLUDED

#include "value.h"

enum PieceType {
PIECE_TYPE_NONE = 0,
PAWN = 1, KNIGHT = 2, BISHOP = 3, ROOK = 4, QUEEN = 5, KING = 6
};

enum Piece {
PIECE_NONE_DARK_SQ = 0, WP = 1, WN = 2, WB = 3, WR = 4, WQ = 5, WK = 6,
BP = 9, BN = 10, BB = 11, BR = 12, BQ = 13, BK = 14, PIECE_NONE = 16
};

enum Color {
WHITE, BLACK, COLOR_NONE
};

ENABLE_OPERATORS_ON(PieceType)
ENABLE_OPERATORS_ON(Piece)
ENABLE_OPERATORS_ON(Color)

/// Important: If the material values are changed, one must also
/// adjust the piece square tables, and the method game_phase() in the
/// Position class!
///
/// Values modified by Joona Kiiski

const Value PawnValueMidgame   = Value(0x0C6);
const Value PawnValueEndgame   = Value(0x102);
const Value KnightValueMidgame = Value(0x331);
const Value KnightValueEndgame = Value(0x34E);
const Value BishopValueMidgame = Value(0x344);
const Value BishopValueEndgame = Value(0x359);
const Value RookValueMidgame   = Value(0x4F6);
const Value RookValueEndgame   = Value(0x4FE);
const Value QueenValueMidgame  = Value(0x9D9);
const Value QueenValueEndgame  = Value(0x9FE);

inline Piece make_piece(Color c, PieceType pt) {
return Piece((int(c) << 3) | int(pt));
}

inline PieceType type_of_piece(Piece p)  {
return PieceType(int(p) & 7);
}

inline Color color_of_piece(Piece p) {
return Color(int(p) >> 3);
}

inline Color opposite_color(Color c) {
return Color(int(c) ^ 1);
}

inline bool color_is_ok(Color c) {
return c == WHITE || c == BLACK;
}

inline bool piece_type_is_ok(PieceType pt) {
return pt >= PAWN && pt <= KING;
}

inline bool piece_is_ok(Piece p) {
return piece_type_is_ok(type_of_piece(p)) && color_is_ok(color_of_piece(p));
}

inline char piece_type_to_char(PieceType pt) {
static const char ch[] = " PNBRQK";
return ch[pt];
}

#endif // !defined(PIECE_H_INCLUDED)
"#;

#[allow(unused, non_upper_case_globals)]
pub(crate) const CODE_OP_whole_save: &str = r#"/*
Stockfish, a UCI chess playing engine derived from Glaurung 2.1
Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
Copyright (C) 2008-2010 Marco Costalba, Joona Kiiski, Tord Romstad

Stockfish is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Stockfish is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


////
//// Includes
////

#include <algorithm>
#include <cassert>
#include <cstring>
#include <fstream>
#include <map>
#include <iostream>
#include <sstream>

#include "bitcount.h"
#include "movegen.h"
#include "movepick.h"
#include "position.h"
#include "psqtab.h"
#include "rkiss.h"
#include "san.h"
#include "tt.h"
#include "ucioption.h"

using std::string;
using std::cout;
using std::endl;


////
//// Position's static data definitions
////

Key Position::zobrist[2][8][64];
Key Position::zobEp[64];
Key Position::zobCastle[16];
Key Position::zobSideToMove;
Key Position::zobExclusion;

Score Position::PieceSquareTable[16][64];

// Material values arrays, indexed by Piece
const Value Position::PieceValueMidgame[17] = {
VALUE_ZERO,
PawnValueMidgame, KnightValueMidgame, BishopValueMidgame,
RookValueMidgame, QueenValueMidgame, VALUE_ZERO,
VALUE_ZERO, VALUE_ZERO,
PawnValueMidgame, KnightValueMidgame, BishopValueMidgame,
RookValueMidgame, QueenValueMidgame
};

const Value Position::PieceValueEndgame[17] = {
VALUE_ZERO,
PawnValueEndgame, KnightValueEndgame, BishopValueEndgame,
RookValueEndgame, QueenValueEndgame, VALUE_ZERO,
VALUE_ZERO, VALUE_ZERO,
PawnValueEndgame, KnightValueEndgame, BishopValueEndgame,
RookValueEndgame, QueenValueEndgame
};

// Material values array used by SEE, indexed by PieceType
const Value Position::seeValues[] = {
  VALUE_ZERO,
  PawnValueMidgame, KnightValueMidgame, BishopValueMidgame,
  RookValueMidgame, QueenValueMidgame, QueenValueMidgame*10
};


namespace {

// Bonus for having the side to move (modified by Joona Kiiski)
const Score TempoValue = make_score(48, 22);

bool isZero(char c) { return c == '0'; }

struct PieceLetters : public std::map<char, Piece> {

  PieceLetters() {

    operator[]('K') = WK; operator[]('k') = BK;
    operator[]('Q') = WQ; operator[]('q') = BQ;
    operator[]('R') = WR; operator[]('r') = BR;
    operator[]('B') = WB; operator[]('b') = BB;
    operator[]('N') = WN; operator[]('n') = BN;
    operator[]('P') = WP; operator[]('p') = BP;
    operator[](' ') = PIECE_NONE; operator[]('.') = PIECE_NONE_DARK_SQ;
  }

  char from_piece(Piece p) const {

      std::map<char, Piece>::const_iterator it;
      for (it = begin(); it != end(); ++it)
          if (it->second == p)
              return it->first;

      assert(false);
      return 0;
  }
} pieceLetters;
}


/// CheckInfo c'tor

CheckInfo::CheckInfo(const Position& pos) {

Color us = pos.side_to_move();
Color them = opposite_color(us);

ksq = pos.king_square(them);
dcCandidates = pos.discovered_check_candidates(us);

checkSq[PAWN] = pos.attacks_from<PAWN>(ksq, them);
checkSq[KNIGHT] = pos.attacks_from<KNIGHT>(ksq);
checkSq[BISHOP] = pos.attacks_from<BISHOP>(ksq);
checkSq[ROOK] = pos.attacks_from<ROOK>(ksq);
checkSq[QUEEN] = checkSq[BISHOP] | checkSq[ROOK];
checkSq[KING] = EmptyBoardBB;
}


/// Position c'tors. Here we always create a copy of the original position
/// or the FEN string, we want the new born Position object do not depend
/// on any external data so we detach state pointer from the source one.

Position::Position(const Position& pos, int th) {

memcpy(this, &pos, sizeof(Position));
detach(); // Always detach() in copy c'tor to avoid surprises
threadID = th;
nodes = 0;
}

Position::Position(const string& fen, int th) {

from_fen(fen);
threadID = th;
}


/// Position::detach() copies the content of the current state and castling
/// masks inside the position itself. This is needed when the st pointee could
/// become stale, as example because the caller is about to going out of scope.

void Position::detach() {

startState = *st;
st = &startState;
st->previous = NULL; // as a safe guard
}


/// Position::from_fen() initializes the position object with the given FEN
/// string. This function is not very robust - make sure that input FENs are
/// correct (this is assumed to be the responsibility of the GUI).

void Position::from_fen(const string& fen) {
/*
 A FEN string defines a particular position using only the ASCII character set.

 A FEN string contains six fields. The separator between fields is a space. The fields are:

 1) Piece placement (from white's perspective). Each rank is described, starting with rank 8 and ending
    with rank 1; within each rank, the contents of each square are described from file a through file h.
    Following the Standard Algebraic Notation (SAN), each piece is identified by a single letter taken
    from the standard English names. White pieces are designated using upper-case letters ("PNBRQK")
    while Black take lowercase ("pnbrqk"). Blank squares are noted using digits 1 through 8 (the number
    of blank squares), and "/" separate ranks.

 2) Active color. "w" means white moves next, "b" means black.

 3) Castling availability. If neither side can castle, this is "-". Otherwise, this has one or more
    letters: "K" (White can castle kingside), "Q" (White can castle queenside), "k" (Black can castle
    kingside), and/or "q" (Black can castle queenside).

 4) En passant target square in algebraic notation. If there's no en passant target square, this is "-".
    If a pawn has just made a 2-square move, this is the position "behind" the pawn. This is recorded
    regardless of whether there is a pawn in position to make an en passant capture.

 5) Halfmove clock: This is the number of halfmoves since the last pawn advance or capture. This is used
    to determine if a draw can be claimed under the fifty-move rule.

 6) Fullmove number: The number of the full move. It starts at 1, and is incremented after Black's move.
*/

char token;
std::istringstream ss(fen);
Rank rank = RANK_8;
File file = FILE_A;

clear();

// 1. Piece placement field
while (ss.get(token) && token != ' ')
{
    if (isdigit(token))
    {
        file += File(token - '0'); // Skip the given number of files
        continue;
    }
    else if (token == '/')
    {
        file = FILE_A;
        rank--;
        continue;
    }

    if (pieceLetters.find(token) == pieceLetters.end())
        goto incorrect_fen;

    put_piece(pieceLetters[token], make_square(file, rank));
    file++;
}

// 2. Active color
if (!ss.get(token) || (token != 'w' && token != 'b'))
    goto incorrect_fen;

sideToMove = (token == 'w' ? WHITE : BLACK);

if (!ss.get(token) || token != ' ')
    goto incorrect_fen;

// 3. Castling availability
while (ss.get(token) && token != ' ')
{
    if (token == '-')
        continue;

    if (!set_castling_rights(token))
        goto incorrect_fen;
}

// 4. En passant square -- ignore if no capture is possible
char col, row;
if (   (ss.get(col) && (col >= 'a' && col <= 'h'))
    && (ss.get(row) && (row == '3' || row == '6')))
{
    Square fenEpSquare = make_square(file_from_char(col), rank_from_char(row));
    Color them = opposite_color(sideToMove);

    if (attacks_from<PAWN>(fenEpSquare, them) & pieces(PAWN, sideToMove))
        st->epSquare = fenEpSquare;
}

// 5-6. Halfmove clock and fullmove number are not parsed

// Various initialisations
castleRightsMask[make_square(initialKFile,  RANK_1)] ^= WHITE_OO | WHITE_OOO;
castleRightsMask[make_square(initialKFile,  RANK_8)] ^= BLACK_OO | BLACK_OOO;
castleRightsMask[make_square(initialKRFile, RANK_1)] ^= WHITE_OO;
castleRightsMask[make_square(initialKRFile, RANK_8)] ^= BLACK_OO;
castleRightsMask[make_square(initialQRFile, RANK_1)] ^= WHITE_OOO;
castleRightsMask[make_square(initialQRFile, RANK_8)] ^= BLACK_OOO;

isChess960 =   initialKFile  != FILE_E
            || initialQRFile != FILE_A
            || initialKRFile != FILE_H;

find_checkers();

st->key = compute_key();
st->pawnKey = compute_pawn_key();
st->materialKey = compute_material_key();
st->value = compute_value();
st->npMaterial[WHITE] = compute_non_pawn_material(WHITE);
st->npMaterial[BLACK] = compute_non_pawn_material(BLACK);
return;

incorrect_fen:
cout << "Error in FEN string: " << fen << endl;
}


/// Position::set_castling_rights() sets castling parameters castling avaiability.
/// This function is compatible with 3 standards: Normal FEN standard, Shredder-FEN
/// that uses the letters of the columns on which the rooks began the game instead
/// of KQkq and also X-FEN standard that, in case of Chess960, if an inner Rook is
/// associated with the castling right, the traditional castling tag will be replaced
/// by the file letter of the involved rook as for the Shredder-FEN.

bool Position::set_castling_rights(char token) {

  Color c = token >= 'a' ? BLACK : WHITE;
  Square sqA = (c == WHITE ? SQ_A1 : SQ_A8);
  Square sqH = (c == WHITE ? SQ_H1 : SQ_H8);
  Piece rook = (c == WHITE ? WR : BR);

  initialKFile = square_file(king_square(c));
  token = char(toupper(token));

  if (token == 'K')
  {
      for (Square sq = sqH; sq >= sqA; sq--)
          if (piece_on(sq) == rook)
          {
              do_allow_oo(c);
              initialKRFile = square_file(sq);
              break;
          }
  }
  else if (token == 'Q')
  {
      for (Square sq = sqA; sq <= sqH; sq++)
          if (piece_on(sq) == rook)
          {
              do_allow_ooo(c);
              initialQRFile = square_file(sq);
              break;
          }
  }
  else if (token >= 'A' && token <= 'H')
  {
      File rookFile = File(token - 'A') + FILE_A;
      if (rookFile < initialKFile)
      {
          do_allow_ooo(c);
          initialQRFile = rookFile;
      }
      else
      {
          do_allow_oo(c);
          initialKRFile = rookFile;
      }
  }
  else return false;

return true;
}


/// Position::to_fen() returns a FEN representation of the position. In case
/// of Chess960 the Shredder-FEN notation is used. Mainly a debugging function.

const string Position::to_fen() const {

string fen;
Square sq;
char emptyCnt = '0';

for (Rank rank = RANK_8; rank >= RANK_1; rank--)
{
    for (File file = FILE_A; file <= FILE_H; file++)
    {
        sq = make_square(file, rank);

        if (square_is_occupied(sq))
        {
            fen += emptyCnt;
            fen += pieceLetters.from_piece(piece_on(sq));
            emptyCnt = '0';
        } else
            emptyCnt++;
    }
    fen += emptyCnt;
    fen += '/';
    emptyCnt = '0';
}

fen.erase(std::remove_if(fen.begin(), fen.end(), isZero), fen.end());
fen.erase(--fen.end());
fen += (sideToMove == WHITE ? " w " : " b ");

if (st->castleRights != CASTLES_NONE)
{
    if (can_castle_kingside(WHITE))
        fen += isChess960 ? char(toupper(file_to_char(initialKRFile))) : 'K';

    if (can_castle_queenside(WHITE))
        fen += isChess960 ? char(toupper(file_to_char(initialQRFile))) : 'Q';

    if (can_castle_kingside(BLACK))
        fen += isChess960 ? file_to_char(initialKRFile) : 'k';

    if (can_castle_queenside(BLACK))
        fen += isChess960 ? file_to_char(initialQRFile) : 'q';
} else
    fen += '-';

fen += (ep_square() == SQ_NONE ? " -" : " " + square_to_string(ep_square()));
return fen;
}


/// Position::print() prints an ASCII representation of the position to
/// the standard output. If a move is given then also the san is printed.

void Position::print(Move move) const {

const char* dottedLine = "\n+---+---+---+---+---+---+---+---+\n";
static bool requestPending = false;

// Check for reentrancy, as example when called from inside
// MovePicker that is used also here in move_to_san()
if (requestPending)
    return;

requestPending = true;

if (move)
{
    Position p(*this, thread());
    string dd = (color_of_piece_on(move_from(move)) == BLACK ? ".." : "");
    cout << "\nMove is: " << dd << move_to_san(p, move);
}

for (Rank rank = RANK_8; rank >= RANK_1; rank--)
{
    cout << dottedLine << '|';
    for (File file = FILE_A; file <= FILE_H; file++)
    {
        Square sq = make_square(file, rank);
        char c = (color_of_piece_on(sq) == BLACK ? '=' : ' ');
        Piece piece = piece_on(sq);

        if (piece == PIECE_NONE && square_color(sq) == DARK)
            piece = PIECE_NONE_DARK_SQ;

        cout << c << pieceLetters.from_piece(piece) << c << '|';
    }
}
cout << dottedLine << "Fen is: " << to_fen() << "\nKey is: " << st->key << endl;
requestPending = false;
}


/// Position:hidden_checkers<>() returns a bitboard of all pinned (against the
/// king) pieces for the given color and for the given pinner type. Or, when
/// template parameter FindPinned is false, the pieces of the given color
/// candidate for a discovery check against the enemy king.
/// Bitboard checkersBB must be already updated when looking for pinners.

template<bool FindPinned>
Bitboard Position::hidden_checkers(Color c) const {

Bitboard result = EmptyBoardBB;
Bitboard pinners = pieces_of_color(FindPinned ? opposite_color(c) : c);

// Pinned pieces protect our king, dicovery checks attack
// the enemy king.
Square ksq = king_square(FindPinned ? c : opposite_color(c));

// Pinners are sliders, not checkers, that give check when candidate pinned is removed
pinners &= (pieces(ROOK, QUEEN) & RookPseudoAttacks[ksq]) | (pieces(BISHOP, QUEEN) & BishopPseudoAttacks[ksq]);

if (FindPinned && pinners)
    pinners &= ~st->checkersBB;

while (pinners)
{
    Square s = pop_1st_bit(&pinners);
    Bitboard b = squares_between(s, ksq) & occupied_squares();

    assert(b);

    if (  !(b & (b - 1)) // Only one bit set?
        && (b & pieces_of_color(c))) // Is an our piece?
        result |= b;
}
return result;
}


/// Position:pinned_pieces() returns a bitboard of all pinned (against the
/// king) pieces for the given color. Note that checkersBB bitboard must
/// be already updated.

Bitboard Position::pinned_pieces(Color c) const {

return hidden_checkers<true>(c);
}


/// Position:discovered_check_candidates() returns a bitboard containing all
/// pieces for the given side which are candidates for giving a discovered
/// check. Contrary to pinned_pieces() here there is no need of checkersBB
/// to be already updated.

Bitboard Position::discovered_check_candidates(Color c) const {

return hidden_checkers<false>(c);
}

/// Position::attackers_to() computes a bitboard containing all pieces which
/// attacks a given square.

Bitboard Position::attackers_to(Square s) const {

return  (attacks_from<PAWN>(s, BLACK) & pieces(PAWN, WHITE))
      | (attacks_from<PAWN>(s, WHITE) & pieces(PAWN, BLACK))
      | (attacks_from<KNIGHT>(s)      & pieces(KNIGHT))
      | (attacks_from<ROOK>(s)        & pieces(ROOK, QUEEN))
      | (attacks_from<BISHOP>(s)      & pieces(BISHOP, QUEEN))
      | (attacks_from<KING>(s)        & pieces(KING));
}

/// Position::attacks_from() computes a bitboard of all attacks
/// of a given piece put in a given square.

Bitboard Position::attacks_from(Piece p, Square s) const {

assert(square_is_ok(s));

switch (p)
{
case WB: case BB: return attacks_from<BISHOP>(s);
case WR: case BR: return attacks_from<ROOK>(s);
case WQ: case BQ: return attacks_from<QUEEN>(s);
default: return StepAttackBB[p][s];
}
}

Bitboard Position::attacks_from(Piece p, Square s, Bitboard occ) {

assert(square_is_ok(s));

switch (p)
{
case WB: case BB: return bishop_attacks_bb(s, occ);
case WR: case BR: return rook_attacks_bb(s, occ);
case WQ: case BQ: return bishop_attacks_bb(s, occ) | rook_attacks_bb(s, occ);
default: return StepAttackBB[p][s];
}
}


/// Position::move_attacks_square() tests whether a move from the current
/// position attacks a given square.

bool Position::move_attacks_square(Move m, Square s) const {

assert(move_is_ok(m));
assert(square_is_ok(s));

Bitboard occ, xray;
Square f = move_from(m), t = move_to(m);

assert(square_is_occupied(f));

if (bit_is_set(attacks_from(piece_on(f), t), s))
    return true;

// Move the piece and scan for X-ray attacks behind it
occ = occupied_squares();
do_move_bb(&occ, make_move_bb(f, t));
xray = ( (rook_attacks_bb(s, occ)   & pieces(ROOK, QUEEN))
        |(bishop_attacks_bb(s, occ) & pieces(BISHOP, QUEEN)))
       & pieces_of_color(color_of_piece_on(f));

// If we have attacks we need to verify that are caused by our move
// and are not already existent ones.
return xray && (xray ^ (xray & attacks_from<QUEEN>(s)));
}


/// Position::find_checkers() computes the checkersBB bitboard, which
/// contains a nonzero bit for each checking piece (0, 1 or 2). It
/// currently works by calling Position::attackers_to, which is probably
/// inefficient. Consider rewriting this function to use the last move
/// played, like in non-bitboard versions of Glaurung.

void Position::find_checkers() {

Color us = side_to_move();
st->checkersBB = attackers_to(king_square(us)) & pieces_of_color(opposite_color(us));
}


/// Position::pl_move_is_legal() tests whether a pseudo-legal move is legal

bool Position::pl_move_is_legal(Move m, Bitboard pinned) const {

assert(is_ok());
assert(move_is_ok(m));
assert(pinned == pinned_pieces(side_to_move()));

// Castling moves are checked for legality during move generation.
if (move_is_castle(m))
    return true;

// En passant captures are a tricky special case. Because they are
// rather uncommon, we do it simply by testing whether the king is attacked
// after the move is made
if (move_is_ep(m))
{
    Color us = side_to_move();
    Color them = opposite_color(us);
    Square from = move_from(m);
    Square to = move_to(m);
    Square capsq = make_square(square_file(to), square_rank(from));
    Square ksq = king_square(us);
    Bitboard b = occupied_squares();

    assert(to == ep_square());
    assert(piece_on(from) == piece_of_color_and_type(us, PAWN));
    assert(piece_on(capsq) == piece_of_color_and_type(them, PAWN));
    assert(piece_on(to) == PIECE_NONE);

    clear_bit(&b, from);
    clear_bit(&b, capsq);
    set_bit(&b, to);

    return   !(rook_attacks_bb(ksq, b) & pieces(ROOK, QUEEN, them))
          && !(bishop_attacks_bb(ksq, b) & pieces(BISHOP, QUEEN, them));
}

Color us = side_to_move();
Square from = move_from(m);

assert(color_of_piece_on(from) == us);
assert(piece_on(king_square(us)) == piece_of_color_and_type(us, KING));

// If the moving piece is a king, check whether the destination
// square is attacked by the opponent.
if (type_of_piece_on(from) == KING)
    return !(attackers_to(move_to(m)) & pieces_of_color(opposite_color(us)));

// A non-king move is legal if and only if it is not pinned or it
// is moving along the ray towards or away from the king.
return (   !pinned
        || !bit_is_set(pinned, from)
        || (direction_between_squares(from, king_square(us)) == direction_between_squares(move_to(m), king_square(us))));
}


/// Position::pl_move_is_evasion() tests whether a pseudo-legal move is a legal evasion

bool Position::pl_move_is_evasion(Move m, Bitboard pinned) const
{
assert(is_check());

Color us = side_to_move();
Square from = move_from(m);
Square to = move_to(m);

// King moves and en-passant captures are verified in pl_move_is_legal()
if (type_of_piece_on(from) == KING || move_is_ep(m))
    return pl_move_is_legal(m, pinned);

Bitboard target = checkers();
Square checksq = pop_1st_bit(&target);

if (target) // double check ?
    return false;

// Our move must be a blocking evasion or a capture of the checking piece
target = squares_between(checksq, king_square(us)) | checkers();
return bit_is_set(target, to) && pl_move_is_legal(m, pinned);
}


/// Position::move_is_check() tests whether a pseudo-legal move is a check

bool Position::move_is_check(Move m) const {

return move_is_check(m, CheckInfo(*this));
}

bool Position::move_is_check(Move m, const CheckInfo& ci) const {

assert(is_ok());
assert(move_is_ok(m));
assert(ci.dcCandidates == discovered_check_candidates(side_to_move()));
assert(color_of_piece_on(move_from(m)) == side_to_move());
assert(piece_on(ci.ksq) == piece_of_color_and_type(opposite_color(side_to_move()), KING));

Square from = move_from(m);
Square to = move_to(m);
PieceType pt = type_of_piece_on(from);

// Direct check ?
if (bit_is_set(ci.checkSq[pt], to))
    return true;

// Discovery check ?
if (ci.dcCandidates && bit_is_set(ci.dcCandidates, from))
{
    // For pawn and king moves we need to verify also direction
    if (  (pt != PAWN && pt != KING)
        ||(direction_between_squares(from, ci.ksq) != direction_between_squares(to, ci.ksq)))
        return true;
}

// Can we skip the ugly special cases ?
if (!move_is_special(m))
    return false;

Color us = side_to_move();
Bitboard b = occupied_squares();

// Promotion with check ?
if (move_is_promotion(m))
{
    clear_bit(&b, from);

    switch (move_promotion_piece(m))
    {
    case KNIGHT:
        return bit_is_set(attacks_from<KNIGHT>(to), ci.ksq);
    case BISHOP:
        return bit_is_set(bishop_attacks_bb(to, b), ci.ksq);
    case ROOK:
        return bit_is_set(rook_attacks_bb(to, b), ci.ksq);
    case QUEEN:
        return bit_is_set(queen_attacks_bb(to, b), ci.ksq);
    default:
        assert(false);
    }
}

// En passant capture with check ? We have already handled the case
// of direct checks and ordinary discovered check, the only case we
// need to handle is the unusual case of a discovered check through
// the captured pawn.
if (move_is_ep(m))
{
    Square capsq = make_square(square_file(to), square_rank(from));
    clear_bit(&b, from);
    clear_bit(&b, capsq);
    set_bit(&b, to);
    return  (rook_attacks_bb(ci.ksq, b) & pieces(ROOK, QUEEN, us))
          ||(bishop_attacks_bb(ci.ksq, b) & pieces(BISHOP, QUEEN, us));
}

// Castling with check ?
if (move_is_castle(m))
{
    Square kfrom, kto, rfrom, rto;
    kfrom = from;
    rfrom = to;

    if (rfrom > kfrom)
    {
        kto = relative_square(us, SQ_G1);
        rto = relative_square(us, SQ_F1);
    } else {
        kto = relative_square(us, SQ_C1);
        rto = relative_square(us, SQ_D1);
    }
    clear_bit(&b, kfrom);
    clear_bit(&b, rfrom);
    set_bit(&b, rto);
    set_bit(&b, kto);
    return bit_is_set(rook_attacks_bb(rto, b), ci.ksq);
}

return false;
}


/// Position::do_move() makes a move, and saves all information necessary
/// to a StateInfo object. The move is assumed to be legal.
/// Pseudo-legal moves should be filtered out before this function is called.

void Position::do_move(Move m, StateInfo& newSt) {

CheckInfo ci(*this);
do_move(m, newSt, ci, move_is_check(m, ci));
}

void Position::do_move(Move m, StateInfo& newSt, const CheckInfo& ci, bool moveIsCheck) {

assert(is_ok());
assert(move_is_ok(m));

nodes++;
Key key = st->key;

// Copy some fields of old state to our new StateInfo object except the
// ones which are recalculated from scratch anyway, then switch our state
// pointer to point to the new, ready to be updated, state.
struct ReducedStateInfo {
  Key pawnKey, materialKey;
  int castleRights, rule50, gamePly, pliesFromNull;
  Square epSquare;
  Score value;
  Value npMaterial[2];
};

if (&newSt != st)
    memcpy(&newSt, st, sizeof(ReducedStateInfo));

newSt.previous = st;
st = &newSt;

// Save the current key to the history[] array, in order to be able to
// detect repetition draws.
history[st->gamePly++] = key;

// Update side to move
key ^= zobSideToMove;

// Increment the 50 moves rule draw counter. Resetting it to zero in the
// case of non-reversible moves is taken care of later.
st->rule50++;
st->pliesFromNull++;

if (move_is_castle(m))
{
    st->key = key;
    do_castle_move(m);
    return;
}

Color us = side_to_move();
Color them = opposite_color(us);
Square from = move_from(m);
Square to = move_to(m);
bool ep = move_is_ep(m);
bool pm = move_is_promotion(m);

Piece piece = piece_on(from);
PieceType pt = type_of_piece(piece);
PieceType capture = ep ? PAWN : type_of_piece_on(to);

assert(color_of_piece_on(from) == us);
assert(color_of_piece_on(to) == them || square_is_empty(to));
assert(!(ep || pm) || piece == piece_of_color_and_type(us, PAWN));
assert(!pm || relative_rank(us, to) == RANK_8);

if (capture)
    do_capture_move(key, capture, them, to, ep);

// Update hash key
key ^= zobrist[us][pt][from] ^ zobrist[us][pt][to];

// Reset en passant square
if (st->epSquare != SQ_NONE)
{
    key ^= zobEp[st->epSquare];
    st->epSquare = SQ_NONE;
}

// Update castle rights, try to shortcut a common case
int cm = castleRightsMask[from] & castleRightsMask[to];
if (cm != ALL_CASTLES && ((cm & st->castleRights) != st->castleRights))
{
    key ^= zobCastle[st->castleRights];
    st->castleRights &= castleRightsMask[from];
    st->castleRights &= castleRightsMask[to];
    key ^= zobCastle[st->castleRights];
}

// Prefetch TT access as soon as we know key is updated
prefetch((char*)TT.first_entry(key));

// Move the piece
Bitboard move_bb = make_move_bb(from, to);
do_move_bb(&(byColorBB[us]), move_bb);
do_move_bb(&(byTypeBB[pt]), move_bb);
do_move_bb(&(byTypeBB[0]), move_bb); // HACK: byTypeBB[0] == occupied squares

board[to] = board[from];
board[from] = PIECE_NONE;

// Update piece lists, note that index[from] is not updated and
// becomes stale. This works as long as index[] is accessed just
// by known occupied squares.
index[to] = index[from];
pieceList[us][pt][index[to]] = to;

// If the moving piece was a pawn do some special extra work
if (pt == PAWN)
{
    // Reset rule 50 draw counter
    st->rule50 = 0;

    // Update pawn hash key and prefetch in L1/L2 cache
    st->pawnKey ^= zobrist[us][PAWN][from] ^ zobrist[us][PAWN][to];
    prefetchPawn(st->pawnKey, threadID);

    // Set en passant square, only if moved pawn can be captured
    if ((to ^ from) == 16)
    {
        if (attacks_from<PAWN>(from + (us == WHITE ? DELTA_N : DELTA_S), us) & pieces(PAWN, them))
        {
            st->epSquare = Square((int(from) + int(to)) / 2);
            key ^= zobEp[st->epSquare];
        }
    }

    if (pm) // promotion ?
    {
        PieceType promotion = move_promotion_piece(m);

        assert(promotion >= KNIGHT && promotion <= QUEEN);

        // Insert promoted piece instead of pawn
        clear_bit(&(byTypeBB[PAWN]), to);
        set_bit(&(byTypeBB[promotion]), to);
        board[to] = piece_of_color_and_type(us, promotion);

        // Update piece counts
        pieceCount[us][promotion]++;
        pieceCount[us][PAWN]--;

        // Update material key
        st->materialKey ^= zobrist[us][PAWN][pieceCount[us][PAWN]];
        st->materialKey ^= zobrist[us][promotion][pieceCount[us][promotion]-1];

        // Update piece lists, move the last pawn at index[to] position
        // and shrink the list. Add a new promotion piece to the list.
        Square lastPawnSquare = pieceList[us][PAWN][pieceCount[us][PAWN]];
        index[lastPawnSquare] = index[to];
        pieceList[us][PAWN][index[lastPawnSquare]] = lastPawnSquare;
        pieceList[us][PAWN][pieceCount[us][PAWN]] = SQ_NONE;
        index[to] = pieceCount[us][promotion] - 1;
        pieceList[us][promotion][index[to]] = to;

        // Partially revert hash keys update
        key ^= zobrist[us][PAWN][to] ^ zobrist[us][promotion][to];
        st->pawnKey ^= zobrist[us][PAWN][to];

        // Partially revert and update incremental scores
        st->value -= pst(us, PAWN, to);
        st->value += pst(us, promotion, to);

        // Update material
        st->npMaterial[us] += PieceValueMidgame[promotion];
    }
}

// Update incremental scores
st->value += pst_delta(piece, from, to);

// Set capture piece
st->capturedType = capture;

// Update the key with the final value
st->key = key;

// Update checkers bitboard, piece must be already moved
st->checkersBB = EmptyBoardBB;

if (moveIsCheck)
{
    if (ep | pm)
        st->checkersBB = attackers_to(king_square(them)) & pieces_of_color(us);
    else
    {
        // Direct checks
        if (bit_is_set(ci.checkSq[pt], to))
            st->checkersBB = SetMaskBB[to];

        // Discovery checks
        if (ci.dcCandidates && bit_is_set(ci.dcCandidates, from))
        {
            if (pt != ROOK)
                st->checkersBB |= (attacks_from<ROOK>(ci.ksq) & pieces(ROOK, QUEEN, us));

            if (pt != BISHOP)
                st->checkersBB |= (attacks_from<BISHOP>(ci.ksq) & pieces(BISHOP, QUEEN, us));
        }
    }
}

// Finish
sideToMove = opposite_color(sideToMove);
st->value += (sideToMove == WHITE ?  TempoValue : -TempoValue);

assert(is_ok());
}


/// Position::do_capture_move() is a private method used to update captured
/// piece info. It is called from the main Position::do_move function.

void Position::do_capture_move(Key& key, PieceType capture, Color them, Square to, bool ep) {

  assert(capture != KING);

  Square capsq = to;

  // If the captured piece was a pawn, update pawn hash key,
  // otherwise update non-pawn material.
  if (capture == PAWN)
  {
      if (ep) // en passant ?
      {
          capsq = (them == BLACK)? (to - DELTA_N) : (to - DELTA_S);

          assert(to == st->epSquare);
          assert(relative_rank(opposite_color(them), to) == RANK_6);
          assert(piece_on(to) == PIECE_NONE);
          assert(piece_on(capsq) == piece_of_color_and_type(them, PAWN));

          board[capsq] = PIECE_NONE;
      }
      st->pawnKey ^= zobrist[them][PAWN][capsq];
  }
  else
      st->npMaterial[them] -= PieceValueMidgame[capture];

  // Remove captured piece
  clear_bit(&(byColorBB[them]), capsq);
  clear_bit(&(byTypeBB[capture]), capsq);
  clear_bit(&(byTypeBB[0]), capsq);

  // Update hash key
  key ^= zobrist[them][capture][capsq];

  // Update incremental scores
  st->value -= pst(them, capture, capsq);

  // Update piece count
  pieceCount[them][capture]--;

  // Update material hash key
  st->materialKey ^= zobrist[them][capture][pieceCount[them][capture]];

  // Update piece list, move the last piece at index[capsq] position
  //
  // WARNING: This is a not perfectly revresible operation. When we
  // will reinsert the captured piece in undo_move() we will put it
  // at the end of the list and not in its original place, it means
  // index[] and pieceList[] are not guaranteed to be invariant to a
  // do_move() + undo_move() sequence.
  Square lastPieceSquare = pieceList[them][capture][pieceCount[them][capture]];
  index[lastPieceSquare] = index[capsq];
  pieceList[them][capture][index[lastPieceSquare]] = lastPieceSquare;
  pieceList[them][capture][pieceCount[them][capture]] = SQ_NONE;

  // Reset rule 50 counter
  st->rule50 = 0;
}


/// Position::do_castle_move() is a private method used to make a castling
/// move. It is called from the main Position::do_move function. Note that
/// castling moves are encoded as "king captures friendly rook" moves, for
/// instance white short castling in a non-Chess960 game is encoded as e1h1.

void Position::do_castle_move(Move m) {

assert(move_is_ok(m));
assert(move_is_castle(m));

Color us = side_to_move();
Color them = opposite_color(us);

// Reset capture field
st->capturedType = PIECE_TYPE_NONE;

// Find source squares for king and rook
Square kfrom = move_from(m);
Square rfrom = move_to(m);  // HACK: See comment at beginning of function
Square kto, rto;

assert(piece_on(kfrom) == piece_of_color_and_type(us, KING));
assert(piece_on(rfrom) == piece_of_color_and_type(us, ROOK));

// Find destination squares for king and rook
if (rfrom > kfrom) // O-O
{
    kto = relative_square(us, SQ_G1);
    rto = relative_square(us, SQ_F1);
} else { // O-O-O
    kto = relative_square(us, SQ_C1);
    rto = relative_square(us, SQ_D1);
}

// Remove pieces from source squares:
clear_bit(&(byColorBB[us]), kfrom);
clear_bit(&(byTypeBB[KING]), kfrom);
clear_bit(&(byTypeBB[0]), kfrom); // HACK: byTypeBB[0] == occupied squares
clear_bit(&(byColorBB[us]), rfrom);
clear_bit(&(byTypeBB[ROOK]), rfrom);
clear_bit(&(byTypeBB[0]), rfrom); // HACK: byTypeBB[0] == occupied squares

// Put pieces on destination squares:
set_bit(&(byColorBB[us]), kto);
set_bit(&(byTypeBB[KING]), kto);
set_bit(&(byTypeBB[0]), kto); // HACK: byTypeBB[0] == occupied squares
set_bit(&(byColorBB[us]), rto);
set_bit(&(byTypeBB[ROOK]), rto);
set_bit(&(byTypeBB[0]), rto); // HACK: byTypeBB[0] == occupied squares

// Update board array
Piece king = piece_of_color_and_type(us, KING);
Piece rook = piece_of_color_and_type(us, ROOK);
board[kfrom] = board[rfrom] = PIECE_NONE;
board[kto] = king;
board[rto] = rook;

// Update piece lists
pieceList[us][KING][index[kfrom]] = kto;
pieceList[us][ROOK][index[rfrom]] = rto;
int tmp = index[rfrom]; // In Chess960 could be rto == kfrom
index[kto] = index[kfrom];
index[rto] = tmp;

// Update incremental scores
st->value += pst_delta(king, kfrom, kto);
st->value += pst_delta(rook, rfrom, rto);

// Update hash key
st->key ^= zobrist[us][KING][kfrom] ^ zobrist[us][KING][kto];
st->key ^= zobrist[us][ROOK][rfrom] ^ zobrist[us][ROOK][rto];

// Clear en passant square
if (st->epSquare != SQ_NONE)
{
    st->key ^= zobEp[st->epSquare];
    st->epSquare = SQ_NONE;
}

// Update castling rights
st->key ^= zobCastle[st->castleRights];
st->castleRights &= castleRightsMask[kfrom];
st->key ^= zobCastle[st->castleRights];

// Reset rule 50 counter
st->rule50 = 0;

// Update checkers BB
st->checkersBB = attackers_to(king_square(them)) & pieces_of_color(us);

// Finish
sideToMove = opposite_color(sideToMove);
st->value += (sideToMove == WHITE ?  TempoValue : -TempoValue);

assert(is_ok());
}


/// Position::undo_move() unmakes a move. When it returns, the position should
/// be restored to exactly the same state as before the move was made.

void Position::undo_move(Move m) {

assert(is_ok());
assert(move_is_ok(m));

sideToMove = opposite_color(sideToMove);

if (move_is_castle(m))
{
    undo_castle_move(m);
    return;
}

Color us = side_to_move();
Color them = opposite_color(us);
Square from = move_from(m);
Square to = move_to(m);
bool ep = move_is_ep(m);
bool pm = move_is_promotion(m);

PieceType pt = type_of_piece_on(to);

assert(square_is_empty(from));
assert(color_of_piece_on(to) == us);
assert(!pm || relative_rank(us, to) == RANK_8);
assert(!ep || to == st->previous->epSquare);
assert(!ep || relative_rank(us, to) == RANK_6);
assert(!ep || piece_on(to) == piece_of_color_and_type(us, PAWN));

if (pm) // promotion ?
{
    PieceType promotion = move_promotion_piece(m);
    pt = PAWN;

    assert(promotion >= KNIGHT && promotion <= QUEEN);
    assert(piece_on(to) == piece_of_color_and_type(us, promotion));

    // Replace promoted piece with a pawn
    clear_bit(&(byTypeBB[promotion]), to);
    set_bit(&(byTypeBB[PAWN]), to);

    // Update piece counts
    pieceCount[us][promotion]--;
    pieceCount[us][PAWN]++;

    // Update piece list replacing promotion piece with a pawn
    Square lastPromotionSquare = pieceList[us][promotion][pieceCount[us][promotion]];
    index[lastPromotionSquare] = index[to];
    pieceList[us][promotion][index[lastPromotionSquare]] = lastPromotionSquare;
    pieceList[us][promotion][pieceCount[us][promotion]] = SQ_NONE;
    index[to] = pieceCount[us][PAWN] - 1;
    pieceList[us][PAWN][index[to]] = to;
}

// Put the piece back at the source square
Bitboard move_bb = make_move_bb(to, from);
do_move_bb(&(byColorBB[us]), move_bb);
do_move_bb(&(byTypeBB[pt]), move_bb);
do_move_bb(&(byTypeBB[0]), move_bb); // HACK: byTypeBB[0] == occupied squares

board[from] = piece_of_color_and_type(us, pt);
board[to] = PIECE_NONE;

// Update piece list
index[from] = index[to];
pieceList[us][pt][index[from]] = from;

if (st->capturedType)
{
    Square capsq = to;

    if (ep)
        capsq = (us == WHITE)? (to - DELTA_N) : (to - DELTA_S);

    assert(st->capturedType != KING);
    assert(!ep || square_is_empty(capsq));

    // Restore the captured piece
    set_bit(&(byColorBB[them]), capsq);
    set_bit(&(byTypeBB[st->capturedType]), capsq);
    set_bit(&(byTypeBB[0]), capsq);

    board[capsq] = piece_of_color_and_type(them, st->capturedType);

    // Update piece count
    pieceCount[them][st->capturedType]++;

    // Update piece list, add a new captured piece in capsq square
    index[capsq] = pieceCount[them][st->capturedType] - 1;
    pieceList[them][st->capturedType][index[capsq]] = capsq;
}

// Finally point our state pointer back to the previous state
st = st->previous;

assert(is_ok());
}


/// Position::undo_castle_move() is a private method used to unmake a castling
/// move. It is called from the main Position::undo_move function. Note that
/// castling moves are encoded as "king captures friendly rook" moves, for
/// instance white short castling in a non-Chess960 game is encoded as e1h1.

void Position::undo_castle_move(Move m) {

assert(move_is_ok(m));
assert(move_is_castle(m));

// When we have arrived here, some work has already been done by
// Position::undo_move.  In particular, the side to move has been switched,
// so the code below is correct.
Color us = side_to_move();

// Find source squares for king and rook
Square kfrom = move_from(m);
Square rfrom = move_to(m);  // HACK: See comment at beginning of function
Square kto, rto;

// Find destination squares for king and rook
if (rfrom > kfrom) // O-O
{
    kto = relative_square(us, SQ_G1);
    rto = relative_square(us, SQ_F1);
} else { // O-O-O
    kto = relative_square(us, SQ_C1);
    rto = relative_square(us, SQ_D1);
}

assert(piece_on(kto) == piece_of_color_and_type(us, KING));
assert(piece_on(rto) == piece_of_color_and_type(us, ROOK));

// Remove pieces from destination squares:
clear_bit(&(byColorBB[us]), kto);
clear_bit(&(byTypeBB[KING]), kto);
clear_bit(&(byTypeBB[0]), kto); // HACK: byTypeBB[0] == occupied squares
clear_bit(&(byColorBB[us]), rto);
clear_bit(&(byTypeBB[ROOK]), rto);
clear_bit(&(byTypeBB[0]), rto); // HACK: byTypeBB[0] == occupied squares

// Put pieces on source squares:
set_bit(&(byColorBB[us]), kfrom);
set_bit(&(byTypeBB[KING]), kfrom);
set_bit(&(byTypeBB[0]), kfrom); // HACK: byTypeBB[0] == occupied squares
set_bit(&(byColorBB[us]), rfrom);
set_bit(&(byTypeBB[ROOK]), rfrom);
set_bit(&(byTypeBB[0]), rfrom); // HACK: byTypeBB[0] == occupied squares

// Update board
board[rto] = board[kto] = PIECE_NONE;
board[rfrom] = piece_of_color_and_type(us, ROOK);
board[kfrom] = piece_of_color_and_type(us, KING);

// Update piece lists
pieceList[us][KING][index[kto]] = kfrom;
pieceList[us][ROOK][index[rto]] = rfrom;
int tmp = index[rto];  // In Chess960 could be rto == kfrom
index[kfrom] = index[kto];
index[rfrom] = tmp;

// Finally point our state pointer back to the previous state
st = st->previous;

assert(is_ok());
}


/// Position::do_null_move makes() a "null move": It switches the side to move
/// and updates the hash key without executing any move on the board.

void Position::do_null_move(StateInfo& backupSt) {

assert(is_ok());
assert(!is_check());

// Back up the information necessary to undo the null move to the supplied
// StateInfo object.
// Note that differently from normal case here backupSt is actually used as
// a backup storage not as a new state to be used.
backupSt.key      = st->key;
backupSt.epSquare = st->epSquare;
backupSt.value    = st->value;
backupSt.previous = st->previous;
backupSt.pliesFromNull = st->pliesFromNull;
st->previous = &backupSt;

// Save the current key to the history[] array, in order to be able to
// detect repetition draws.
history[st->gamePly++] = st->key;

// Update the necessary information
if (st->epSquare != SQ_NONE)
    st->key ^= zobEp[st->epSquare];

st->key ^= zobSideToMove;
prefetch((char*)TT.first_entry(st->key));

sideToMove = opposite_color(sideToMove);
st->epSquare = SQ_NONE;
st->rule50++;
st->pliesFromNull = 0;
st->value += (sideToMove == WHITE) ?  TempoValue : -TempoValue;
}


/// Position::undo_null_move() unmakes a "null move".

void Position::undo_null_move() {

assert(is_ok());
assert(!is_check());

// Restore information from the our backup StateInfo object
StateInfo* backupSt = st->previous;
st->key      = backupSt->key;
st->epSquare = backupSt->epSquare;
st->value    = backupSt->value;
st->previous = backupSt->previous;
st->pliesFromNull = backupSt->pliesFromNull;

// Update the necessary information
sideToMove = opposite_color(sideToMove);
st->rule50--;
st->gamePly--;
}


/// Position::see() is a static exchange evaluator: It tries to estimate the
/// material gain or loss resulting from a move. There are three versions of
/// this function: One which takes a destination square as input, one takes a
/// move, and one which takes a 'from' and a 'to' square. The function does
/// not yet understand promotions captures.

int Position::see(Move m) const {

assert(move_is_ok(m));
return see(move_from(m), move_to(m));
}

int Position::see_sign(Move m) const {

assert(move_is_ok(m));

Square from = move_from(m);
Square to = move_to(m);

// Early return if SEE cannot be negative because captured piece value
// is not less then capturing one. Note that king moves always return
// here because king midgame value is set to 0.
if (midgame_value_of_piece_on(to) >= midgame_value_of_piece_on(from))
    return 1;

return see(from, to);
}

int Position::see(Square from, Square to) const {

Bitboard occupied, attackers, stmAttackers, b;
int swapList[32], slIndex = 1;
PieceType capturedType, pt;
Color stm;

assert(square_is_ok(from));
assert(square_is_ok(to));

capturedType = type_of_piece_on(to);

// King cannot be recaptured
if (capturedType == KING)
    return seeValues[capturedType];

occupied = occupied_squares();

// Handle en passant moves
if (st->epSquare == to && type_of_piece_on(from) == PAWN)
{
    Square capQq = (side_to_move() == WHITE ? to - DELTA_N : to - DELTA_S);

    assert(capturedType == PIECE_TYPE_NONE);
    assert(type_of_piece_on(capQq) == PAWN);

    // Remove the captured pawn
    clear_bit(&occupied, capQq);
    capturedType = PAWN;
}

// Find all attackers to the destination square, with the moving piece
// removed, but possibly an X-ray attacker added behind it.
clear_bit(&occupied, from);
attackers =  (rook_attacks_bb(to, occupied)  & pieces(ROOK, QUEEN))
           | (bishop_attacks_bb(to, occupied)& pieces(BISHOP, QUEEN))
           | (attacks_from<KNIGHT>(to)       & pieces(KNIGHT))
           | (attacks_from<KING>(to)         & pieces(KING))
           | (attacks_from<PAWN>(to, WHITE)  & pieces(PAWN, BLACK))
           | (attacks_from<PAWN>(to, BLACK)  & pieces(PAWN, WHITE));

// If the opponent has no attackers we are finished
stm = opposite_color(color_of_piece_on(from));
stmAttackers = attackers & pieces_of_color(stm);
if (!stmAttackers)
    return seeValues[capturedType];

// The destination square is defended, which makes things rather more
// difficult to compute. We proceed by building up a "swap list" containing
// the material gain or loss at each stop in a sequence of captures to the
// destination square, where the sides alternately capture, and always
// capture with the least valuable piece. After each capture, we look for
// new X-ray attacks from behind the capturing piece.
swapList[0] = seeValues[capturedType];
capturedType = type_of_piece_on(from);

do {
    // Locate the least valuable attacker for the side to move. The loop
    // below looks like it is potentially infinite, but it isn't. We know
    // that the side to move still has at least one attacker left.
    for (pt = PAWN; !(stmAttackers & pieces(pt)); pt++)
        assert(pt < KING);

    // Remove the attacker we just found from the 'occupied' bitboard,
    // and scan for new X-ray attacks behind the attacker.
    b = stmAttackers & pieces(pt);
    occupied ^= (b & (~b + 1));
    attackers |=  (rook_attacks_bb(to, occupied)   & pieces(ROOK, QUEEN))
                | (bishop_attacks_bb(to, occupied) & pieces(BISHOP, QUEEN));

    attackers &= occupied; // Cut out pieces we've already done

    // Add the new entry to the swap list
    assert(slIndex < 32);
    swapList[slIndex] = -swapList[slIndex - 1] + seeValues[capturedType];
    slIndex++;

    // Remember the value of the capturing piece, and change the side to
    // move before beginning the next iteration.
    capturedType = pt;
    stm = opposite_color(stm);
    stmAttackers = attackers & pieces_of_color(stm);

    // Stop before processing a king capture
    if (capturedType == KING && stmAttackers)
    {
        assert(slIndex < 32);
        swapList[slIndex++] = QueenValueMidgame*10;
        break;
    }
} while (stmAttackers);

// Having built the swap list, we negamax through it to find the best
// achievable score from the point of view of the side to move.
while (--slIndex)
    swapList[slIndex-1] = Min(-swapList[slIndex], swapList[slIndex-1]);

return swapList[0];
}


/// Position::clear() erases the position object to a pristine state, with an
/// empty board, white to move, and no castling rights.

void Position::clear() {

st = &startState;
memset(st, 0, sizeof(StateInfo));
st->epSquare = SQ_NONE;
startPosPlyCounter = 0;
nodes = 0;

memset(byColorBB,  0, sizeof(Bitboard) * 2);
memset(byTypeBB,   0, sizeof(Bitboard) * 8);
memset(pieceCount, 0, sizeof(int) * 2 * 8);
memset(index,      0, sizeof(int) * 64);

for (int i = 0; i < 64; i++)
    board[i] = PIECE_NONE;

for (int i = 0; i < 8; i++)
    for (int j = 0; j < 16; j++)
        pieceList[0][i][j] = pieceList[1][i][j] = SQ_NONE;

for (Square sq = SQ_A1; sq <= SQ_H8; sq++)
    castleRightsMask[sq] = ALL_CASTLES;

sideToMove = WHITE;
initialKFile = FILE_E;
initialKRFile = FILE_H;
initialQRFile = FILE_A;
}


/// Position::reset_game_ply() simply sets gamePly to 0. It is used from the
/// UCI interface code, whenever a non-reversible move is made in a
/// 'position fen <fen> moves m1 m2 ...' command.  This makes it possible
/// for the program to handle games of arbitrary length, as long as the GUI
/// handles draws by the 50 move rule correctly.

void Position::reset_game_ply() {

st->gamePly = 0;
}

void Position::inc_startpos_ply_counter() {

startPosPlyCounter++;
}

/// Position::put_piece() puts a piece on the given square of the board,
/// updating the board array, pieces list, bitboards, and piece counts.

void Position::put_piece(Piece p, Square s) {

Color c = color_of_piece(p);
PieceType pt = type_of_piece(p);

board[s] = p;
index[s] = pieceCount[c][pt]++;
pieceList[c][pt][index[s]] = s;

set_bit(&(byTypeBB[pt]), s);
set_bit(&(byColorBB[c]), s);
set_bit(&(byTypeBB[0]), s); // HACK: byTypeBB[0] contains all occupied squares.
}


/// Position::compute_key() computes the hash key of the position. The hash
/// key is usually updated incrementally as moves are made and unmade, the
/// compute_key() function is only used when a new position is set up, and
/// to verify the correctness of the hash key when running in debug mode.

Key Position::compute_key() const {

Key result = zobCastle[st->castleRights];

for (Square s = SQ_A1; s <= SQ_H8; s++)
    if (square_is_occupied(s))
        result ^= zobrist[color_of_piece_on(s)][type_of_piece_on(s)][s];

if (ep_square() != SQ_NONE)
    result ^= zobEp[ep_square()];

if (side_to_move() == BLACK)
    result ^= zobSideToMove;

return result;
}


/// Position::compute_pawn_key() computes the hash key of the position. The
/// hash key is usually updated incrementally as moves are made and unmade,
/// the compute_pawn_key() function is only used when a new position is set
/// up, and to verify the correctness of the pawn hash key when running in
/// debug mode.

Key Position::compute_pawn_key() const {

Bitboard b;
Key result = 0;

for (Color c = WHITE; c <= BLACK; c++)
{
    b = pieces(PAWN, c);
    while (b)
        result ^= zobrist[c][PAWN][pop_1st_bit(&b)];
}
return result;
}


/// Position::compute_material_key() computes the hash key of the position.
/// The hash key is usually updated incrementally as moves are made and unmade,
/// the compute_material_key() function is only used when a new position is set
/// up, and to verify the correctness of the material hash key when running in
/// debug mode.

Key Position::compute_material_key() const {

int count;
Key result = 0;

for (Color c = WHITE; c <= BLACK; c++)
    for (PieceType pt = PAWN; pt <= QUEEN; pt++)
    {
        count = piece_count(c, pt);
        for (int i = 0; i < count; i++)
            result ^= zobrist[c][pt][i];
    }
return result;
}


/// Position::compute_value() compute the incremental scores for the middle
/// game and the endgame. These functions are used to initialize the incremental
/// scores when a new position is set up, and to verify that the scores are correctly
/// updated by do_move and undo_move when the program is running in debug mode.
Score Position::compute_value() const {

Bitboard b;
Score result = SCORE_ZERO;

for (Color c = WHITE; c <= BLACK; c++)
    for (PieceType pt = PAWN; pt <= KING; pt++)
    {
        b = pieces(pt, c);
        while (b)
            result += pst(c, pt, pop_1st_bit(&b));
    }

result += (side_to_move() == WHITE ? TempoValue / 2 : -TempoValue / 2);
return result;
}


/// Position::compute_non_pawn_material() computes the total non-pawn middle
/// game material value for the given side. Material values are updated
/// incrementally during the search, this function is only used while
/// initializing a new Position object.

Value Position::compute_non_pawn_material(Color c) const {

Value result = VALUE_ZERO;

for (PieceType pt = KNIGHT; pt <= QUEEN; pt++)
    result += piece_count(c, pt) * PieceValueMidgame[pt];

return result;
}


/// Position::is_draw() tests whether the position is drawn by material,
/// repetition, or the 50 moves rule. It does not detect stalemates, this
/// must be done by the search.

bool Position::is_draw() const {

// Draw by material?
if (   !pieces(PAWN)
    && (non_pawn_material(WHITE) + non_pawn_material(BLACK) <= BishopValueMidgame))
    return true;

// Draw by the 50 moves rule?
if (st->rule50 > 99 && (st->rule50 > 100 || !is_mate()))
    return true;

// Draw by repetition?
for (int i = 4, e = Min(Min(st->gamePly, st->rule50), st->pliesFromNull); i <= e; i += 2)
    if (history[st->gamePly - i] == st->key)
        return true;

return false;
}


/// Position::is_mate() returns true or false depending on whether the
/// side to move is checkmated.

bool Position::is_mate() const {

MoveStack moves[MOVES_MAX];
return is_check() && generate_moves(*this, moves) == moves;
}


/// Position::has_mate_threat() tests whether the side to move is under
/// a threat of being mated in one from the current position.

bool Position::has_mate_threat() {

MoveStack mlist[MOVES_MAX], *last, *cur;
StateInfo st1, st2;
bool mateFound = false;

// If we are under check it's up to evasions to do the job
if (is_check())
    return false;

// First pass the move to our opponent doing a null move
do_null_move(st1);

// Then generate pseudo-legal moves that could give check
last = generate_non_capture_checks(*this, mlist);
last = generate_captures(*this, last);

// Loop through the moves, and see if one of them gives mate
Bitboard pinned = pinned_pieces(sideToMove);
CheckInfo ci(*this);
for (cur = mlist; cur != last && !mateFound; cur++)
{
    Move move = cur->move;
    if (   !pl_move_is_legal(move, pinned)
        || !move_is_check(move, ci))
        continue;

    do_move(move, st2, ci, true);

    if (is_mate())
        mateFound = true;

    undo_move(move);
}

undo_null_move();
return mateFound;
}


/// Position::init_zobrist() is a static member function which initializes at
/// startup the various arrays used to compute hash keys.

void Position::init_zobrist() {

int i,j, k;
RKISS rk;

for (i = 0; i < 2; i++) for (j = 0; j < 8; j++) for (k = 0; k < 64; k++)
    zobrist[i][j][k] = rk.rand<Key>();

for (i = 0; i < 64; i++)
    zobEp[i] = rk.rand<Key>();

for (i = 0; i < 16; i++)
    zobCastle[i] = rk.rand<Key>();

zobSideToMove = rk.rand<Key>();
zobExclusion  = rk.rand<Key>();
}


/// Position::init_piece_square_tables() initializes the piece square tables.
/// This is a two-step operation: First, the white halves of the tables are
/// copied from the MgPST[][] and EgPST[][] arrays. Second, the black halves
/// of the tables are initialized by mirroring and changing the sign of the
/// corresponding white scores.

void Position::init_piece_square_tables() {

for (Square s = SQ_A1; s <= SQ_H8; s++)
    for (Piece p = WP; p <= WK; p++)
        PieceSquareTable[p][s] = make_score(MgPST[p][s], EgPST[p][s]);

for (Square s = SQ_A1; s <= SQ_H8; s++)
    for (Piece p = BP; p <= BK; p++)
        PieceSquareTable[p][s] = -PieceSquareTable[p-8][flip_square(s)];
}


/// Position::flipped_copy() makes a copy of the input position, but with
/// the white and black sides reversed. This is only useful for debugging,
/// especially for finding evaluation symmetry bugs.

void Position::flipped_copy(const Position& pos) {

assert(pos.is_ok());

clear();
threadID = pos.thread();

// Board
for (Square s = SQ_A1; s <= SQ_H8; s++)
    if (!pos.square_is_empty(s))
        put_piece(Piece(pos.piece_on(s) ^ 8), flip_square(s));

// Side to move
sideToMove = opposite_color(pos.side_to_move());

// Castling rights
if (pos.can_castle_kingside(WHITE))  do_allow_oo(BLACK);
if (pos.can_castle_queenside(WHITE)) do_allow_ooo(BLACK);
if (pos.can_castle_kingside(BLACK))  do_allow_oo(WHITE);
if (pos.can_castle_queenside(BLACK)) do_allow_ooo(WHITE);

initialKFile  = pos.initialKFile;
initialKRFile = pos.initialKRFile;
initialQRFile = pos.initialQRFile;

castleRightsMask[make_square(initialKFile,  RANK_1)] ^= (WHITE_OO | WHITE_OOO);
castleRightsMask[make_square(initialKFile,  RANK_8)] ^= (BLACK_OO | BLACK_OOO);
castleRightsMask[make_square(initialKRFile, RANK_1)] ^=  WHITE_OO;
castleRightsMask[make_square(initialKRFile, RANK_8)] ^=  BLACK_OO;
castleRightsMask[make_square(initialQRFile, RANK_1)] ^=  WHITE_OOO;
castleRightsMask[make_square(initialQRFile, RANK_8)] ^=  BLACK_OOO;

// En passant square
if (pos.st->epSquare != SQ_NONE)
    st->epSquare = flip_square(pos.st->epSquare);

// Checkers
find_checkers();

// Hash keys
st->key = compute_key();
st->pawnKey = compute_pawn_key();
st->materialKey = compute_material_key();

// Incremental scores
st->value = compute_value();

// Material
st->npMaterial[WHITE] = compute_non_pawn_material(WHITE);
st->npMaterial[BLACK] = compute_non_pawn_material(BLACK);

assert(is_ok());
}


/// Position::is_ok() performs some consitency checks for the position object.
/// This is meant to be helpful when debugging.

bool Position::is_ok(int* failedStep) const {

// What features of the position should be verified?
const bool debugAll = false;

const bool debugBitboards       = debugAll || false;
const bool debugKingCount       = debugAll || false;
const bool debugKingCapture     = debugAll || false;
const bool debugCheckerCount    = debugAll || false;
const bool debugKey             = debugAll || false;
const bool debugMaterialKey     = debugAll || false;
const bool debugPawnKey         = debugAll || false;
const bool debugIncrementalEval = debugAll || false;
const bool debugNonPawnMaterial = debugAll || false;
const bool debugPieceCounts     = debugAll || false;
const bool debugPieceList       = debugAll || false;
const bool debugCastleSquares   = debugAll || false;

if (failedStep) *failedStep = 1;

// Side to move OK?
if (!color_is_ok(side_to_move()))
    return false;

// Are the king squares in the position correct?
if (failedStep) (*failedStep)++;
if (piece_on(king_square(WHITE)) != WK)
    return false;

if (failedStep) (*failedStep)++;
if (piece_on(king_square(BLACK)) != BK)
    return false;

// Castle files OK?
if (failedStep) (*failedStep)++;
if (!file_is_ok(initialKRFile))
    return false;

if (!file_is_ok(initialQRFile))
    return false;

// Do both sides have exactly one king?
if (failedStep) (*failedStep)++;
if (debugKingCount)
{
    int kingCount[2] = {0, 0};
    for (Square s = SQ_A1; s <= SQ_H8; s++)
        if (type_of_piece_on(s) == KING)
            kingCount[color_of_piece_on(s)]++;

    if (kingCount[0] != 1 || kingCount[1] != 1)
        return false;
}

// Can the side to move capture the opponent's king?
if (failedStep) (*failedStep)++;
if (debugKingCapture)
{
    Color us = side_to_move();
    Color them = opposite_color(us);
    Square ksq = king_square(them);
    if (attackers_to(ksq) & pieces_of_color(us))
        return false;
}

// Is there more than 2 checkers?
if (failedStep) (*failedStep)++;
if (debugCheckerCount && count_1s<CNT32>(st->checkersBB) > 2)
    return false;

// Bitboards OK?
if (failedStep) (*failedStep)++;
if (debugBitboards)
{
    // The intersection of the white and black pieces must be empty
    if ((pieces_of_color(WHITE) & pieces_of_color(BLACK)) != EmptyBoardBB)
        return false;

    // The union of the white and black pieces must be equal to all
    // occupied squares
    if ((pieces_of_color(WHITE) | pieces_of_color(BLACK)) != occupied_squares())
        return false;

    // Separate piece type bitboards must have empty intersections
    for (PieceType p1 = PAWN; p1 <= KING; p1++)
        for (PieceType p2 = PAWN; p2 <= KING; p2++)
            if (p1 != p2 && (pieces(p1) & pieces(p2)))
                return false;
}

// En passant square OK?
if (failedStep) (*failedStep)++;
if (ep_square() != SQ_NONE)
{
    // The en passant square must be on rank 6, from the point of view of the
    // side to move.
    if (relative_rank(side_to_move(), ep_square()) != RANK_6)
        return false;
}

// Hash key OK?
if (failedStep) (*failedStep)++;
if (debugKey && st->key != compute_key())
    return false;

// Pawn hash key OK?
if (failedStep) (*failedStep)++;
if (debugPawnKey && st->pawnKey != compute_pawn_key())
    return false;

// Material hash key OK?
if (failedStep) (*failedStep)++;
if (debugMaterialKey && st->materialKey != compute_material_key())
    return false;

// Incremental eval OK?
if (failedStep) (*failedStep)++;
if (debugIncrementalEval && st->value != compute_value())
    return false;

// Non-pawn material OK?
if (failedStep) (*failedStep)++;
if (debugNonPawnMaterial)
{
    if (st->npMaterial[WHITE] != compute_non_pawn_material(WHITE))
        return false;

    if (st->npMaterial[BLACK] != compute_non_pawn_material(BLACK))
        return false;
}

// Piece counts OK?
if (failedStep) (*failedStep)++;
if (debugPieceCounts)
    for (Color c = WHITE; c <= BLACK; c++)
        for (PieceType pt = PAWN; pt <= KING; pt++)
            if (pieceCount[c][pt] != count_1s<CNT32>(pieces(pt, c)))
                return false;

if (failedStep) (*failedStep)++;
if (debugPieceList)
{
    for (Color c = WHITE; c <= BLACK; c++)
        for (PieceType pt = PAWN; pt <= KING; pt++)
            for (int i = 0; i < pieceCount[c][pt]; i++)
            {
                if (piece_on(piece_list(c, pt, i)) != piece_of_color_and_type(c, pt))
                    return false;

                if (index[piece_list(c, pt, i)] != i)
                    return false;
            }
}

if (failedStep) (*failedStep)++;
if (debugCastleSquares) {
    for (Color c = WHITE; c <= BLACK; c++) {
        if (can_castle_kingside(c) && piece_on(initial_kr_square(c)) != piece_of_color_and_type(c, ROOK))
            return false;
        if (can_castle_queenside(c) && piece_on(initial_qr_square(c)) != piece_of_color_and_type(c, ROOK))
            return false;
    }
    if (castleRightsMask[initial_kr_square(WHITE)] != (ALL_CASTLES ^ WHITE_OO))
        return false;
    if (castleRightsMask[initial_qr_square(WHITE)] != (ALL_CASTLES ^ WHITE_OOO))
        return false;
    if (castleRightsMask[initial_kr_square(BLACK)] != (ALL_CASTLES ^ BLACK_OO))
        return false;
    if (castleRightsMask[initial_qr_square(BLACK)] != (ALL_CASTLES ^ BLACK_OOO))
        return false;
}

if (failedStep) *failedStep = 0;
return true;
}"#;
