use hyperast::nodes::{SyntaxSerializer, TextSerializer};
use hyperast::tree_gen::utils_ts;

use crate::legion::PythonTreeGen;

#[test]
fn medium_extra_bare() {
    let mut stores = Default::default();
    let mut r#gen = PythonTreeGen::bare(&mut stores);
    let text = ISSUE_QUOTE_DOC;
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());
    let name = b"";
    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;
    println!("{}", SyntaxSerializer::new(&stores, id));
    type SerWithRoles<'a, IdN, HAST> =
        hyperast::nodes::SimpleSerializer<'a, IdN, HAST, true, false, false, false, true>;
    println!("{}", SerWithRoles::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));
    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
    dbg!(&f.local.metrics);
}

/// It produces a weird node type, 1376 specifically.
/// In `Type::from_u16`, I assume it is an _ERROR type.
/// Maybe there is a better alternative.
#[test]
fn test_plot_py_error_type() {
    use hyperast::nodes::{SyntaxSerializer, TextSerializer};
    use hyperast::tree_gen::utils_ts;

    //     const EX: &str = r#"def f():
    //     return 42
    // "#;
    const EX: &str = r#"../plot.py"#;

    let name = b"";
    let text = EX;

    let mut stores = Default::default();
    let mut r#gen = PythonTreeGen::bare(&mut stores);
    let tree = utils_ts::tree_sitter_parse(text.as_bytes(), &crate::language());
    eprintln!("{}", tree.root_node().to_sexp());

    let f = r#gen.generate_file(name, text.as_bytes(), tree.walk());
    let id = f.local.compressed_node;

    println!("{}", SyntaxSerializer::new(&stores, id));
    type SerWithRoles<'a, IdN, HAST> =
        hyperast::nodes::SimpleSerializer<'a, IdN, HAST, true, false, false, false, true>;
    println!("{}", SerWithRoles::new(&stores, id));
    println!("\n{}", TextSerializer::new(&stores, id));

    assert_eq!(text, TextSerializer::new(&stores, id).to_string());
}

const ISSUE_QUOTE_DOC: &str = r#"
@click.group(chain=True)
def cli():
    """This script processes a bunch of images through pillow in a unix
    pipe.  One commands feeds into the next.

    Example:

    \b
        imagepipe open -i example01.jpg resize -w 128 display
        imagepipe open -i example02.jpg blur save
    """
    pass
"#;
