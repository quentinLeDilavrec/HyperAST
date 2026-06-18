use std::path::{Path, PathBuf};

use tree_sitter::Parser;

use hyperast::store::SimpleStores;

use crate::TStore;
use crate::legion::XmlTreeGen;
use crate::tree_sitter_parse;

#[test]
fn xml_tree_sitter_simple() {
    let mut parser = Parser::new();

    {
        parser.set_language(&crate::language()).unwrap();
    }

    let text = {
        let source_code1 = "<?xml version=\"1.0\"?><!-- q -->
        <project>
        <plugin>
        </plugin>
        <!-- This plugin's configuration is used to store Eclipse m2e settings only.
        It has no influence on the Maven build itself. -->
        <plugin>
        </plugin>
        </project>
          ";
        source_code1.as_bytes()
    };
    let tree = parser.parse(text, None).unwrap();
    println!("{}", tree.root_node().to_sexp());
}
#[test]
fn xml_tree_sitter_simple2() {
    let text = {
        let source_code1 = "<?xml version=\"1.0\"?><!-- q -->
<project>

    <require.bzip>false</require.bzip>
    <zstd.prefix></zstd.prefix>
    <zstd.lib></zstd.lib>
    <zstd.include></zstd.include>
    <require.zstd>false</require.zstd>
    <openssl.prefix></openssl.prefix>
    <openssl.lib></openssl.lib>
    <openssl.include></openssl.include>
    <require.isal>false</require.isal>
    <isal.prefix></isal.prefix>
    <isal.lib></isal.lib>
    <require.openssl>false</require.openssl>
    <runningWithNative>true</runningWithNative>
    <bundle.openssl.in.bin>false</bundle.openssl.in.bin>
    <extra.libhadoop.rpath></extra.libhadoop.rpath>
</project>
          ";
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{}", tree.root_node().to_sexp());
}

#[test]
fn xml_tree_sitter_on_pom() {
    let path: PathBuf = Path::new("src/tests/pom.xml.test").to_path_buf();

    let text = std::fs::read(path).unwrap();
    let tree = tree_sitter_parse(&text);
    println!("{:#?}", tree.root_node().to_sexp());
}

#[test]
fn hyperast_on_pom() {
    let path: PathBuf = Path::new("src/tests/pom.xml.test").to_path_buf();

    let text = std::fs::read(path).unwrap();
    let tree = tree_sitter_parse(&text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::<TStore>::default();
    let mut tree_gen = XmlTreeGen::new(&mut stores);
    let x = tree_gen.generate_file(b"", &text, tree.walk()).local;
    let id = x.compressed_node;
    use hyperast::nodes;
    println!("{}", nodes::SexpSerializer::new(&stores, id));
    println!("{}", nodes::SyntaxWithFieldsSerializer::new(&stores, id));
    println!("{}", nodes::TextSerializer::new(&stores, id));
    println!(
        "{}",
        nodes::SimpleSerializer::<_, _, true>::new(&stores, id)
    );
}

#[test]
fn xml_issue_cdata() {
    let text = {
        let source_code1 = r#"<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
<configuration>
    <bottom><![CDATA[<p align="center">Copyright &#169; {inceptionYear}-{currentYear} {organizationName}. All Rights Reserved.<br />
    .</p>]]></bottom>
</configuration>
</project>"#;
        source_code1.as_bytes()
    };
    let tree = tree_sitter_parse(text);
    println!("{:#?}", tree.root_node().to_sexp());
    let mut stores = SimpleStores::<TStore>::default();
    let mut tree_gen = XmlTreeGen {
        line_break: "\n".as_bytes().to_vec(),
        stores: &mut stores,
    };
    let _x = tree_gen.generate_file(b"", text, tree.walk()).local;
    // println!("{}", tree.root_node().to_sexp());
}

#[test]
fn type_test_generic_eq() {
    use hyperast::types::HyperType;

    let k = crate::Type::Document;
    let k0 = crate::Type::Document;
    let k1 = crate::Type::Element;
    assert!(k.eq(&k));
    assert!(k.eq(&k0));
    assert!(k0.eq(&k));
    assert!(k1.eq(&k1));
    assert!(k.ne(&k1));
    assert!(k1.ne(&k));

    assert!(k.generic_eq(&k));
    assert!(k.generic_eq(&k0));
    assert!(k0.generic_eq(&k));
    assert!(k1.generic_eq(&k1));
    assert!(!k.generic_eq(&k1));
    assert!(!k1.generic_eq(&k));

    let ak = crate::types::impls::as_any(&crate::Type::Document);
    let ak0 = crate::types::impls::as_any(&crate::Type::Document);
    let ak1 = crate::types::impls::as_any(&crate::Type::Element);

    assert!(ak.generic_eq(&ak));
    assert!(ak.generic_eq(&ak0));
    assert!(ak0.generic_eq(&ak));
    assert!(ak1.generic_eq(&ak1));
    assert!(!ak.generic_eq(&ak1));
    assert!(!ak1.generic_eq(&ak));

    assert!(k.generic_eq(&ak));
    assert!(k.generic_eq(&ak0));
    assert!(k0.generic_eq(&ak));
    assert!(k1.generic_eq(&ak1));
    assert!(!k.generic_eq(&ak1));
    assert!(!k1.generic_eq(&ak));

    assert!(ak.generic_eq(&k));
    assert!(ak.generic_eq(&k0));
    assert!(ak0.generic_eq(&k));
    assert!(ak1.generic_eq(&k1));
    assert!(!ak.generic_eq(&k1));
    assert!(!ak1.generic_eq(&k));

    assert!(ak.eq(&ak));
    assert!(ak.eq(&ak0));
    assert!(ak0.eq(&ak));
    assert!(ak1.eq(&ak1));
    assert!(!ak.eq(&ak1));
    assert!(!ak1.eq(&ak));
}
