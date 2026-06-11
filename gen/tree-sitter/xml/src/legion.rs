//! fully compress all subtrees from an Xml CST
use std::{fmt::Debug, vec};

use hyperast::filter::BloomSize;
use hyperast::full::FullNode;
use hyperast::hashed;
use hyperast::hashed::SyntaxNodeHashs;
use hyperast::hashed::{IndexingHashBuilder, MetaDataHashsBuilder};
use hyperast::nodes::Space;
use hyperast::store::SimpleStores;
use hyperast::store::nodes::DefaultNodeStore as NodeStore;
use hyperast::store::nodes::compo;
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::store::nodes::legion::{eq_node, subtree_builder};
use hyperast::tree_gen;
use hyperast::tree_gen::Spaces;
use hyperast::tree_gen::parser::{Node, TreeCursor};
use hyperast::tree_gen::{AccIndentation, Accumulator, WithByteRange};
use hyperast::tree_gen::{BasicAccumulator, SubTreeMetrics};
use hyperast::tree_gen::{BasicGlobalData, GlobalData};
use hyperast::tree_gen::{Parents, PreResult};
use hyperast::tree_gen::{SpacedGlobalData, TextedGlobalData};
use hyperast::tree_gen::{TreeGen, ZippedTreeGen};
use hyperast::tree_gen::{compute_indentation, get_spacing, has_final_space};
use hyperast::types::LabelStore as LabelStoreTrait;

use crate::TNode;
use crate::types::{TStore, Type, XmlEnabledTypeStore};

pub type LabelIdentifier = hyperast::store::labels::DefaultLabelIdentifier;

pub struct XmlTreeGen<'stores, TS = TStore> {
    pub line_break: Vec<u8>,
    pub stores: &'stores mut SimpleStores<TS>,
}

pub type Global<'a> = SpacedGlobalData<'a>;

#[derive(Debug, Clone)]
pub struct Local {
    pub compressed_node: NodeIdentifier,
    pub metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
}

impl Local {
    fn acc(self, acc: &mut Acc) {
        if self.metrics.size_no_spaces > 0 {
            acc.no_space.push(self.compressed_node)
        }
        acc.simple.push(self.compressed_node);
        acc.metrics.acc(self.metrics);

        // TODO things with this.ana
    }
}

pub struct Acc {
    simple: BasicAccumulator<Type, NodeIdentifier>,
    no_space: Vec<NodeIdentifier>,
    labeled: bool,
    start_byte: usize,
    end_byte: usize,
    metrics: SubTreeMetrics<SyntaxNodeHashs<u32>>,
    padding_start: usize,
    indentation: Spaces,
}

pub type FNode = FullNode<BasicGlobalData, Local>;
impl Accumulator for Acc {
    type Node = FNode;
    fn push(&mut self, full_node: Self::Node) {
        full_node.local.acc(self);
    }
}

impl AccIndentation for Acc {
    fn indentation(&self) -> &Spaces {
        &self.indentation
    }
}

impl WithByteRange for Acc {
    fn has_children(&self) -> bool {
        !self.simple.children.is_empty()
    }

    fn begin_byte(&self) -> usize {
        self.start_byte
    }

    fn end_byte(&self) -> usize {
        self.end_byte
    }
}

impl Debug for Acc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Acc").field("simple", &self.simple).finish()
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct TTreeCursor<'a>(tree_sitter::TreeCursor<'a>);

impl Debug for TTreeCursor<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TTreeCursor")
            .field(&self.0.node().kind())
            .finish()
    }
}
impl<'a> hyperast::tree_gen::parser::TreeCursor for TTreeCursor<'a> {
    type N = TNode<'a>;
    fn node(&self) -> TNode<'a> {
        TNode(self.0.node())
    }

    fn role(&self) -> Option<std::num::NonZeroU16> {
        self.0.field_id()
    }

    fn goto_first_child(&mut self) -> bool {
        self.0.goto_first_child()
    }

    fn goto_parent(&mut self) -> bool {
        self.0.goto_parent()
    }

    fn goto_next_sibling(&mut self) -> bool {
        self.0.goto_next_sibling()
    }
}

impl<TS: XmlEnabledTypeStore> ZippedTreeGen for XmlTreeGen<'_, TS> {
    // type Node1 = SimpleNode1<NodeIdentifier, String>;
    type Stores = SimpleStores<TS>;
    type Text = [u8];
    type Node<'b> = TNode<'b>;
    type TreeCursor<'b> = TTreeCursor<'b>;

    fn stores(&mut self) -> &mut Self::Stores {
        self.stores
    }

    fn init_val(&mut self, text: &[u8], node: &Self::Node<'_>) -> Self::Acc {
        let kind = TS::obtain_type(node);
        let parent_indentation = Space::try_format_indentation(&self.line_break)
            .unwrap_or_else(|| vec![Space::Space; self.line_break.len()]);
        let indent = compute_indentation(
            &self.line_break,
            text,
            node.start_byte(),
            0,
            &parent_indentation,
        );
        let labeled = node.has_label();
        Acc {
            simple: BasicAccumulator {
                kind,
                children: vec![],
            },
            no_space: vec![],
            labeled,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: 0,
            indentation: indent,
        }
    }

    fn pre_skippable(
        &mut self,
        text: &Self::Text,
        cursor: &Self::TreeCursor<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> PreResult<<Self as TreeGen>::Acc> {
        let node = cursor.node();
        if node.0.is_missing() {
            return PreResult::Skip;
        }
        let Some(kind) = TS::try_obtain_type(&node) else {
            return PreResult::Skip;
        };
        let mut acc = self.pre(text, &node, stack, global);
        if kind == Type::AttValue {
            acc.labeled = true;
            return PreResult::SkipChildren(acc);
        }
        // log::warn!("not retrieving roles");
        PreResult::Ok(acc)
    }
    fn pre(
        &mut self,
        text: &[u8],
        node: &Self::Node<'_>,
        stack: &Parents<Self::Acc>,
        global: &mut Self::Global,
    ) -> <Self as TreeGen>::Acc {
        let parent_indentation = &stack.parent().unwrap().indentation();
        let kind = TS::obtain_type(node);
        let indent = compute_indentation(
            &self.line_break,
            text,
            node.start_byte(),
            global.sum_byte_length(),
            parent_indentation,
        );
        // if global.sum_byte_length() < 400 {
        //     dbg!((kind,node.start_byte(),node.end_byte(),global.sum_byte_length(),indent.len()));
        // }
        Acc {
            labeled: node.has_label(),
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            metrics: Default::default(),
            padding_start: global.sum_byte_length(),
            indentation: indent,
            simple: BasicAccumulator {
                kind,
                children: vec![],
            },
            no_space: vec![],
        }
    }

    fn post(
        &mut self,
        parent: &mut <Self as TreeGen>::Acc,
        global: &mut Self::Global,
        text: &[u8],
        acc: <Self as TreeGen>::Acc,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let spacing = get_spacing(
            acc.padding_start,
            acc.start_byte,
            text,
            parent.indentation(),
        );
        if let Some(spacing) = spacing {
            parent.push(FullNode {
                global: global.simple(),
                local: self.make_spacing(spacing),
            });
        }
        let label = if acc.labeled {
            std::str::from_utf8(&text[acc.start_byte..acc.end_byte])
                .ok()
                .map(|x| x.to_string())
        } else {
            None
        };
        self.make(global, acc, label)
    }
}

pub fn tree_sitter_parse_xml(text: &[u8]) -> Result<tree_sitter::Tree, tree_sitter::Tree> {
    hyperast::tree_gen::utils_ts::tree_sitter_parse(text, &crate::language())
}

impl<TS> XmlTreeGen<'_, TS> {
    pub fn tree_sitter_parse(text: &[u8]) -> Result<tree_sitter::Tree, tree_sitter::Tree> {
        let mut parser = tree_sitter::Parser::new();
        let language = crate::language();
        parser.set_language(&language).unwrap();
        let tree = parser.parse(text, None).unwrap();
        if tree.root_node().has_error() {
            Err(tree)
        } else {
            Ok(tree)
        }
    }
}
impl<'a, TS: XmlEnabledTypeStore> XmlTreeGen<'a, TS> {
    fn make_spacing(&mut self, spacing: Vec<u8>) -> Local {
        let kind = Type::Spaces;
        let interned_kind = TS::intern(kind);
        let bytes_len = spacing.len();
        let spacing = std::str::from_utf8(&spacing).unwrap().to_string();
        use num::ToPrimitive;
        let line_count = spacing
            .matches("\n")
            .count()
            .to_u32()
            .expect("too many newlines");
        let spacing_id = self.stores.label_store.get_or_insert(spacing.clone());
        let hbuilder: hashed::HashesBuilder<SyntaxNodeHashs<u32>> =
            hashed::HashesBuilder::new(Default::default(), &interned_kind, &spacing, 1);
        let hsyntax = hbuilder.most_discriminating();
        let hashable = &hsyntax;

        let eq = |x: hyperast::store::nodes::legion::EntryRef| {
            let t = x.get_component::<_>();
            if t != Ok(&interned_kind) {
                return false;
            }
            let l = x.get_component::<LabelIdentifier>();
            if l != Ok(&spacing_id) {
                return false;
            }
            true
        };

        let insertion = self.stores.node_store.prepare_insertion(&hashable, eq);

        let mut hashs = hbuilder.build();
        hashs.structt = 0;
        hashs.label = 0;

        let compressed_node = if let Some(id) = insertion.occupied_id() {
            id
        } else {
            let vacant = insertion.vacant();
            let mut dyn_builder = subtree_builder::<TS>(interned_kind);
            dyn_builder.add(compo::BytesLen(bytes_len.try_into().unwrap()));
            dyn_builder.add(spacing_id);
            dyn_builder.add(hashs);
            dyn_builder.add(BloomSize::None);
            if line_count != 0 {
                dyn_builder.add(compo::LineCount(line_count));
            }
            NodeStore::insert_built_after_prepare(vacant, dyn_builder.build())
        };
        Local {
            compressed_node,
            metrics: SubTreeMetrics {
                size: 1,
                height: 0,
                hashs,
                size_no_spaces: 0,
                line_count,
            },
        }
    }

    pub fn new(stores: &mut SimpleStores<TS>) -> XmlTreeGen<'_, TS> {
        XmlTreeGen {
            line_break: "\n".as_bytes().to_vec(),
            stores,
        }
    }

    pub fn generate_file(
        &mut self,
        name: &[u8],
        text: &'a [u8],
        cursor: tree_sitter::TreeCursor,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let mut global = Global::from(TextedGlobalData::new(Default::default(), text));
        let mut init = self.init_val(text, &TNode(cursor.node()));
        let mut xx = TTreeCursor(cursor);

        let spacing = get_spacing(
            init.padding_start,
            init.start_byte,
            text,
            init.indentation(),
        );
        if let Some(spacing) = spacing {
            global.down();
            init.start_byte = 0;
            init.push(FullNode {
                global: global.simple(),
                local: self.make_spacing(spacing),
            });
            global.right();
        }
        let mut stack = init.into();

        self.r#gen(text, &mut stack, &mut xx, &mut global);

        let mut acc = stack.finalize();

        if has_final_space(&0, global.sum_byte_length(), text) {
            let spacing = get_spacing(
                global.sum_byte_length(),
                text.len(),
                text,
                acc.indentation(),
            );
            if let Some(spacing) = spacing {
                global.right();
                acc.push(FullNode {
                    global: global.simple(),
                    local: self.make_spacing(spacing),
                });
            }
        }
        let label = Some(std::str::from_utf8(name).unwrap().to_owned());

        self.make(&mut global, acc, label)
    }
}

impl<'stores, TS: XmlEnabledTypeStore> TreeGen for XmlTreeGen<'stores, TS> {
    type Acc = Acc;
    type Global = SpacedGlobalData<'stores>;
    fn make(
        &mut self,
        global: &mut <Self as TreeGen>::Global,
        acc: <Self as TreeGen>::Acc,
        label: Option<String>,
    ) -> <<Self as TreeGen>::Acc as Accumulator>::Node {
        let stores = &mut self.stores;
        let kind = acc.simple.kind;
        let interned_kind = TS::intern(kind);
        let metrics = acc.metrics.finalize(&interned_kind, &label);

        let hashable = &metrics.hashs.most_discriminating();

        let label_id = label.as_ref().map(|label| {
            // Some notable type can contain very different labels,
            // they might benefit from a particular storing (like a blob storage, even using git's object database )
            // eg. acc.simple.kind == Type::Comment and acc.simple.kind.is_literal()
            stores.label_store.get_or_insert(label.as_str())
        });
        let eq = eq_node(&interned_kind, label_id.as_ref(), &acc.simple.children);

        let node_store = &mut stores.node_store;

        let insertion = node_store.prepare_insertion(hashable, eq);

        let local = if let Some(compressed_node) = insertion.occupied_id() {
            let metrics = metrics.map_hashs(|h| h.build());
            Local {
                compressed_node,
                metrics,
            }
        } else {
            let mut metrics = metrics.map_hashs(|h| h.build());
            let own_line_count = tree_gen::newline_count(&label);
            metrics.line_count += own_line_count;

            let byte_len = (acc.end_byte - acc.start_byte).try_into().unwrap();
            let bytes_len = compo::BytesLen(byte_len);
            let children_is_empty = acc.simple.children.is_empty();

            let vacant = insertion.vacant();

            let mut dyn_builder = subtree_builder::<TS>(interned_kind);
            dyn_builder.add(bytes_len);

            let hashs = metrics.add_md_metrics(&mut dyn_builder, children_is_empty);
            hashs.persist(&mut dyn_builder);

            if acc.simple.children.len() != acc.no_space.len() {
                let children = acc.no_space;
                tree_gen::add_cs_no_spaces(&mut dyn_builder, children);
            }

            acc.simple
                .add_primary(&mut dyn_builder, interned_kind, label_id);

            let compressed_node =
                NodeStore::insert_built_after_prepare(vacant, dyn_builder.build());

            Local {
                compressed_node,
                metrics,
            }
        };

        FullNode {
            global: global.simple(),
            local,
        }
    }
}
