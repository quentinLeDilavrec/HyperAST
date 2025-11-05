use epaint::text::LayoutSection;

use egui_addon::syntax_highlighting::TokenType;
use egui_addon::syntax_highlighting::simple::CodeTheme;
use hyperast::nodes::IndentedAlt;
use hyperast::nodes::Space;
use hyperast::types;

use types::{Childrn, HyperAST, HyperType, NodeId};

pub struct Layouter<'a, 'b, IdN, HAST, const SPC: bool = false> {
    stores: &'a HAST,
    root: IdN,
    root_indent: &'static str,
    theme: &'b CodeTheme,
    size: f32,
    fg: egui::Color32,
    bg: egui::Color32,
}

impl<'store, 'b, IdN, HAST, const SPC: bool> Layouter<'store, 'b, IdN, HAST, SPC> {
    pub fn new(
        stores: &'store HAST,
        root: IdN,
        theme: &'b CodeTheme,
        size: f32,
        fg: egui::Color32,
        bg: egui::Color32,
    ) -> Self {
        Self {
            stores,
            root,
            root_indent: "\n",
            theme,
            size,
            fg,
            bg,
        }
    }
}

struct Frmt {
    format: TokenType,
    size: f32,
    fg: egui::Color32,
    bg: egui::Color32,
}

impl Frmt {
    fn with(self, theme: &CodeTheme) -> egui::TextFormat {
        let mut format = theme.formats[self.format].clone();
        format.font_id = egui::FontId::monospace(self.size);
        format.background = self.bg;
        format.color = format.color.lerp_to_gamma(self.fg, 0.3);
        format
    }
}

impl<'store, 'b, IdN, HAST, const SPC: bool> Layouter<'store, 'b, IdN, HAST, SPC>
where
    HAST: HyperAST<IdN = IdN>,
    IdN: NodeId<IdN = IdN>,
{
    pub fn compute(&self) -> Result<(usize, Vec<LayoutSection>), IndentedAlt> {
        let mut layout = vec![];
        let mut offset = 0;
        match self._compute(&self.root, self.root_indent, &mut layout, &mut offset) {
            Err(IndentedAlt::FmtError) => Err(IndentedAlt::FmtError),
            _ => Ok((offset, layout)),
        }
    }
    fn _compute(
        &self,
        id: &HAST::IdN,
        parent_indent: &str,
        out: &mut Vec<LayoutSection>,
        offset: &mut usize,
    ) -> Result<String, IndentedAlt> {
        use types::LabelStore;
        use types::Labeled;
        use types::NodeStore;
        use types::WithChildren;
        let b = self.stores.node_store().resolve(id);
        // let kind = (self.stores.type_store(), b);
        let kind = self.stores.resolve_type(id);
        let label = b.try_get_label();
        let children = b.children();

        if kind.is_spaces() {
            let indent = if let Some(label) = label {
                let s = self.stores.label_store().resolve(label);
                let len = s.len();
                let s = Space::format_indentation(s.as_bytes());
                let b: String = s.iter().map(|x| x.to_string()).collect();
                let end = *offset + len;
                let format = self.frmt(TokenType::Punctuation);
                out.push(LayoutSection {
                    leading_space: 0.0,
                    byte_range: *offset..end.clone(),
                    format: format.with(self.theme),
                });
                *offset = end;
                if b.contains("\n") {
                    b
                } else {
                    parent_indent[parent_indent.rfind('\n').unwrap_or(0)..].to_owned()
                }
            } else {
                parent_indent[parent_indent.rfind('\n').unwrap_or(0)..].to_owned()
            };
            return Ok(indent);
        }

        match (label, children) {
            (None, None) => {
                // out.write_str(&kind.to_string()).unwrap();
                let len = kind.to_string().len();
                let end = *offset + len;
                let format = self.frmt(TokenType::Keyword);
                out.push(LayoutSection {
                    leading_space: 0.0,
                    byte_range: *offset..end.clone(),
                    format: format.with(self.theme),
                });
                *offset = end;
            }
            (label, Some(children)) => {
                if let Some(label) = label {
                    let s = self.stores.label_store().resolve(label);
                    dbg!(s);
                }
                if !children.is_empty() {
                    let mut it = children.iter_children();
                    let op = |alt| {
                        if alt == IndentedAlt::NoIndent {
                            Ok(parent_indent[parent_indent.rfind('\n').unwrap_or(0)..].to_owned())
                        } else {
                            Err(alt)
                        }
                    };
                    let mut ind = self
                        ._compute(&it.next().unwrap(), parent_indent, out, offset)
                        .or_else(op)?;
                    for id in it {
                        ind = self._compute(&id, &ind, out, offset).or_else(op)?;
                    }
                }
            }
            (Some(label), None) => {
                let s = self.stores.label_store().resolve(label);
                let len = s.len();
                let end = *offset + len;
                let format = self.frmt(TokenType::Punctuation);
                out.push(LayoutSection {
                    leading_space: 0.0,
                    byte_range: *offset..end.clone(),
                    format: format.with(self.theme),
                });
                *offset = end;
            }
        };
        Err(IndentedAlt::NoIndent)
    }

    fn frmt(&self, format: TokenType) -> Frmt {
        Frmt {
            format,
            size: self.size,
            fg: self.fg,
            bg: self.bg,
        }
    }
}
