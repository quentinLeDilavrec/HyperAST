use epaint::text::LayoutSection;

use egui_addon::syntax_highlighting::TokenType;
use egui_addon::syntax_highlighting::simple::CodeTheme;

use hyperast::nodes::IndentedAlt;
use hyperast::nodes::Space;
use hyperast::types;

use types::{Childrn, HyperAST, HyperType, NodeId};

pub struct Layouter<'a, 'b, IdN, HAST, const SPC: bool = false> {
    pub(super) stores: &'a HAST,
    pub(super) root: IdN,
    pub(super) root_indent: &'static str,
    pub(super) theme: AdvTheme<&'b CodeTheme>,
}

#[derive(Copy, Clone)]
pub(crate) struct AdvTheme<Thm = CodeTheme> {
    pub theme: Thm,
    pub size: f32,
    pub fg: egui::Color32,
    pub bg: egui::Color32,
}

impl<Thm> AdvTheme<Thm> {
    pub fn fg(mut self, fg: egui::Color32) -> Self {
        self.fg = fg;
        self
    }
    pub fn bg(mut self, bg: egui::Color32) -> Self {
        self.bg = bg;
        self
    }
    pub fn size(mut self, size: f32) -> Self {
        self.size = size;
        self
    }
}
impl<Thm> From<Thm> for AdvTheme<Thm> {
    fn from(theme: Thm) -> Self {
        AdvTheme {
            theme,
            size: 12.0,
            fg: egui::Color32::WHITE,
            bg: egui::Color32::TRANSPARENT,
        }
    }
}
impl From<&egui::Ui> for AdvTheme<CodeTheme> {
    fn from(ui: &egui::Ui) -> Self {
        let theme = egui_addon::syntax_highlighting::simple::CodeTheme::from_memory(ui.ctx());
        Self::from(theme).fg(ui.style().visuals.text_color())
    }
}

impl<Thm> std::ops::Deref for AdvTheme<&Thm> {
    type Target = Thm;
    fn deref(&self) -> &Self::Target {
        &self.theme
    }
}

struct Frmt {
    format: TokenType,
    size: f32,
    fg: egui::Color32,
    bg: egui::Color32,
}

impl Frmt {
    fn with(self, theme: impl std::ops::Deref<Target = CodeTheme>) -> egui::TextFormat {
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
        let kind = self.stores.resolve_type(id);
        let label = b.try_get_label();
        let children = b.children();

        let last_line = |s: &str| s[s.rfind('\n').unwrap_or(0)..].to_owned();
        if kind.is_spaces() {
            let Some(label) = label else {
                return Ok(last_line(parent_indent));
            };
            let s = self.stores.label_store().resolve(label);
            let len = s.len();
            let s = Space::format_indentation(s.as_bytes());
            let b: String = s.iter().map(|x| x.to_string()).collect();
            // if !b.contains("\n") {
            //     return Ok(last_line(parent_indent));
            // };
            let end = *offset + len;
            let format = self.frmt(TokenType::Whitespace);
            out.push(LayoutSection {
                leading_space: 0.0,
                byte_range: *offset..end,
                format: format.with(self.theme),
            });
            *offset = end;
            return Ok(b);
        }

        if let (Some(label), Some(_)) = (&label, &children) {
            let s = self.stores.label_store().resolve(label);
            wasm_rs_dbg::dbg!(s);
        }
        match (label, children) {
            (None, None) => {
                let len = kind.to_string().len();
                let end = *offset + len;
                let format = self.frmt(TokenType::Keyword);
                out.push(LayoutSection {
                    leading_space: 0.0,
                    byte_range: *offset..end,
                    format: format.with(self.theme),
                });
                *offset = end;
            }
            (_, Some(children)) if !children.is_empty() => {
                let mut it = children.iter_children();
                let op = |alt| {
                    if alt == IndentedAlt::NoIndent {
                        Ok(last_line(parent_indent))
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
            (_, Some(_)) => {}
            (Some(label), None) => {
                let s = self.stores.label_store().resolve(label);
                let len = s.len();
                let end = *offset + len;
                let format = self.frmt(TokenType::Punctuation);
                out.push(LayoutSection {
                    leading_space: 0.0,
                    byte_range: *offset..end,
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
            size: self.theme.size,
            fg: self.theme.fg,
            bg: self.theme.bg,
        }
    }
}
