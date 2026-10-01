use crate::types::CommitId;

// TODO split by repo and query, maybe including config variations... but not by commit limit for example
#[derive(Default, Debug)]
pub(crate) struct ResultsPerCommit {
    // prop: cols.len() == floats.len() + ints.len()
    cols: Vec<String>,
    // comp_time and offsets into second level of floats/ints, first level of texts
    map: std::collections::HashMap<[u8; 8], (f32, u32)>,
    floats: Vec<Vec<f32>>,
    ints: Vec<Vec<i32>>,
    // does not include comp_time as it vary too much
    texts: Vec<std::sync::Arc<egui::Galley>>,
}

impl ResultsPerCommit {
    pub(crate) fn offset(&self, commit: &CommitId) -> Option<u32> {
        let mut c: [u8; 8] = [0; 8];
        c.copy_from_slice(&commit.0[..8]);
        Some(self.map.get(&c)?.1)
    }

    pub(crate) fn offset_with_variation(
        &self,
        commit: &CommitId,
        before: Option<&CommitId>,
        after: Option<&CommitId>,
    ) -> Option<u32> {
        let offset = self._get_offset(commit)?;
        match (before, after) {
            (Some(before), Some(after)) => {
                let before = self._get_offset(before);
                let after = self._get_offset(after);
                match (before, after) {
                    (Some(before), Some(after)) if offset == after && before == offset => None,
                    _ => Some(offset),
                }
            }
            _ => Some(offset),
        }
    }

    pub(crate) fn vals_to_string(&self, offset: u32) -> String {
        crate::utils::join(self.ints.iter().map(|v| v[offset as usize]), "\n").to_string()
    }

    pub(crate) fn try_diff_as_string(&self, c1: &CommitId, c2: &CommitId) -> Option<String> {
        let c1 = self._get_offset(c1)?;
        let c2 = self._get_offset(c2)?;
        if c1 == c2 {
            return None;
        }
        let vals = self.ints.iter().map(|v| v[c1 as usize] - v[c2 as usize]);
        let vals = vals.map(|v| format!("{:+}", v));
        let s = crate::utils::join(vals, "\n").to_string();
        Some(s)
    }

    pub(crate) fn _get_offset(&self, commit: &CommitId) -> Option<u32> {
        let mut c: [u8; 8] = [0; 8];
        c.copy_from_slice(&commit.0[..8]);
        Some(self.map.get(&c)?.1)
    }

    /// true if columns did not change
    pub(crate) fn set_cols(&mut self, h: &[String]) -> bool {
        if self.cols.is_empty() {
            self.ints = vec![vec![]; h.len()];
            self.cols = h.to_vec();
            // TODO init also for floats
            return false;
        }
        if self.cols != h {
            // for now reset all data, and replace cols
            log::warn!("{:?} {:?}", self.cols, h);
            self.ints = vec![vec![]; h.len()];
            self.texts = vec![];
            self.cols = h.to_vec();
        }
        true
    }

    pub(crate) fn insert(
        &mut self,
        commit: &CommitId,
        // galley: impl Fn() -> Arc<egui::Galley>,
        comp_time: f32,
        floats: &[f32],
        ints: &[i32],
    ) {
        let mut c: [u8; 8] = [0; 8];
        c.copy_from_slice(&commit.0[..8]);
        match self.map.entry(c) {
            std::collections::hash_map::Entry::Occupied(mut occ) => {
                let (t, v) = occ.get_mut();
                *t = comp_time;
                let i = *v as usize;
                let mut ident = true;
                for j in 0..ints.len() {
                    if !ident {
                        break;
                    }
                    ident &= self.ints[j][i] == ints[j];
                }
                for j in 0..floats.len() {
                    if !ident {
                        break;
                    }
                    // pretty dangerous but necessary due to prerender
                    // anyway should be deterministic
                    // and it depends on data, just do not put it in,
                    // then setting opt out of ser for unstable values could be useful
                    ident &= self.floats[j][i] == floats[j];
                }
                if !ident {
                    // TODO gc unused ints and floats
                    match Self::find_vals(&mut self.ints, &mut self.floats, ints, floats) {
                        Ok(i) => {
                            *v = i as u32;
                        }
                        Err(len) => {
                            *v = len as u32;
                            for j in 0..ints.len() {
                                self.ints[j].push(ints[j]);
                            }
                            for j in 0..floats.len() {
                                self.floats[j].push(floats[j]);
                            }
                        }
                    }
                }
            }
            std::collections::hash_map::Entry::Vacant(vac) => {
                match Self::find_vals(&mut self.ints, &mut self.floats, ints, floats) {
                    Ok(i) => {
                        vac.insert((comp_time, i as u32));
                    }
                    Err(i) => {
                        vac.insert((comp_time, i as u32));
                        for j in 0..ints.len() {
                            self.ints[j].push(ints[j]);
                        }
                        for j in 0..floats.len() {
                            self.floats[j].push(floats[j]);
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn find_vals(
        s_ints: &mut Vec<Vec<i32>>,
        s_floats: &mut Vec<Vec<f32>>,
        ints: &[i32],
        floats: &[f32],
    ) -> Result<usize, usize> {
        let len = s_ints[0].len();
        // TODO impl the complete logic
        for i in 0..len {
            let mut ident = true;
            for j in 0..ints.len() {
                if !ident {
                    break;
                }
                ident &= s_ints[j][i] == ints[j];
            }
            for j in 0..floats.len() {
                if !ident {
                    break;
                }
                // pretty dangerous but necessary due to prerendering of text.
                // anyway should be deterministic
                // and it depends on data, just do not put it in,
                // then setting opt out of ser for unstable values could be useful
                ident &= s_floats[j][i] == floats[j];
            }
            if ident {
                return Ok(i);
            }
        }
        Err(len)
    }
}
