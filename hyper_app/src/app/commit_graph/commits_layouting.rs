use std::collections::HashMap;

pub(super) struct Settings {
    branch_order: BranchOrder,
    branches: BranchSettings,
}

pub(super) struct Graph {}

pub(super) fn compute(
    mut commits: Vec<CommitInfo>,
    indices: HashMap<Oid, usize>,
    settings: &Settings,
    branches: Vec<(String, Oid)>,
) -> Result<Graph, String> {
    assign_children(&mut commits, &indices);

    let mut all_branches = assign_branches(branches, &mut commits, &indices, settings)?;
    // correct_fork_merges(&commits, &indices, &mut all_branches, settings)?;
    // assign_sources_targets(&commits, &indices, &mut all_branches);

    let (shortest_first, forward) = match settings.branch_order {
        BranchOrder::ShortestFirst(fwd) => (true, fwd),
        BranchOrder::LongestFirst(fwd) => (false, fwd),
    };

    assign_branch_columns(
        &commits,
        &indices,
        &mut all_branches,
        &settings.branches,
        shortest_first,
        forward,
    );
    Ok(Graph {})
}

/// Walks through the commits and adds each commit's Oid to the children of its parents.
fn assign_children(commits: &mut [CommitInfo], indices: &HashMap<Oid, usize>) {
    for idx in 0..commits.len() {
        let (oid, parents) = {
            let info = &commits[idx];
            (info.oid, info.parents)
        };
        for par_oid in &parents {
            if let Some(par_idx) = par_oid.and_then(|oid| indices.get(&oid)) {
                commits[*par_idx].children.push(oid);
            }
        }
    }
}

/// Extracts branches from repository and merge summaries, assigns branches and branch traces to commits.
///
/// Algorithm:
/// * Find all actual branches (incl. target oid) and all extract branches from merge summaries (incl. parent oid)
/// * Sort all branches by persistence
/// * Iterating over all branches in persistence order, trace back over commit parents until a trace is already assigned
fn assign_branches(
    branches: Vec<(String, Oid)>,
    commits: &mut [CommitInfo],
    indices: &HashMap<Oid, usize>,
    settings: &Settings,
) -> Result<Vec<BranchInfo>, String> {
    let mut branch_idx = 0;

    let mut branches = extract_branches(branches, commits, indices, settings)?;

    let mut index_map: Vec<_> = (0..branches.len())
        .map(|old_idx| {
            let (target, is_tag, is_merged) = {
                let branch = &branches[old_idx];
                (branch.target, branch.is_tag, branch.is_merged)
            };
            if let Some(&idx) = indices.get(&target) {
                let info = &mut commits[idx];
                if is_tag {
                    info.tags.push(old_idx);
                } else if !is_merged {
                    info.branches.push(old_idx);
                }
                let oid = info.oid;
                let any_assigned =
                    trace_branch(commits, indices, &mut branches, oid, old_idx).unwrap_or(false);

                if any_assigned || !is_merged {
                    branch_idx += 1;
                    Some(branch_idx - 1)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    let mut commit_count = vec![0; branches.len()];
    for info in commits.iter_mut() {
        if let Some(trace) = info.branch_trace {
            commit_count[trace] += 1;
        }
    }

    let mut count_skipped = 0;
    for (idx, branch) in branches.iter().enumerate() {
        if let Some(mapped) = index_map[idx] {
            if commit_count[idx] == 0 && branch.is_merged && !branch.is_tag {
                index_map[idx] = None;
                count_skipped += 1;
            } else {
                index_map[idx] = Some(mapped - count_skipped);
            }
        }
    }

    for info in commits.iter_mut() {
        if let Some(trace) = info.branch_trace {
            info.branch_trace = index_map[trace];
            for br in info.branches.iter_mut() {
                *br = index_map[*br].unwrap();
            }
            for tag in info.tags.iter_mut() {
                *tag = index_map[*tag].unwrap();
            }
        }
    }

    let branches: Vec<_> = branches
        .into_iter()
        .enumerate()
        .filter_map(|(arr_index, branch)| {
            if index_map[arr_index].is_some() {
                Some(branch)
            } else {
                None
            }
        })
        .collect();

    Ok(branches)
}

/// Traces back branches by following 1st commit parent,
/// until a commit is reached that already has a trace.
fn trace_branch(
    // repository: &Repository,
    commits: &mut [CommitInfo],
    indices: &HashMap<Oid, usize>,
    branches: &mut [BranchInfo],
    oid: Oid,
    branch_index: usize,
) -> Result<bool, String> {
    let mut curr_oid = oid;
    let mut prev_index: Option<usize> = None;
    let mut start_index: Option<i32> = None;
    let mut any_assigned = false;
    while let Some(index) = indices.get(&curr_oid) {
        let info = &mut commits[*index];
        if let Some(old_trace) = info.branch_trace {
            let (old_name, old_term, old_svg, old_range) = {
                let old_branch = &branches[old_trace];
                (
                    &old_branch.name.clone(),
                    old_branch.visual.term_color,
                    old_branch.visual.svg_color.clone(),
                    old_branch.range,
                )
            };
            let new_name = &branches[branch_index].name;
            let old_end = old_range.0.unwrap_or(0);
            let new_end = branches[branch_index].range.0.unwrap_or(0);
            if new_name == old_name && old_end >= new_end {
                let old_branch = &mut branches[old_trace];
                if let Some(old_end) = old_range.1 {
                    if index > &old_end {
                        old_branch.range = (None, None);
                    } else {
                        old_branch.range = (Some(*index), old_branch.range.1);
                    }
                } else {
                    old_branch.range = (Some(*index), old_branch.range.1);
                }
            } else {
                let branch = &mut branches[branch_index];
                // if branch.name.starts_with(ORIGIN) && branch.name[7..] == old_name[..] {
                //     branch.visual.term_color = old_term;
                //     branch.visual.svg_color = old_svg;
                // }
                match prev_index {
                    None => start_index = Some(*index as i32 - 1),
                    Some(prev_index) => {
                        // TODO: in cases where no crossings occur, the rule for merge commits can also be applied to normal commits
                        // see also print::get_deviate_index()
                        if commits[prev_index].is_merge {
                            let mut temp_index = prev_index;
                            for sibling_oid in &commits[*index].children {
                                if sibling_oid != &curr_oid {
                                    let sibling_index = indices[sibling_oid];
                                    if sibling_index > temp_index {
                                        temp_index = sibling_index;
                                    }
                                }
                            }
                            start_index = Some(temp_index as i32);
                        } else {
                            start_index = Some(*index as i32 - 1);
                        }
                    }
                }
                break;
            }
        }

        info.branch_trace = Some(branch_index);
        any_assigned = true;

        let commit = &commits[indices[&curr_oid]]; //.find_commit(curr_oid)?;
        match commit.parents.len() {
            0 => {
                start_index = Some(*index as i32);
                break;
            }
            _ => {
                prev_index = Some(*index);
                curr_oid = commit.parents[0].unwrap() //parent_id(0)?;
            }
        }
    }

    let branch = &mut branches[branch_index];
    if let Some(end) = branch.range.0 {
        if let Some(start_index) = start_index {
            if start_index < end as i32 {
                // TODO: find a better solution (bool field?) to identify non-deleted branches that were not assigned to any commits, and thus should not occupy a column.
                branch.range = (None, None);
            } else {
                branch.range = (branch.range.0, Some(start_index as usize));
            }
        } else {
            branch.range = (branch.range.0, None);
        }
    } else {
        branch.range = (branch.range.0, start_index.map(|si| si as usize));
    }
    Ok(any_assigned)
}

/// Extracts (real or derived from merge summary) and assigns basic properties.
fn extract_branches(
    branches: Vec<(String, Oid)>,
    commits: &[CommitInfo],
    indices: &HashMap<Oid, usize>,
    settings: &Settings,
) -> Result<Vec<BranchInfo>, String> {
    // let filter = if settings.include_remote {
    //     None
    // } else {
    //     Some(BranchType::Local)
    // };
    // let actual_branches = repository
    //     .branches(filter)
    //     .map_err(|err| err.message().to_string())?
    //     .collect::<Result<Vec<_>, Error>>()
    //     .map_err(|err| err.message().to_string())?;

    let actual_branches = branches;

    // enum BranchType {
    //     Local, Remote
    // }

    let mut counter = 0;

    let mut valid_branches = actual_branches
        // .iter()
        .into_iter()
        // .filter_map(|(br, tp)| {
        .filter_map(|(name, target)| {
            // let Some(name) = br.get().name() else {return None};
            // name.and_then(|n| {
            let n = name;
            // let target = br.get().target();
            Some(target).map(|t| {
                counter += 1;
                let start_index = 11;
                // let start_index = match tp {
                //     BranchType::Local => 11,
                //     BranchType::Remote => 13,
                // };
                let name = &n[start_index..];
                let end_index = indices.get(&t).cloned();

                // let term_color = match to_terminal_color(
                //     &branch_color(
                //         name,
                //         &settings.branches.terminal_colors[..],
                //         &settings.branches.terminal_colors_unknown,
                //         counter,
                //     )[..],
                // ) {
                //     Ok(col) => col,
                //     Err(err) => return Err(err),
                // };

                Ok(BranchInfo::new(
                    t,
                    None,
                    name.to_string(),
                    // branch_order(name, &settings.branches.persistence) as u8,
                    0,
                    false,
                    // &BranchType::Remote == tp,
                    false,
                    false,
                    BranchVis::new(
                        0,
                        0,
                        "aabbcc".to_string(),
                        // branch_order(name, &settings.branches.order),
                        // term_color,
                        // branch_color(
                        //     name,
                        //     &settings.branches.svg_colors,
                        //     &settings.branches.svg_colors_unknown,
                        //     counter,
                        // ),
                    ),
                    end_index,
                ))
            })
        })
        .collect::<Result<Vec<_>, String>>()?;

    // for (idx, info) in commits.iter().enumerate() {
    //     let commit = repository
    //         .find_commit(info.oid)
    //         .map_err(|err| err.message().to_string())?;
    //     if info.is_merge {
    //         if let Some(summary) = commit.summary() {
    //             counter += 1;

    //             let parent_oid = commit
    //                 .parent_id(1)
    //                 .map_err(|err| err.message().to_string())?;

    //             let branch_name = parse_merge_summary(summary, &settings.merge_patterns)
    //                 .unwrap_or_else(|| "unknown".to_string());

    //             let persistence =
    //                 branch_order(&branch_name, &settings.branches.persistence) as u8;

    //             let pos = branch_order(&branch_name, &settings.branches.order);

    //             let term_col = to_terminal_color(
    //                 &branch_color(
    //                     &branch_name,
    //                     &settings.branches.terminal_colors[..],
    //                     &settings.branches.terminal_colors_unknown,
    //                     counter,
    //                 )[..],
    //             )?;
    //             // let svg_col = branch_color(
    //             //     &branch_name,
    //             //     &settings.branches.svg_colors,
    //             //     &settings.branches.svg_colors_unknown,
    //             //     counter,
    //             // );

    //             let branch_info = BranchInfo::new(
    //                 parent_oid,
    //                 Some(info.oid),
    //                 branch_name,
    //                 persistence,
    //                 false,
    //                 true,
    //                 false,
    //                 BranchVis::new(pos, term_col, svg_col),
    //                 Some(idx + 1),
    //             );
    //             valid_branches.push(branch_info);
    //         }
    //     }
    // }

    // valid_branches.sort_by_cached_key(|branch| (branch.persistence, !branch.is_merged));

    // let mut tags = Vec::new();

    // repository
    //     .tag_foreach(|oid, name| {
    //         tags.push((oid, name.to_vec()));
    //         true
    //     })
    //     .map_err(|err| err.message().to_string())?;

    // for (oid, name) in tags {
    //     let name = std::str::from_utf8(&name[5..]).map_err(|err| err.to_string())?;

    //     let target = repository
    //         .find_tag(oid)
    //         .map(|tag| tag.target_id())
    //         .or_else(|_| repository.find_commit(oid).map(|_| oid));

    //     if let Ok(target_oid) = target {
    //         if let Some(target_index) = indices.get(&target_oid) {
    //             counter += 1;
    //             let term_col = to_terminal_color(
    //                 &branch_color(
    //                     name,
    //                     &settings.branches.terminal_colors[..],
    //                     &settings.branches.terminal_colors_unknown,
    //                     counter,
    //                 )[..],
    //             )?;
    //             let pos = branch_order(name, &settings.branches.order);
    //             let svg_col = branch_color(
    //                 name,
    //                 &settings.branches.svg_colors,
    //                 &settings.branches.svg_colors_unknown,
    //                 counter,
    //             );
    //             let tag_info = BranchInfo::new(
    //                 target_oid,
    //                 None,
    //                 name.to_string(),
    //                 settings.branches.persistence.len() as u8 + 1,
    //                 false,
    //                 false,
    //                 true,
    //                 BranchVis::new(pos, term_col, svg_col),
    //                 Some(*target_index),
    //             );
    //             valid_branches.push(tag_info);
    //         }
    //     }
    // }

    Ok(valid_branches)
}

/// Sorts branches into columns for visualization, that all branches can be
/// visualized linearly and without overlaps. Uses Shortest-First scheduling.
///
/// https://github.com/mlange-42/git-graph/blob/7b9bb72a310243cc53d906d1e7ec3c9aad1c75d2/src/graph.rs#L791
pub(super) fn assign_branch_columns(
    commits: &[CommitInfo],
    indices: &HashMap<Oid, usize>,
    branches: &mut [BranchInfo],
    settings: &BranchSettings,
    shortest_first: bool,
    forward: bool,
) {
    let mut occupied: Vec<Vec<Vec<(usize, usize)>>> = vec![vec![]; settings.order.len() + 1];

    let length_sort_factor = if shortest_first { 1 } else { -1 };
    let start_sort_factor = if forward { 1 } else { -1 };

    let mut branches_sort: Vec<_> = branches
        .iter()
        .enumerate()
        .filter(|(_idx, br)| br.range.0.is_some() || br.range.1.is_some())
        .map(|(idx, br)| {
            (
                idx,
                br.range.0.unwrap_or(0),
                br.range.1.unwrap_or(branches.len() - 1),
                br.visual
                    .source_order_group
                    .unwrap_or(settings.order.len() + 1),
                br.visual
                    .target_order_group
                    .unwrap_or(settings.order.len() + 1),
            )
        })
        .collect();

    branches_sort.sort_by_cached_key(|tup| {
        (
            std::cmp::max(tup.3, tup.4),
            (tup.2 as i32 - tup.1 as i32) * length_sort_factor,
            tup.1 as i32 * start_sort_factor,
        )
    });

    for (branch_idx, start, end, _, _) in branches_sort {
        let branch = &branches[branch_idx];
        let group = branch.visual.order_group;
        let group_occ = &mut occupied[group];

        let align_right = branch
            .source_branch
            .map(|src| branches[src].visual.order_group > branch.visual.order_group)
            .unwrap_or(false)
            || branch
                .target_branch
                .map(|trg| branches[trg].visual.order_group > branch.visual.order_group)
                .unwrap_or(false);

        let len = group_occ.len();
        let mut found = len;
        for i in 0..len {
            let index = if align_right { len - i - 1 } else { i };
            let column_occ = &group_occ[index];
            let mut occ = false;
            for (s, e) in column_occ {
                if start <= *e && end >= *s {
                    occ = true;
                    break;
                }
            }
            if !occ {
                if let Some(merge_trace) = branch
                    .merge_target
                    .and_then(|t| indices.get(&t))
                    .and_then(|t_idx| commits[*t_idx].branch_trace)
                {
                    let merge_branch = &branches[merge_trace];
                    if merge_branch.visual.order_group == branch.visual.order_group {
                        if let Some(merge_column) = merge_branch.visual.column {
                            if merge_column == index {
                                occ = true;
                            }
                        }
                    }
                }
            }
            if !occ {
                found = index;
                break;
            }
        }

        let branch = &mut branches[branch_idx];
        branch.visual.column = Some(found);
        if found == group_occ.len() {
            group_occ.push(vec![]);
        }
        group_occ[found].push((start, end));
    }

    let group_offset: Vec<usize> = occupied
        .iter()
        .scan(0, |acc, group| {
            *acc += group.len();
            Some(*acc)
        })
        .collect();

    for branch in branches {
        if let Some(column) = branch.visual.column {
            let offset = if branch.visual.order_group == 0 {
                0
            } else {
                group_offset[branch.visual.order_group - 1]
            };
            branch.visual.column = Some(column + offset);
        }
    }
}
pub struct BranchSettings {
    order: Vec<BranchOrder>,
}
impl BranchSettings {
    pub(crate) fn new(len: usize) -> Self {
        Self {
            order: (0..len).map(|_| BranchOrder::ShortestFirst(true)).collect(),
        }
    }
}
/// Ordering policy for branches in visual columns.
pub enum BranchOrder {
    /// Recommended! Shortest branches are inserted left-most.
    ///
    /// For branches with equal length, branches ending last are inserted first.
    /// Reverse (arg = false): Branches ending first are inserted first.
    ShortestFirst(bool),
    /// Longest branches are inserted left-most.
    ///
    /// For branches with equal length, branches ending last are inserted first.
    /// Reverse (arg = false): Branches ending first are inserted first.
    LongestFirst(bool),
}

pub type Oid = [u8; 20];
/// Represents a commit.
pub struct CommitInfo {
    pub oid: Oid,
    pub is_merge: bool,
    pub parents: [Option<Oid>; 2],
    pub children: Vec<Oid>,
    pub branches: Vec<usize>,
    pub tags: Vec<usize>,
    pub branch_trace: Option<usize>,
}
pub struct Commit {
    pub oid: Oid,
    pub parents: Vec<Oid>,
}
impl Commit {
    fn id(&self) -> Oid {
        self.oid
    }

    fn parent_count(&self) -> usize {
        self.parents.len()
    }

    fn parent_id(&self, i: usize) -> Result<Oid, ()> {
        Ok(self.parents[i])
    }
}

impl CommitInfo {
    fn new(commit: &Commit) -> Self {
        CommitInfo {
            oid: commit.id(),
            is_merge: commit.parent_count() > 1,
            parents: [commit.parent_id(0).ok(), commit.parent_id(1).ok()],
            children: Vec::new(),
            branches: Vec::new(),
            tags: Vec::new(),
            branch_trace: None,
        }
    }
}

/// Represents a branch (real or derived from merge summary).
pub struct BranchInfo {
    pub target: Oid,
    pub merge_target: Option<Oid>,
    pub source_branch: Option<usize>,
    pub target_branch: Option<usize>,
    pub name: String,
    pub persistence: u8,
    pub is_remote: bool,
    pub is_merged: bool,
    pub is_tag: bool,
    pub visual: BranchVis,
    pub range: (Option<usize>, Option<usize>),
}
impl BranchInfo {
    #[allow(clippy::too_many_arguments)]
    fn new(
        target: Oid,
        merge_target: Option<Oid>,
        name: String,
        persistence: u8,
        is_remote: bool,
        is_merged: bool,
        is_tag: bool,
        visual: BranchVis,
        end_index: Option<usize>,
    ) -> Self {
        BranchInfo {
            target,
            merge_target,
            target_branch: None,
            source_branch: None,
            name,
            persistence,
            is_remote,
            is_merged,
            is_tag,
            visual,
            range: (end_index, None),
        }
    }
}

/// Branch properties for visualization.
pub struct BranchVis {
    /// The branch's column group (left to right)
    pub order_group: usize,
    /// The branch's merge target column group (left to right)
    pub target_order_group: Option<usize>,
    /// The branch's source branch column group (left to right)
    pub source_order_group: Option<usize>,
    /// The branch's terminal color (index in 256-color palette)
    pub term_color: u8,
    /// SVG color (name or RGB in hex annotation)
    pub svg_color: String,
    /// The column the branch is located in
    pub column: Option<usize>,
}

impl BranchVis {
    fn new(order_group: usize, term_color: u8, svg_color: String) -> Self {
        BranchVis {
            order_group,
            target_order_group: None,
            source_order_group: None,
            term_color,
            svg_color,
            column: None,
        }
    }
}
