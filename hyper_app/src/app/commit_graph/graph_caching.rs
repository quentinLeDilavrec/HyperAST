use lazy_static::lazy_static;

use super::update_results_per_commit;
use crate::app::ProjectId;
use crate::app::QueryId;
use crate::app::ResultsPerCommit;
use crate::app::commit::CommitsLayoutTimed;
use crate::app::commit::compute_commit_layout_timed;
use crate::app::querying::StreamedComputeResults;
use crate::app::types::CommitId;
use crate::app::utils_commit;

pub(crate) type GuardedCache<'a, T, U> =
    std::sync::MutexGuard<'a, utils_commit::BorrowFrameCache<T, U>>;

pub(crate) type Cache<T, U> =
    std::sync::Arc<std::sync::Mutex<utils_commit::BorrowFrameCache<T, U>>>;

lazy_static! {
    pub(super) static ref RES_PER_COMMIT: Cache<ResultsPerCommit, ComputeResPerCommit> =
        Default::default();
}

lazy_static! {
    pub(super) static ref LAYOUT: Cache<CommitsLayoutTimed, ComputeLayout> = Default::default();
}

#[derive(Default)]
pub struct ComputeResPerCommit;

pub(crate) type ResHash = u64;

impl
    egui::util::cache::ComputerMut<
        ((ProjectId, QueryId, ResHash), &StreamedComputeResults),
        ResultsPerCommit,
    > for ComputeResPerCommit
{
    fn compute(
        &mut self,
        (_, r): ((ProjectId, QueryId, ResHash), &StreamedComputeResults),
    ) -> ResultsPerCommit {
        let mut res = ResultsPerCommit::default();
        update_results_per_commit(&mut res, r);
        res
    }
}

#[derive(Default)]
pub struct ComputeLayout;

impl
    egui::util::cache::ComputerMut<
        ((&str, &CommitId, usize), &super::CommitMdStore),
        CommitsLayoutTimed,
    > for ComputeLayout
{
    fn compute(
        &mut self,
        ((name, target, _), fetched_commit_metadata): (
            (&str, &CommitId, usize),
            &super::CommitMdStore,
        ),
    ) -> CommitsLayoutTimed {
        compute_commit_layout_timed(
            |id| fetched_commit_metadata.get(id)?.as_ref().ok().cloned(),
            Some((name.to_string(), *target)).into_iter(),
        )
    }
}
