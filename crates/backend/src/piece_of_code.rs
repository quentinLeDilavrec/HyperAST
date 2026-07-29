use hyperast::position::position_accessors;
use hyperast::store::SimpleStores;
use hyperast::types::PrimInt;
use hyperast::types::TypeStore;
use hyperast_vcs_git::git::Oid;
use hyperast_vcs_git::git::Repo;

use crate::IdN;
use crate::Idx;

pub type Position = hyperast::position::StructuralPosition<IdN, Idx>;

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct PieceOfCode<IdN = crate::IdN, Idx = usize> {
    pub user: String,
    pub name: String,
    #[serde(deserialize_with = "crate::utils::string_to_oid")]
    #[serde(serialize_with = "crate::utils::oid_to_string")]
    pub commit: Oid,
    #[serde(default = "Vec::new")]
    pub path: Vec<Idx>,
    #[serde(default)]
    pub file: String,
    #[serde(default)]
    pub start: usize,
    #[serde(default)]
    pub end: usize,
    #[serde(bound(serialize = "IdN: Clone + Into<crate::IdN>"))]
    #[serde(serialize_with = "custom_ser")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default = "Vec::new")]
    pub path_ids: Vec<IdN>, // WARN this is not necessarily fetched::NodeIdentifier
}

#[derive(Clone, PartialEq, Debug)]
pub struct LocalPieceOfCode<IdN, Idx> {
    pub file: String,
    pub start: usize,
    pub end: usize,
    pub path: Vec<Idx>,
    pub path_ids: Vec<IdN>,
}

impl<Idx> LocalPieceOfCode<IdN, Idx> {
    pub(crate) fn from_root_and_offsets<TS: TypeStore>(
        stores: &SimpleStores<TS>,
        root: IdN,
        path: Vec<impl PrimInt>,
    ) -> Self
    where
        Idx: PrimInt,
    {
        use hyperast::position::compute_position_and_nodes;
        let (pos, path_ids) = compute_position_and_nodes(root, &mut path.iter().copied(), stores);
        let _rooted_offsets =
            hyperast::position::Offsets::from_iterator(path.iter().copied().map(|x| x.cast()))
                .with_root(root);
        let offsets = path.into_iter().map(|x| x.cast()).collect();
        let _compound = _rooted_offsets
            .with_store(stores)
            .compute_pos_pre_order::<_, hyperast::position::CompoundPositionPreparer<
            hyperast::position::Position,
            hyperast::position::offsets_and_nodes::StructuralPosition<_, _>,
        >>();

        let _pos = _compound.0;
        assert_eq!(_pos, pos);
        let _path_ids = _compound.1;
        assert_eq!(
            _path_ids.iter_nodes().skip(1).rev().collect::<Vec<_>>(),
            path_ids
        );

        if cfg!(debug_assertion) {
            let from_offsets_and_nodes = pos
                .clone()
                .with_root(root)
                .with_store(stores)
                .compute_pos_file_and_offset::<_, hyperast::position::CompoundPositionPreparer<
                hyperast::position::Position,
                hyperast::position::offsets_and_nodes::StructuralPosition<_, _>,
            >>();
            assert_eq!(pos, from_offsets_and_nodes.0);
            assert_eq!(
                path_ids,
                (from_offsets_and_nodes.1.iter_nodes())
                    .skip(1)
                    .rev()
                    .collect::<Vec<_>>()
            );
        }

        Self::from_position(&pos, offsets, path_ids)
    }
}

impl<IdN, Idx> LocalPieceOfCode<IdN, Idx> {
    pub(crate) fn from_position(
        pos: &hyperast::position::Position,
        path: Vec<Idx>,
        path_ids: Vec<IdN>,
    ) -> Self {
        let range = pos.range();
        let file = pos.file();
        Self::from_file_and_range(file, range, path, path_ids)
    }
    pub(crate) fn from_file_and_range(
        file: &std::path::Path,
        range: std::ops::Range<usize>,
        path: Vec<Idx>,
        path_ids: Vec<IdN>,
    ) -> Self {
        let std::ops::Range { start, end } = range;
        let file = file.to_str().unwrap().to_string();
        Self {
            file,
            start,
            end,
            path,
            path_ids,
        }
    }
    #[allow(unused)]
    pub(crate) fn from_pos<P>(pos: &P) -> Self
    where
        P: position_accessors::WithOffsets<Idx = Idx>
            + position_accessors::WithPreOrderPath<IdN>
            + position_accessors::FileAndOffsetPostionT<IdN, IdO = usize>,
    {
        let mut path = vec![];
        let mut path_ids = vec![];
        for (o, i) in pos.iter_offsets_and_nodes() {
            path.push(o);
            path_ids.push(i);
        }
        Self::from_file_and_range(&pos.file(), pos.start()..pos.end(), path, path_ids)
    }
    pub(crate) fn globalize(self, spec: &Repo, commit: Oid) -> PieceOfCode<IdN, Idx> {
        PieceOfCode {
            user: spec.user().to_string(),
            name: spec.name().to_string(),
            commit,
            path: self.path,
            path_ids: self.path_ids,
            file: self.file,
            start: self.start,
            end: self.end,
        }
    }
    #[allow(unused)]
    fn map_path<Idx2, F: Fn(Idx) -> Idx2>(self, f: F) -> LocalPieceOfCode<IdN, Idx2> {
        let path = self.path.into_iter().map(f).collect();
        LocalPieceOfCode {
            path,
            path_ids: self.path_ids,
            file: self.file,
            start: self.start,
            end: self.end,
        }
    }
}

fn custom_ser<IdN: Clone + Into<crate::IdN>, S>(
    x: &Vec<IdN>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::ser::SerializeSeq;
    let mut seq = serializer.serialize_seq(Some(x.len()))?;
    for element in x {
        let element: crate::IdN = element.clone().into();
        let id: u64 = unsafe { std::mem::transmute(element) };
        if id > u32::MAX as u64 {
            log::error!("node ids are too big, it will lead to bugs when used")
        }
        seq.serialize_element(&id)?;
    }
    seq.end()
}
