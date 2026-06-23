use git2::{Oid, Repository};

use crate::preprocessed::RepositoryProcessor;
use crate::processing::ParametrizedProcessorHandle as PPHandle;
use crate::processing::{CacheHolding, ObjectName};

use super::MakeModuleAcc;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Parameter {}

pub struct MakefileProc(Option<Parameter>, crate::processing::caches::Makefile);

impl From<Parameter> for MakefileProc {
    fn from(value: Parameter) -> Self {
        MakefileProc(Some(value), Default::default())
    }
}

impl PartialEq<MakefileProc> for Parameter {
    fn eq(&self, other: &MakefileProc) -> bool {
        Some(self) == other.0.as_ref()
    }
}

impl CacheHolding<crate::processing::caches::Makefile> for MakefileProc {
    fn get_caches_mut(&mut self) -> &mut crate::processing::caches::Makefile {
        &mut self.1
    }
    fn get_caches(&self) -> &crate::processing::caches::Makefile {
        &self.1
    }
}

pub fn matches(name: &ObjectName) -> bool {
    name.eq_str("Makefile")
}

impl crate::processing::CachesHolding for MakefileProc {
    type Caches = crate::processing::caches::Makefile;
}

use hyperast_gen_ts_xml::legion::XmlTreeGen;
impl RepositoryProcessor {
    pub(super) fn help_handle_makefile(
        &mut self,
        parent_acc: &mut MakeModuleAcc,
        oid: Oid,
        name: ObjectName,
        repository: &Repository,
        parameters: PPHandle<MakefileProc>,
    ) -> Result<(), crate::ParseErr> {
        let x = self
            .processing_systems
            .caching_blob_handler::<MakefileProc>()
            .handle(oid, repository, &name, parameters, |_c, n, t| {
                let mut bare = XmlTreeGen::bare(self.main_stores.mut_with_ts());
                handle_makefile_file(&mut bare, n, t).map_err(|_| crate::ParseErr::IllFormed)
            })?;
        let name = self.intern_object_name(&name);
        assert!(!parent_acc.primary.children_names.contains(&name));
        parent_acc.push_makefile(name, x);
        Ok(())
    }
}

pub(crate) fn handle_makefile_file<'a, E>(
    tree_gen: &mut XmlTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> Result<super::MakeFile, crate::ParseErr>
where
    E: hyperast::tree_gen::TsExtra<super::SimpleStores>,
{
    // let time = std::time::Instant::now();
    log::trace!("not parsing {} bytes long Makefile", text.len()); // TODO parse the makefile
    let text = b"<proj></proj>";
    let tree = hyperast_gen_ts_xml::tree_sitter_parse(text);
    // let parsing_time = time.elapsed();
    // super::super::report_or_fail_on_errored_tree!(name, tree, parsing_time);
    if tree.root_node().has_error() {
        log::warn!("bad CST");
        log::debug!("{:?}", name.try_str());
        log::debug!("{}", tree.root_node().to_sexp());
        if crate::PROPAGATE_ERROR_ON_BAD_CST_NODE {
            return Err(crate::ParseErr::IllFormed);
        }
    }
    let (n, _) = tree_gen
        .generate_file(name.as_bytes(), text, tree.walk())
        .into();
    let x = n.local;
    // TODO extract submodules, dependencies and directories. maybe even more ie. artefact id, ...
    let x = super::MakeFile {
        compressed_node: x.compressed_node,
        metrics: x.metrics,
        submodules: vec![],
        source_dirs: vec![".".to_owned()],
        test_source_dirs: vec!["../tests".to_owned()],
    };
    Ok(x)
}
