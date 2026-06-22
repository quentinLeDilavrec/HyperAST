use std::marker::PhantomData;

use hyperast_gen_ts_xml::legion::XmlTreeGen;

use crate::ParseErr;
use crate::processing::CacheHolding;
use crate::processing::ObjectName;
use crate::processing::ParametrizedProcessor2Handle as PCP2Handle;
use crate::processing::caches::Pom as PomCaches;

use super::MavenProc;
use super::POM;
use super::SimpleStores;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct PomParameter {}

pub struct PomProc {
    parameter: Option<PomParameter>,
    cache: PomCaches,
}

impl From<PomParameter> for PomProc {
    fn from(value: PomParameter) -> Self {
        PomProc {
            parameter: Some(value),
            cache: Default::default(),
        }
    }
}

impl PartialEq<PomProc> for PomParameter {
    fn eq(&self, other: &PomProc) -> bool {
        other.parameter.as_ref() == Some(self)
    }
}

pub(crate) fn handle_pom_file<'a, E>(
    tree_gen: &mut XmlTreeGen<'a, 'a, E>,
    name: &ObjectName,
    text: &'a [u8],
) -> Result<POM, ParseErr>
where
    E: hyperast::tree_gen::TsExtra<SimpleStores>,
{
    let tree = hyperast_gen_ts_xml::tree_sitter_parse(text);
    if tree.root_node().has_error() {
        log::warn!("bad CST: {:?}", name.try_str());
        log::debug!("{}", tree.root_node().to_sexp());
        if crate::PROPAGATE_ERROR_ON_BAD_CST_NODE {
            return Err(ParseErr::IllFormed);
        }
    }

    let (n, _) = tree_gen
        .generate_file(name.as_bytes(), text, tree.walk())
        .into();
    let x = n.local;
    // TODO extract submodules, dependencies and directories. maybe even more ie. artefact id, ...
    let x = POM {
        compressed_node: x.compressed_node,
        metrics: x.metrics,
        submodules: vec![],
        source_dirs: vec!["src/main/java".to_owned()],
        test_source_dirs: vec!["src/test/java".to_owned()],
    };
    Ok(x)
}

impl crate::preprocessed::RepositoryProcessor {
    pub(crate) fn handle_pom(
        &mut self,
        oid: git2::Oid,
        parent_acc: &mut super::MavenModuleAcc,
        name: ObjectName,
        repository: &git2::Repository,
        parameters: PCP2Handle<PomProc>,
    ) -> Result<(), crate::ParseErr> {
        use hyperast_gen_ts_xml::legion::XmlTreeGen;
        let x = self
            .processing_systems
            .caching_blob_handler::<crate::processing::file_sys::Pom>()
            .handle(oid, repository, &name, parameters, |_c, n, t| {
                let line_break = crate::_auto_configured_line_break(t);
                // let holder = c.mut_or_default::<PomProcessorHolder>();
                // let pom_proc = holder.with_parameters_mut(parameters.0);
                // let md_cache = &mut pom_proc.cache.object_map;
                let mut xml_tree_gen = XmlTreeGen::bare(self.main_stores.mut_with_ts()) //
                    .set_line_break(line_break);
                super::handle_pom_file(&mut xml_tree_gen, n, t)
            })?;
        let name = self.intern_object_name(&name);
        assert!(!parent_acc.primary.children_names.contains(&name));
        parent_acc.push_pom(name, x);
        Ok(())
    }
}

// impl CommitProc for PomProc {
//     fn prepare_processing(
//         &self,
//         _repository: &git2::Repository,
//         _commit_builder: CommitBuilder,
//         _param_handle: PCPHandle,
//     ) -> Box<dyn PreparedCommitProc> {
//         unimplemented!("required for processing at the root of a project")
//     }

//     fn get_commit(&self, _commit_oid: git2::Oid) -> Option<&crate::Commit> {
//         unimplemented!("required for processing at the root of a project")
//     }

//     fn commit_count(&self) -> usize {
//         unimplemented!()
//     }
// }

impl CacheHolding<PomCaches> for PomProc {
    fn get_caches_mut(&mut self) -> &mut PomCaches {
        &mut self.cache
    }

    fn get_caches(&self) -> &PomCaches {
        &self.cache
    }
}

impl From<PCP2Handle<MavenProc>> for PCP2Handle<PomProc> {
    fn from(value: PCP2Handle<MavenProc>) -> Self {
        PCP2Handle(value.0, PhantomData)
    }
}
