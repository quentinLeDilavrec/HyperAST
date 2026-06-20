use hyperast::store::nodes::legion::RawHAST;

use crate::processing::erased::{ConfigParametersHandle, ParametrizedCommitProc2 as _};
use crate::processors::java::JavaProc;
use crate::processors::maven::MavenModuleAcc;
use crate::{preprocessed::RepositoryProcessor, processing::ProcessorHolder};

pub(super) type ScriptingPrepro<'a, 'hast> = hyperast::scripting::Prepro<
    RawHAST<'hast, hyperast_gen_ts_java::TStore>,
    &'a hyperast_gen_ts_java::legion_with_refs::Acc,
>;

impl MavenModuleAcc {
    pub(super) fn init_scripting(mut self, prep_scripting: Option<&ScriptingPrepro>) -> Self {
        if let Some(more) = prep_scripting {
            log::info!("prep_scripting");
            use hyperast::tree_gen::Prepro;
            match more.preprocessing(hyperast_gen_ts_java::Type::Directory) {
                Ok(acc) => self.scripting_acc = Some(acc),
                Err(err) => {
                    log::error!("error when handling maven modules {}", err);
                }
            }
        } else {
            log::trace!("no prep_scripting");
        };
        self
    }
}

// TODO generalize and factor similar preps
// and use the type in ParametrizedCommitProcessor2Handle to get the Holder
pub(super) fn prep_scripting(
    prepro: &RepositoryProcessor,
    handle: ConfigParametersHandle,
) -> Option<&crate::Str> {
    prepro
        .processing_systems
        // it is fine but could do better and kind of use MavenHolder
        .get::<ProcessorHolder<JavaProc>>()
        .as_ref()?
        .with_parameters(handle)
        .parameter
        .prepro
        .as_ref()
}
