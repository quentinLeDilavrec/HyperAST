use std::{env, path::Path};

use hyperast_benchmark_diffs::{
    buggy_fixed::{buggy_fixed_dataset_roots, run_dir},
    setup_env_logger,
};
#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

pub(crate) fn main() {
    setup_env_logger();
    let args: Vec<String> = std::env::args().collect();
    // hyperast_benchmark_diffs::with_profiling(Path::new("profile.pb"), || {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let dataset = args.get(1).map_or("defects4j", |x| x);
    let src_dst = buggy_fixed_dataset_roots(root, dataset);
    let path = args.get(2).map_or("Json", |x| x);
    let [src, dst] = src_dst.map(|x| x.join(path));
    eprintln!("{}", run_dir(&src, &dst).unwrap());
    // });
}
