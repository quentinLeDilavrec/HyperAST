use hyperast_benchmark_diffs::check_commit;

fn main() {
    let inputs = hyperast_benchmark_diffs::REPOSITORIES;
    let mut inputs = (inputs.iter().cloned().enumerate())
        .map(|(i, x)| x.with(i, 1))
        .collect::<Vec<_>>();
    for i in inputs.iter_mut() {
        eprintln!("fetching {}/{}", i.user, i.name);
        if let Err(e) = check_commit(i) {
            eprintln!("repo {}/{} fetch and check failed: {}", i.user, i.name, e);
        }
    }
}
