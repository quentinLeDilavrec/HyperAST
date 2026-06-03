# convenience help to fetch the repositories in the dataset in advance (thus shortening the benchmarks)
# it is not mandatory

RUSTFLAGS="-C target-cpu=native" \
    cargo run --release -p hyperast_benchmark_diffs \
        --bin fetch_repositories

# fetch large repositories directly with git command as libgit2 has a hard time with those
# NOTE they are in the dataset but they have been marked as no fetch
git clone --depth 100 https://github.com/chromium/chromium
git clone --depth 100 https://github.com/torvalds/linux
