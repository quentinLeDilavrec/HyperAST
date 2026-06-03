# bench a single commit per repository
RUSTFLAGS="-C target-cpu=native" \
    cargo bench -p hyperast_benchmark_diffs \
        --bench bottomup_runtime_repo -- \
            --quick
