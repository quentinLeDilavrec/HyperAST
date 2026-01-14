set -x
commits=$1
input=benchmark_search/queries/try_fail_catch_main.scm
sub=(benchmark_search/queries/try_stmt-fail-try_fail_catch.scm -s 2)
# exe=target/release/synth
exe=target/debug/synth

# RUST_BACKTRACE=1 \
# RUST_LOG=debug,hyperast_gen_ts_tsquery=trace,hyper_diff::matchers::heuristic::gt::lazy2_greedy_subtree_matcher=trace
RUSTFLAGS="-C target-cpu=native" \
cargo build -p hyperast_benchmark_search --bin synth # --release


run() {
    user=$1
    name=$2
    commit=$3

    # RUST_BACKTRACE=1 \
    # RUST_LOG=debug,hyperast_gen_ts_tsquery=trace,hyper_diff::matchers::heuristic::gt::lazy2_greedy_subtree_matcher=trace \
    $exe \
    $user $name $commit $commits --fetch

    RUST_LOG=debug,hyperast_vcs_git=warn,hyperast_tsquery::query=warn,hyperast_gen_ts_tsquery::no_fmt_legion=warn,blob=warn,hyperast_vcs_git=warn,hyperast_gen_ts_tsquery=info,hyper_diff::matchers::heuristic::gt::lazy2_greedy_subtree_matcher=trace \
    RUST_BACKTRACE=1 \
    $exe \
    $user $name $commit $commits UNIQ Java \
    --input $input \
    --sub benchmark_search/queries/try_stmt-fail-try_fail_catch.scm -s 2 \
    --output benchmark_search/synth_results/v1/$name

    echo $?

    true
}

run graphhopper graphhopper f5f2b7765e6b392c5e8c7855986153af82cc1abe
run apache maven be2b7f890d98af20eb0753650b6605a68a97ac05
run INRIA spoon 56e12a0c0e0e69ea70863011b4f4ca3305e0542b
run quarkusio quarkus 5ac8332061fbbd4f11d5f280ff12b65fe7308540
run apache logging-log4j2 ebfc8945a5dd77b617f4667647ed4b740323acc8
run javaparser javaparser 046bf8be251189452ad6b25bf9107a1a2167ce6f
run apache spark 885f4733c413bdbb110946361247fbbd19f6bba9
run google gson f79ea208b1a42d0ee9e921dcfb3694221a2037ed
run junit-team junit4 cc7c500584fcb85eaf98c568b7441ceac6dd335c
run jenkinsci jenkins be6713661c120c222c17026e62401191bdc4035c
run apache dubbo e831b464837ae5d2afac9841559420aeaef6c52b
run apache skywalking 38a9d4701730e674c9646173dbffc1173623cf24
run apache flink d67338a140bf1b744d95a514b82824bba5b16105
run aws aws-sdk-java 0b01b6c8139e050b36ef79418986cdd8d9704998
run aws aws-toolkit-eclipse 85417f68e1eb6d90d46e145229e390cf55a4a554
run netty netty c2b846750dd2131d65aa25c8cf66bf3649b248f9
run alibaba fastjson f56b5d895f97f4cc3bd787c600a3ee67ba56d4db
run alibaba arthas c661d2d24892ce8a09a783ca3ba82eda90a66a85
run google guava b30a7120f901b4a367b8a9839a8b8ba62457fbdf
run apache hadoop d5e97fe4d6baf43a5576cbd1700c22b788dba01e
run FasterXML jackson-core 3cb5ce818e476d5b0b504b1833c7d33be80e9ca4
run qos-ch slf4j 2b0e15874aaf5502c9d6e36b0b81fc6bc14a8531
run jacoco jacoco 62a2b556c26f0f42a2ae791a86dc39dd36d35392
