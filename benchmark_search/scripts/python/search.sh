
if [ "$1" = "" ]; then
    bash $( dirname "${BASH_SOURCE[0]}" )/../search.sh " "
    if [ "$?" == "1" ]; then
        exit 1
    fi
    echo "!Please now provide the number of commits to compute as a parameter to the script!"
    echo "Usage:  $0 <COMMITS>"
    exit 0
fi

if [ "$1" = "fetch" ]; then
    fetch="--fetch"
    commits=0
else
    commits=$1

    # input=benchmark_search/queries/any/simple.scm
    # input=benchmark_search/queries/python/file.scm
    # sub=$input

    input=benchmark_search/queries/python/try_fail_expect.scm
    sub=benchmark_search/queries/python/try_fail_expect_.scm

    # input="--input $input OURS --sub $sub"
    input="--input $input TSQ2 --sub $sub --prepare --blob --tree"
    # input="--input $input TS --cache"
fi



run() {
    lang="Python"
    root="benchmark_search/search_results"
    config="$@ $commits $fetch --language $lang $input"
    out="$root/$lang"
    for x in "$@"; do out="$out/$x"; done
    out="$out/$(date +%FT%T)"
    echo $out
    echo "config=$config"
    echo "out=$out"
    mkdir -p $out

    # RUST_BACKTRACE=1 RUST_LOG=debug \
    bash $( dirname "${BASH_SOURCE[0]}" )/../search.sh $config \
        > >(tee -a $out/results.csv) 2> >(tee -a $out/results.stderr >&2)
}

run pallets click 8a1b1a33d739be05b7e91251e3c0dde77c5e152f
run pallets flask 36e4a824f340fdee7ed50937ba8e7f6bc7d17f81
# run numpy numpy 706b1035187baf72959059cd35ac1f5266e1932c
# run vllm-project vllm 091d13976c1c246714bb2112dd2e208561dda6a3
# run tensorflow tensorflow 3a401451953f52a909c0d08489ea36b4c9ca8e67
# run ceph ceph bfc7662a50dada0a88280b5e0bcaffaafc91aaa7
# run pola-rs polars 5e9cf5c09d3c0c316cba1ced8282cb835f76df5f
