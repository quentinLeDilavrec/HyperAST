
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
    # input=benchmark_search/queries/typescript/file.scm
    # sub=$input

    # input=benchmark_search/queries/typescript/before_after.ts.scm
    # input0=benchmark_search/queries/typescript/before_after.0.ts.scm
    # sub=benchmark_search/queries/typescript/before_after_.ts.scm

    input=benchmark_search/queries/typescript/try_fail_catch.ts.scm
    input0=benchmark_search/queries/typescript/try_fail_catch.0.ts.scm
    sub=benchmark_search/queries/typescript/try_fail_catch_.ts.scm

    # input="--input $input OURS --sub $sub"
    # input="--input $input TSQ2 --sub $sub --prepare --blob --tree"
    input="--input $input0 TS --cache"
fi



run() {
    lang="TypeScript"
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


# run denoland deno dbcd1a9194f4a69b2ac96d925c90dcf9bf5a50cc
run microsoft TypeScript 8ef3e2f3d43c8c92bda9510c47f7d4d2b3aeca33
# run microsoft vscode 12717879c47434790e4e956f8e211dc6b1948efa
