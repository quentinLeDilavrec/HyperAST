
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

    # input=benchmark_search/queries/any/simple.c.scm
    # input=benchmark_search/queries/c/file.scm
    # sub=$input

    input=benchmark_search/queries/c/broken_preproc.scm
    input0=benchmark_search/queries/c/broken_preproc.0.scm
    sub=benchmark_search/queries/c/broken_preproc_.scm

    # input="--input $input OURS --sub $sub"
    input="--input $input TSQ2 --sub $sub --prepare --blob --tree"
    # input="--input $input0 TS --cache"
fi



run() {
    lang="C"
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

# run numpy numpy 706b1035187baf72959059cd35ac1f5266e1932c
# run ffmpeg ffmpeg 22179c308fce548edbaa21d124e2ff8a817b36b9
run sqlite sqlite 19688708136ddfac9ea459ce393d8f4391fb057b
# run tursodatabase libsql ef758d96b9424a2d506f7e417f84b42b9b9a5412
run kraj musl 8cb84492b0245d70b2cd0edd523e2b55c7ad67a9
