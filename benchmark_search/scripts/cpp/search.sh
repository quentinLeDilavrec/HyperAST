
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
    input=benchmark_search/queries/c/file.scm
    sub=$input

    input=benchmark_search/queries/c/broken_preproc.scm
    input0=benchmark_search/queries/c/broken_preproc.0.scm
    sub=benchmark_search/queries/c/broken_preproc_.scm

    # input="--input $input OURS --sub $sub"
    input="--input $input TSQ2 --sub $sub --prepare --blob --tree"
    # input="--input $input0 TS --cache"
fi



run() {
    lang="Cpp"
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

# run tensorflow tensorflow 3a401451953f52a909c0d08489ea36b4c9ca8e67
# run ceph ceph bfc7662a50dada0a88280b5e0bcaffaafc91aaa7
# run opencv opencv dd541965e9fc406f03cd6b56f1d62ecfc6cac4ab
# run official-stockfish Stockfish f3bfce353168b03e4fedce515de1898c691f81ec
run boostorg json f2992822afa6bcd7edd7f591ef6a59b5b4025f42
# run godotengine godot 8222d0983aa49f0bd4d74dde05d1a801269231db
# run tursodatabase libsql ef758d96b9424a2d506f7e417f84b42b9b9a5412
