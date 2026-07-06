
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
    # input=benchmark_search/queries/rust/file.scm
    # sub=$input

    input=benchmark_search/queries/rust/destructuring.rs.scm
    sub=benchmark_search/queries/rust/destructuring_.rs.scm

    # input="--input $input OURS --sub $sub"
    # input="--input $input TSQ2 --sub $sub --prepare --blob --tree"
    input="--input $input TS --cache"

fi


run() {
    lang="Rust"
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

run BurntSushi ripgrep dfe4a81d2591daca76d25ae4e052c34b26578155
run astral-sh ruff 03f787e51e94999977b9a5a32b0153d82d7e2142
run astral-sh uv 4dfdb673dc8655078385e0ad51982600fa3cfbca
run tursodatabase libsql ef758d96b9424a2d506f7e417f84b42b9b9a5412
run pola-rs polars 5e9cf5c09d3c0c316cba1ced8282cb835f76df5f
run denoland deno dbcd1a9194f4a69b2ac96d925c90dcf9bf5a50cc
run tursodatabase turso f15d6c17aba5be052ea20893f7a52448ec63b370
run rust-lang rust f46ec5218fe7829ac18323b5ee0b409a63169f27
