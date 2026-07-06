
if [ "$1" = " " ] || [ "$1" = "" ]; then
    >&2 echo "compiling the executable..."
    RUSTFLAGS="-C target-cpu=native" \
    cargo build -p hyperast_benchmark_search --bin search --release
    e_flag=$?
fi
exe=target/release/search

if [ "$1" = "" ]; then
    if [ "$e_flag" = 0 ]; then
        >&2 echo "executable compiled"
        >&2 echo "!Please now add the number of commits to compute as a parameter to your command!"
        $exe
        exit $?
    else
        >&2 echo "failed compilation!" $e_flag
        exit $e_flag
    fi
elif [ "$1" = " " ]; then
    exit $e_flag
fi

>&2 echo "$exe $@"
$exe $@
