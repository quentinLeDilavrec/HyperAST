if [ "$1" = "" ]; then
    bash $( dirname "${BASH_SOURCE[0]}" )/../../search.sh " "
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
fi

# input=benchmark_search/queries/try_fail_catch_focus.scm
input=benchmark_search/queries/try_fail_catch_main.scm

run() {
    echo "benchmarking $@"
    # RUST_BACKTRACE=1 RUST_LOG=debug \
    bash $( dirname "${BASH_SOURCE[0]}" )/../../search.sh $@ $commits \
        --language _Java \
        --input $input OURS --sub $input
        # \
        # 2> /dev/null

        # --sub benchmark_search/queries/try_stmt-fail-try_fail_catch.scm -s 0

}

run INRIA spoon 56e12a0c0e0e69ea70863011b4f4ca3305e0542b
run apache logging-log4j2 ebfc8945a5dd77b617f4667647ed4b740323acc8
run javaparser javaparser 046bf8be251189452ad6b25bf9107a1a2167ce6f
run junit-team junit4 cc7c500584fcb85eaf98c568b7441ceac6dd335c

# run apache maven be2b7f890d98af20eb0753650b6605a68a97ac05
# run jenkinsci jenkins be6713661c120c222c17026e62401191bdc4035c
# run apache skywalking 38a9d4701730e674c9646173dbffc1173623cf24
# run apache spark 885f4733c413bdbb110946361247fbbd19f6bba9
# run google gson f79ea208b1a42d0ee9e921dcfb3694221a2037ed
# run qos-ch slf4j 2b0e15874aaf5502c9d6e36b0b81fc6bc14a8531
# run jacoco jacoco 62a2b556c26f0f42a2ae791a86dc39dd36d35392
# run apache flink d67338a140bf1b744d95a514b82824bba5b16105
# run FasterXML jackson-core 3cb5ce818e476d5b0b504b1833c7d33be80e9ca4
# run alibaba fastjson f56b5d895f97f4cc3bd787c600a3ee67ba56d4db
# run alibaba arthas c661d2d24892ce8a09a783ca3ba82eda90a66a85
# run apache dubbo e831b464837ae5d2afac9841559420aeaef6c52b
# run aws aws-toolkit-eclipse 85417f68e1eb6d90d46e145229e390cf55a4a554
# run google guava b30a7120f901b4a367b8a9839a8b8ba62457fbdf
# run netty netty c2b846750dd2131d65aa25c8cf66bf3649b248f9
# run quarkusio quarkus 5ac8332061fbbd4f11d5f280ff12b65fe7308540
# run apache hadoop d5e97fe4d6baf43a5576cbd1700c22b788dba01e
