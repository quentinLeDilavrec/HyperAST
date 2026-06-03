use hyperast_vcs_git::processing::RepoConfig::{CppMake, JavaMaven};

use crate::InputRepo;

#[rustfmt::skip]
pub const REPOSITORIES: &[InputRepo] = &[
    // Java
    java_repo("qos-ch", "slf4j", "2b0e15874aaf5502c9d6e36b0b81fc6bc14a8531"),
    java_repo("google", "gson", "f79ea208b1a42d0ee9e921dcfb3694221a2037ed"),
    java_repo("junit-team", "junit4", "cc7c500584fcb85eaf98c568b7441ceac6dd335c"), //
    java_repo("jacoco", "jacoco", "62a2b556c26f0f42a2ae791a86dc39dd36d35392"), //
    java_repo("alibaba", "arthas", "c661d2d24892ce8a09a783ca3ba82eda90a66a85"),
    java_repo("FasterXML", "jackson-core", "3cb5ce818e476d5b0b504b1833c7d33be80e9ca4"),
    java_repo("apache", "skywalking", "38a9d4701730e674c9646173dbffc1173623cf24"),
    java_repo("apache", "spark", "885f4733c413bdbb110946361247fbbd19f6bba9"),
    java_repo("apache", "maven", "be2b7f890d98af20eb0753650b6605a68a97ac05"),
    java_repo("aws", "aws-toolkit-eclipse", "85417f68e1eb6d90d46e145229e390cf55a4a554"),
    java_repo("INRIA", "spoon", "56e12a0c0e0e69ea70863011b4f4ca3305e0542b"),
    java_repo("javaparser", "javaparser", "046bf8be251189452ad6b25bf9107a1a2167ce6f"),
    java_repo("jenkinsci", "jenkins", "be6713661c120c222c17026e62401191bdc4035c"),
    java_repo("apache", "logging-log4j2", "ebfc8945a5dd77b617f4667647ed4b740323acc8"),
    java_repo("alibaba", "fastjson", "f56b5d895f97f4cc3bd787c600a3ee67ba56d4db"),
    java_repo("apache", "dubbo", "e831b464837ae5d2afac9841559420aeaef6c52b"),
    java_repo("netty", "netty", "c2b846750dd2131d65aa25c8cf66bf3649b248f9"),
    java_repo("google", "guava", "b30a7120f901b4a367b8a9839a8b8ba62457fbdf"),
    java_repo("quarkusio", "quarkus", "5ac8332061fbbd4f11d5f280ff12b65fe7308540"),
    java_repo("apache", "flink", "d67338a140bf1b744d95a514b82824bba5b16105"),
    java_repo("apache", "hadoop", "d5e97fe4d6baf43a5576cbd1700c22b788dba01e"),
    // Cpp
    cpp_repo("official-stockfish", "Stockfish", "f3bfce353168b03e4fedce515de1898c691f81ec"),
    cpp_repo("ffmpeg", "ffmpeg", "22179c308fce548edbaa21d124e2ff8a817b36b9"),
    cpp_repo("boostorg", "json", "f2992822afa6bcd7edd7f591ef6a59b5b4025f42"),
    // cpp_repo("rigtorp", "awesome-modern-cpp", "430f8e9af9745e9789003c4be5e0a8a6afaa75da"), // only size 3
    // Input {
    //     user: "chromium", name: "chromium",
    //     commit: "f461f9752e5918c5c87f2e3767bcb24945ee0fa0",
    //     config: CppMake,
    //     fetch: false, // too large
    // },
];

const fn java_repo(user: &'static str, name: &'static str, commit: &'static str) -> InputRepo {
    InputRepo {
        user: user,
        name: name,
        commit: commit,
        config: JavaMaven,
        fetch: true,
    }
}

const fn cpp_repo(user: &'static str, name: &'static str, commit: &'static str) -> InputRepo {
    InputRepo {
        user: user,
        name: name,
        commit: commit,
        config: CppMake,
        fetch: true,
    }
}
