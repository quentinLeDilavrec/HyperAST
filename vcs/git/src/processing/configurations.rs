pub enum BuildSystem {
    Maven,
    Make,
    Npm,
    None,
}

pub enum ProcessingConfig<P> {
    Java { limit: usize, dir_path: P },
    JavaMaven { limit: usize, dir_path: P },
    CppMake { limit: usize, dir_path: P },
    Cpp { limit: usize, dir_path: P },
    TsNpm { limit: usize, dir_path: P },
    Python { limit: usize, dir_path: P },
    Any { limit: usize, dir_path: P },
}

/// Contains repository configuration,
/// where each config given the same commit should produce the same result in the hyperast
#[derive(serde::Deserialize, Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum RepoConfig {
    Java,
    CppMake,
    Cpp,
    JavaMaven,
    TsNpm,
    Python,
    Any,
}

impl std::str::FromStr for RepoConfig {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "CppMake" => Self::CppMake,
            "Cpp" => Self::CppMake,
            "cpp" => Self::CppMake,
            "JavaMaven" => Self::JavaMaven,
            "Java" => Self::JavaMaven,
            "java" => Self::JavaMaven,
            "typescript" => Self::TsNpm,
            "javascript" => Self::TsNpm,
            "python" => Self::Python,
            "Ts" => Self::TsNpm,
            "ts" => Self::TsNpm,
            "any" => Self::Any,
            x => return Err(format!("'{}' is not available config", x)),
        })
    }
}

impl From<&RepoConfig> for ProcessingConfig<&'static str> {
    fn from(value: &RepoConfig) -> Self {
        match value {
            RepoConfig::CppMake => Self::CppMake {
                limit: 3,
                dir_path: "",
            },
            RepoConfig::Cpp => Self::Cpp {
                limit: 3,
                dir_path: "",
            },
            RepoConfig::JavaMaven => Self::JavaMaven {
                limit: 3,
                dir_path: "",
            },
            RepoConfig::Java => Self::Java {
                limit: 3,
                dir_path: "",
            },
            RepoConfig::TsNpm => todo!(),
            RepoConfig::Python => Self::Python {
                limit: 3,
                dir_path: "",
            },
            RepoConfig::Any => todo!(),
        }
    }
}
impl From<RepoConfig> for ProcessingConfig<&'static str> {
    fn from(value: RepoConfig) -> Self {
        (&value).into()
    }
}
