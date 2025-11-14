use crate::app::types::Config;

#[derive(Clone)]
pub(super) struct Query {
    pub(crate) description: &'static str,
    pub(crate) query: &'static str,
}
#[derive(Clone)]
pub(super) struct Example {
    pub(crate) name: &'static str,
    pub(crate) commit: Commit,
    pub(crate) config: Config,
    pub(crate) commits: usize,
    pub(crate) query: Query,
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum Forge {
    GitHub,
    GitLab,
}

#[derive(Clone)]
pub struct Repo {
    #[allow(dead_code)]
    pub forge: Forge,
    pub user: &'static str,
    pub name: &'static str,
}

#[derive(Clone)]
pub struct Commit {
    pub repo: Repo,
    pub id: &'static str,
}

impl From<&Repo> for super::super::types::Repo {
    fn from(value: &Repo) -> Self {
        Self {
            user: value.user.into(),
            name: value.name.into(),
        }
    }
}
impl From<&Commit> for super::super::types::Commit {
    fn from(value: &Commit) -> Self {
        Self {
            repo: (&value.repo).into(),
            id: value.id.into(),
        }
    }
}

const BASE_SPOON_EX: Example = Example {
    name: "",
    commit: Commit {
        repo: Repo {
            forge: Forge::GitHub,
            user: "INRIA",
            name: "spoon",
        },
        id: "56e12a0c0e0e69ea70863011b4f4ca3305e0542b",
    },
    config: Config::MavenJava,
    commits: 1,
    query: Query {
        description: "",
        query: "",
    },
};

pub(super) const EXAMPLES: &[Example] = &[
    Example {
        name: "default example (Java)",
        query: Query {
            description: "Count the number of class declarations.
    ",
            query: "
    (class_declaration)
            ",
        },
        ..BASE_SPOON_EX
    },Example {
        name: "example 2 (Java)",
        query: Query {
            description: "Count the number of public class with a superclass and interfaces and that starts with a method.
    ",
            query: r#"(class_declaration
    (modifiers "public")
    superclass: (_)
    interfaces: (_)
    (class_body
        .
        (method_declaration)
    )
)"#,
        },
        ..BASE_SPOON_EX
    },Example {
        name: "example 2 (Java)",
        query: Query {
            description: "Count the number of public class with a superclass and interfaces and that starts with a method.
    ",
            query: r#"(program
  (package_declaration (_)@pkg)
  (class_declaration
    (modifiers "public")
    name: (_) @name
    body: (_
        [
          (method_declaration
            (modifiers
              . ; this is very important otherwise the complexity explodes
              [
                (marker_annotation
                  name: (_)@_anot (#any-eq? @_anot "Test")
                )
                (_)
              ]+
            )
            name: (_)@meth
          )
          (_)
        ]+
    )
  )
)"#,
        },
        ..BASE_SPOON_EX
    }];
