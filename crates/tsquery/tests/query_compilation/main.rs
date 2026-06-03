use std::{env, error::Error, ffi::OsStr, fs, path::Path, process::ExitCode};

use libtest_mimic::{Arguments, Failed, Trial};

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = Arguments::from_args();

    let tests = collect_tests()?;

    Ok(libtest_mimic::run(&args, tests).exit_code())
}

fn collect_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    fn visit_dir(path: &Path, tests: &mut Vec<Trial>) -> Result<(), Box<dyn Error>> {
        let mut queries = Vec::new();
        let mut ir = None;
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let file_type = entry.file_type()?;

            // Handle files
            let entry_path = entry.path();
            if file_type.is_file() {
                if entry_path.extension() == Some(OsStr::new("scm")) {
                    queries.push(entry_path);
                } else if entry_path.extension() == Some(OsStr::new("txt")) {
                    let name = entry_path.file_stem().unwrap().to_str().unwrap();
                    if name != "ir" {
                        let err = format!("expected 'ir.txt' file, got: {}", name);
                        return Err(err.into());
                    } else if ir.is_some() {
                        let err = format!("expected a single '.txt' file");
                        return Err(err.into());
                    }
                    ir = Some(entry_path);
                };
            } else if file_type.is_dir() {
                // Handle directories
                visit_dir(&entry_path, tests)?;
            }
        }

        if queries.is_empty() && ir.is_none() {
            // just a directory with no queries or ir file
            return Ok(());
        }

        let Some(ir) = ir else {
            let err = format!("expected a 'ir.txt' file in {}", path.display());
            return Err(err.into());
        };

        let ir_content = fs::read_to_string(ir).map_err(|e| format!("Cannot read IR file: {e}"))?;

        for query in queries {
            // let name = query
            //     .strip_prefix(env::current_dir()?)?
            //     .display()
            //     .to_string();

            let lang = query.file_stem().unwrap().to_str().unwrap();

            let q_content = fs::read_to_string(&query).map_err(|e| Box::new(e))?;

            let language = if lang == "tsq" {
                tree_sitter_query::language()
            } else if lang == "tsq2" {
                tree_sitter_query::language()
            } else {
                return Err(format!("no corresponding language: {lang}").into());
            };

            let ir_content = ir_content.clone();
            let prefix = env::current_dir()?.join("tests/query_compilation");
            let path = path.strip_prefix(prefix)?.display().to_string();
            let name = format!("{:<25}: {}", path, lang);
            let test = Trial::test(name, move || {
                check_compilation(&q_content, language, &ir_content)
            });
            tests.push(test);
        }

        Ok(())
    }

    // We recursively look for `.rs` files, starting from the current
    // directory.
    let mut tests = Vec::new();
    let current_dir = env::current_dir()?;
    visit_dir(&current_dir, &mut tests)?;

    Ok(tests)
}

fn check_compilation(q: &str, lang: hyperast_tsquery::Language, ir: &str) -> Result<(), Failed> {
    let q = hyperast_tsquery::Query::new(q, lang).unwrap();
    let q = q.to_string();
    if q != ir {
        let msg = pretty_assertions::StrComparison::new(ir, &q).to_string();
        // let msg = format!("query compilation failed: expected '{}', got '{}'", ir, q);
        Err(msg.into())
    } else {
        Ok(())
    }
}
