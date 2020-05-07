#[salsa::database(
    errors::FileDatabaseStorage,
    parser::ParseDatabaseStorage,
    crate::HirDatabaseStorage,
    crate::InternDatabaseStorage
)]
#[derive(Debug, Default)]
pub struct MockDatabaseImpl {
    runtime: salsa::Runtime<MockDatabaseImpl>,
}

impl salsa::Database for MockDatabaseImpl {
    fn salsa_runtime(&self) -> &salsa::Runtime<MockDatabaseImpl> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<MockDatabaseImpl> {
        &mut self.runtime
    }
}

use serde::Deserialize;
use std::{
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
};

use tempfile::tempdir;
#[derive(Debug, Deserialize)]
pub struct DirectoryStructure {
    contents: Vec<TestStructure>,
}

#[derive(Debug, Deserialize)]
pub struct TestStructure {
    #[serde(default)]
    name: String,
    #[serde(default)]
    text: String,
    #[serde(default)]
    kind: Type,
    #[serde(default)]
    contents: Option<DirectoryStructure>,
}

#[derive(Debug, Deserialize, PartialEq)]
enum Type {
    File,
    Dir,
}

impl std::default::Default for Type {
    fn default() -> Self {
        Type::File
    }
}

pub fn create_structure(
    dir: &Path,
    structure: &DirectoryStructure,
    file_names: &mut Vec<PathBuf>,
) -> io::Result<()> {
    for test in &structure.contents {
        create_test(&dir, test, file_names)?
    }

    Ok(())
}

pub fn create_test(
    dir: &Path,
    test: &TestStructure,
    file_names: &mut Vec<PathBuf>,
) -> io::Result<()> {
    if test.kind == Type::Dir {
        fs::create_dir(dir.join(&test.name))?;

        create_structure(
            &dir.join(&test.name).as_path(),
            test.contents.as_ref().unwrap(),
            file_names,
        )?;
    } else {
        let file_path = dir.join(&test.name);

        file_names.push(file_path.clone());
        let mut file = File::create(file_path)?;
        write!(&mut file, "{}", test.text)?;
    }
    Ok(())
}

pub fn load_file<P: AsRef<Path>>(p: P) -> DirectoryStructure {
    return ron::de::from_str::<DirectoryStructure>(
        &fs::read_to_string(p).expect("Couldn't read file"),
    )
    .expect("Invalid ron file");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() -> io::Result<()> {
        let dir = tempdir()?;

        let structure = load_file(&format!(
            "{}/src/resolver/tests/with_dir.ron",
            env!("CARGO_MANIFEST_DIR")
        ));

        let mut file_names = Vec::new();

        create_structure(&dir.path(), &structure, &mut file_names)?;

        use walkdir::WalkDir;

        for entry in WalkDir::new(dir.path()) {
            println!("{}", entry?.path().display());
        }

        dir.close()?;

        Ok(())
    }
}
