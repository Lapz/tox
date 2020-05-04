use reporting::files;
use std::{
    fs,
    io::{self, Read},
    ops::Range,
    path::PathBuf,
    sync::Arc,
};
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FileId(salsa::InternId);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct File {
    /// The name of the file.
    pub name: Arc<String>,
    /// The source code of the file.
    pub source: Arc<String>,
    /// The starting byte indices in the source code.
    pub line_starts: Vec<usize>,
}

impl salsa::InternKey for FileId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        FileId(v)
    }
    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[salsa::query_group(FileDatabaseStorage)]
pub trait FileDatabase {
    #[salsa::interned]
    fn intern_file(&self, path: PathBuf) -> FileId;
    fn source(&self, file: FileId) -> Arc<String>;
    fn name(&self, file: FileId) -> Arc<String>;
    fn file(&self, file: FileId) -> Arc<File>;
    fn line_index(&self, file: FileId, byte_index: usize) -> Option<usize>;
    fn line_range(&self, file: FileId, line_index: usize) -> Option<Range<usize>>;
}

fn source(db: &impl FileDatabase, file_id: FileId) -> Arc<String> {
    let contents = read_file(&db.lookup_intern_file(file_id))
        .expect("Couldn't read file. TODO handle deletion of file");
    Arc::new(contents)
}

fn name(db: &impl FileDatabase, file_id: FileId) -> Arc<String> {
    Arc::new(String::from(
        db.lookup_intern_file(file_id).to_str().unwrap(),
    ))
}

fn line_index(db: &impl FileDatabase, file_id: FileId, byte_index: usize) -> Option<usize> {
    match db.file(file_id).line_starts.binary_search(&byte_index) {
        Ok(line) => Some(line),
        Err(next_line) => Some(next_line - 1),
    }
}

fn line_range(db: &impl FileDatabase, file_id: FileId, line_index: usize) -> Option<Range<usize>> {
    let file = db.file(file_id);
    let line_start = file.line_start(line_index)?;
    let next_line_start = file.line_start(line_index + 1)?;

    Some(line_start..next_line_start)
}

fn file(db: &impl FileDatabase, file_id: FileId) -> Arc<File> {
    let name = db.name(file_id);
    let source = db.source(file_id);
    let line_starts = files::line_starts(&source).collect();

    Arc::new(File {
        name,
        source,
        line_starts,
    })
}

fn read_file(name: &PathBuf) -> io::Result<String> {
    let mut file = fs::File::open(name)?;

    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    Ok(contents)
}

impl File {
    fn line_start(&self, line_index: usize) -> Option<usize> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => self.line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(self.source.len()),
            Ordering::Greater => None,
        }
    }
}
