use crate::hir::ModuleId;
use crate::HirDatabase;
use errors::{FileId, Reporter, WithError};

/// Resolves all modules
/// Our module structure is as follows
///  a module can be declared in the same file
/// i.e
/// -- main.tox --
/// mod foo;
///  |- main.tox
///  |-foo.tox
/// If the second form is
/// |-foo
/// | |-bar.tox

pub fn resolve_modules_query(
    db: &impl HirDatabase,
    file: FileId,
    mod_id: ModuleId,
) -> WithError<Option<FileId>> {
    let mut reporter = Reporter::new(file);
    let module = db.lower_module(file, mod_id);
    let name = db.lookup_intern_name(module.name.item);

    let span = (module.span.start().to_usize(), module.span.end().to_usize());

    let mut path_buf = db.lookup_intern_file(module.file);
    path_buf.pop();

    let mut dir = path_buf.clone();

    dir.push(format!("{}", name));

    path_buf.push(format!("{}.tox", name));

    let (file_exists, dir_exists) = (path_buf.exists(), dir.exists());

    match (file_exists, dir_exists) {
        (false, false) => {
            reporter.error(format!("Unresolved module `{}`", name), "", span);

            WithError(None, reporter.finish())
        }

        (true, false) => {
            if path_buf == db.lookup_intern_file(module.file) {
                reporter.error(
                    format!("Unresolved module `{}`", name),
                    format!("Sub-module folder for `{}` is missing", name),
                    span,
                );

                WithError(None, reporter.finish())
            } else {
                // add a path from file -> module.file_id
                WithError(Some(db.intern_file(path_buf)), reporter.finish())
            }
        }

        (false, true) => {
            dir.push(format!("{}.tox", name));

            if !dir.exists() {
                reporter.error(
                    format!("Unresolved module `{}`", name),
                    "Sub-module's exist but the module file doesn't ",
                    span,
                );
                WithError(None, reporter.finish())
            } else {
                WithError(Some(db.intern_file(dir)), reporter.finish())
            }
        }

        (true, true) => {
            dir.push(format!("{}.tox", name));

            // module exists and is the same as the one its being declared in
            // check its children and report an err if its not found
            if path_buf == db.lookup_intern_file(module.file) && !dir.exists() {
                reporter.error(format!("Unresolved module `{}`", name), "", span);

                WithError(None, reporter.finish())
            } else if dir.exists() && path_buf.exists() {
                reporter.error(
                    format!("Conflicting module `{}`", name),
                    format!(
                        "{} exists and so does {}. You can only have the file or the dir not both",
                        dir.display(),
                        path_buf.display(),
                    ),
                    span,
                );

                WithError(None, reporter.finish())
            } else if dir.exists() {
                WithError(Some(db.intern_file(dir)), reporter.finish())
            } else {
                WithError(Some(db.intern_file(path_buf)), reporter.finish())
            }

            // add a path from file -> module.file_id
        }
    }
}
