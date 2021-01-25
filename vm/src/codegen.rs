use crate::db::CodegenDatabase;
use errors::{FileId, Reporter, WithError};
use semant::{hir::{NameId}, StackedMap, TypeMap};

#[derive(Debug, Clone, Copy)]
struct LoopDescription {
    /// The index of the start label
    start: usize,
    /// The index of the end label
    end: usize,
}

#[derive(Debug)]
pub(crate) struct CodegenBuilder<DB> {
    db: DB,
    type_map: TypeMap,
    chunks: Vec<Chunk>,
}

struct Builder<DB> {
    db: DB,
    chunk: Chunk,
    current_loop: Option<LoopDescription>,
    ///  A linked list of all the objects allocated. This
    /// is passed to the vm so runtime collection can be done
    pub objects: RawObject,
    /// The stack slot of the variable
    slots: u32,
    line: u32,
    locals: StackedMap<NameId, usize>,
    params: FnvHashMap<NameId, usize>,
}

impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn codegen_function(&mut self, function: &hir::Function) {
        for (i, param) in function.params.iter().enumerate() {

            self.params.insert(param.);
            params.insert(param.name, i);
        }
    }
}
impl<'a, DB> InferDataCollector<&'a DB>
where
    DB: HirDatabase,
{
    pub(crate) fn codegen_function(&mut self, function: &hir::Function) {
        let builder = Builder {
            db: self.db,
            chunk: Chunk::new(),
            current_loop: None,
            objects: std::ptr::null::<RawObject>() as RawObject,
            slots: 0,
            line: 0,
            locals: StackedMap::new(),
            params: StackedMap::new(),
        };
    }
}

pub fn codegen_query(db: &impl CodegenDatabase, file: FileId) -> WithError<()> {
    let WithError(program, mut errors) = db.lower(file);
    let WithError(type_map, error) = db.infer(file);
    let reporter = Reporter::new(file);
    errors.extend(error);

    let collecter = CodegenBuilder {
        db,
        type_map,
        chunks: Vec::new(),
    };

    for function in &program.functions {
        collector.codegen_function(function);
    }

    WithError((), errors)
}
