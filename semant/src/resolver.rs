mod alias;
mod data;
mod function;
mod imports;
mod module;
mod module_graph;
mod source_file;
#[macro_use]
#[cfg(test)]
mod tests;

pub(crate) use data::Resolver;
pub(crate) use imports::resolve_imports_query;
pub(crate) use module::resolve_modules_query;
pub(crate) use module_graph::module_graph_query;
pub(crate) use module_graph::ModuleGraph;
pub(crate) use source_file::resolve_exports_query;
pub(crate) use source_file::resolve_source_file_query;
