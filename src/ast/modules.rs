use std::{collections::HashMap, path::PathBuf, sync::Arc};

use crate::ast::{container::AST, grammar, slice::Slice};
use crate::parse::parse::module;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModulePath {
    File(PathBuf),
    Url(String),
}

impl ModulePath {
    /// Resolve an import path relative to this module's path.
    ///
    /// - If `import_path` is an HTTP URL, it's returned as `ModulePath::Url`
    /// - If `import_path` is an absolute file path, it's returned as `ModulePath::File`
    /// - If `import_path` is relative, it's resolved against this module's
    ///   directory (parent of the file path, or base of the URL)
    pub fn resolve(&self, import_path: &str) -> ModulePath {
        // Absolute HTTP URL
        if import_path.starts_with("http://") || import_path.starts_with("https://") {
            return ModulePath::Url(import_path.to_string());
        }

        // Absolute file path
        if import_path.starts_with('/') {
            return ModulePath::File(PathBuf::from(import_path));
        }

        // Relative path: resolve against current module's directory
        match self {
            ModulePath::File(base) => {
                let dir = base.parent().unwrap_or(base);
                ModulePath::File(
                    dir.join(import_path)
                        .canonicalize()
                        .unwrap_or_else(|_| dir.join(import_path)),
                )
            }
            ModulePath::Url(base_url) => {
                let base_dir = base_url
                    .rfind('/')
                    .map(|i| &base_url[..=i])
                    .unwrap_or(base_url);
                ModulePath::Url(format!("{}{}", base_dir, import_path))
            }
        }
    }
}

pub struct ModulesStore {
    pub modules: HashMap<ModulePath, Module>,
}

pub struct Module {
    pub path: ModulePath,
    pub source: Slice,
    pub ast: AST<grammar::Module>,
}

#[derive(Debug)]
pub enum ModuleLoadError {
    IoError(std::io::Error),
    HttpError(String),
    ParseError(String),
}

impl From<std::io::Error> for ModuleLoadError {
    fn from(err: std::io::Error) -> Self {
        ModuleLoadError::IoError(err)
    }
}

impl Module {
    pub fn from_source(path: ModulePath, source: String) -> Result<Self, ModuleLoadError> {
        let source = Slice::new(Arc::new(source));
        let (_remaining, ast) =
            module(source.clone()).map_err(|e| ModuleLoadError::ParseError(format!("{}", e)))?;

        Ok(Module { path, source, ast })
    }

    pub fn load_file(file_path: PathBuf) -> Result<Self, ModuleLoadError> {
        let source = std::fs::read_to_string(&file_path)?;
        Self::from_source(ModulePath::File(file_path), source)
    }

    pub async fn load_url(url: String) -> Result<Self, ModuleLoadError> {
        let response = reqwest::get(&url)
            .await
            .map_err(|e| ModuleLoadError::HttpError(format!("{}", e)))?;

        let source = response
            .text()
            .await
            .map_err(|e| ModuleLoadError::HttpError(format!("{}", e)))?;

        Self::from_source(ModulePath::Url(url), source)
    }

    /// Extract the import paths from this module's AST declarations.
    fn import_paths(&self) -> Vec<ModulePath> {
        self.ast
            .unpack()
            .declarations
            .iter()
            .filter_map(|decl| match decl.unpack() {
                grammar::Declaration::ImportDeclaration(import) => {
                    Some(import.path.unpack().contents.as_str().to_string())
                }
                _ => None,
            })
            .map(|import_path| self.path.resolve(&import_path))
            .collect()
    }
}

impl ModulesStore {
    /// Load a set of entry-point files and transitively follow all imports.
    pub async fn load(
        entry_points: impl IntoIterator<Item = PathBuf>,
    ) -> Result<Self, ModuleLoadError> {
        let mut store = ModulesStore {
            modules: HashMap::new(),
        };

        let mut queue: Vec<ModulePath> = entry_points.into_iter().map(ModulePath::File).collect();

        while let Some(path) = queue.pop() {
            if store.modules.contains_key(&path) {
                continue;
            }

            let loaded = match &path {
                ModulePath::File(file_path) => Module::load_file(file_path.clone())?,
                ModulePath::Url(url) => Module::load_url(url.clone()).await?,
            };

            let imports = loaded.import_paths();
            store.modules.insert(path, loaded);
            queue.extend(imports);
        }

        Ok(store)
    }
}
