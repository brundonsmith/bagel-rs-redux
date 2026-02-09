use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::ast::{container::AST, grammar, slice::Slice};
use crate::parse::module;

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
    /// - If `import_path` is relative (starts with `./` or `../`), it's resolved
    ///   against this module's directory (parent of the file path, or base of the URL)
    pub fn join(&self, import_path: &str) -> ModulePath {
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

#[derive(Debug, Clone)]
pub struct ModulesStore {
    pub modules: HashMap<ModulePath, Module>,
}

#[derive(Debug, Clone)]
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

    /// Re-parse this module from new source text, updating it in place.
    pub fn reload(&mut self, source: String) -> Result<(), ModuleLoadError> {
        let new = Self::from_source(self.path.clone(), source)?;
        self.source = new.source;
        self.ast = new.ast;
        Ok(())
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
            .map(|import_path| self.path.join(&import_path))
            .collect()
    }
}

impl ModulesStore {
    /// Create an empty store.
    pub fn new() -> Self {
        ModulesStore {
            modules: HashMap::new(),
        }
    }

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

    /// Add a single file (and its transitive imports) to the store.
    /// Skips files already present.
    pub fn add_file(&mut self, file_path: PathBuf) -> Result<(), ModuleLoadError> {
        let mut queue = vec![ModulePath::File(file_path)];

        while let Some(path) = queue.pop() {
            if self.modules.contains_key(&path) {
                continue;
            }

            let loaded = match &path {
                ModulePath::File(fp) => Module::load_file(fp.clone())?,
                ModulePath::Url(_) => continue, // skip URL imports in sync context
            };

            let imports = loaded.import_paths();
            self.modules.insert(path, loaded);
            queue.extend(imports);
        }

        Ok(())
    }

    /// Remove a file from the store.
    pub fn remove_file(&mut self, file_path: &Path) {
        self.modules
            .remove(&ModulePath::File(file_path.to_path_buf()));
    }

    /// Reload a file from new source text. If the file isn't in the store yet,
    /// it is added. Any new imports discovered are loaded from disk.
    pub fn reload_file(&mut self, file_path: &Path, source: String) -> Result<(), ModuleLoadError> {
        let key = ModulePath::File(file_path.to_path_buf());

        match self.modules.get_mut(&key) {
            Some(existing) => {
                existing.reload(source)?;
                // Load any new imports that aren't already in the store
                let imports = existing.import_paths();
                for import_path in imports {
                    if !self.modules.contains_key(&import_path) {
                        if let ModulePath::File(fp) = &import_path {
                            let _ = self.add_file(fp.clone());
                        }
                    }
                }
            }
            None => {
                let loaded = Module::from_source(key.clone(), source)?;
                let imports = loaded.import_paths();
                self.modules.insert(key, loaded);
                for import_path in imports {
                    if !self.modules.contains_key(&import_path) {
                        if let ModulePath::File(fp) = &import_path {
                            let _ = self.add_file(fp.clone());
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub fn find_imported(&self, from: &Module, import_path: &str) -> Option<&Module> {
        let path = from.path.join(import_path);
        self.modules.get(&path)
    }
}
