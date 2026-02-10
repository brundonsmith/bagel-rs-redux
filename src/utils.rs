use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// Given a list of path/glob patterns (or an empty list, meaning "use cwd"),
/// returns a deduplicated set of all matching `.bgl` file paths. Directories
/// are walked recursively. Glob patterns with `*` are expanded.
pub fn resolve_targets(patterns: Vec<String>) -> HashSet<PathBuf> {
    let patterns = if patterns.is_empty() {
        vec![".".to_string()]
    } else {
        patterns
    };

    let mut results = HashSet::new();

    for pattern in &patterns {
        let path = Path::new(pattern);

        if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
            glob::glob(pattern)
                .expect("Invalid glob pattern")
                .filter_map(Result::ok)
                .for_each(|p| collect_path(&p, &mut results));
        } else {
            collect_path(path, &mut results);
        }
    }

    results
}

/// If `path` is a directory, recursively add all `.bgl` files within;
/// otherwise add `path` directly. All paths are canonicalized to absolute
/// paths so that `ModulesStore` keys are always fully-qualified.
fn collect_path(path: &Path, results: &mut HashSet<PathBuf>) {
    if path.is_dir() {
        glob::glob(path.join("**/*.bgl").to_str().expect("Non-UTF-8 path"))
            .expect("Invalid glob pattern")
            .filter_map(Result::ok)
            .filter(|p| is_bagel_module(p))
            .for_each(|p| {
                results.insert(canonicalize_or_absolute(p));
            });
    } else if is_bagel_module(path) {
        results.insert(canonicalize_or_absolute(path.to_path_buf()));
    }
}

fn canonicalize_or_absolute(path: PathBuf) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| {
        std::env::current_dir()
            .map(|cwd| cwd.join(&path))
            .unwrap_or(path)
    })
}

fn is_bagel_module(path: &Path) -> bool {
    path.extension().and_then(|e| e.to_str()) == Some("bgl")
}

/// Determine the entrypoint file from CLI targets.
///
/// - Zero targets: `<pwd>/index.bgl`
/// - One target ending in `.bgl`: that file
/// - One target that's a directory: `<dir>/index.bgl`
/// - Otherwise: `None`
pub fn resolve_entrypoint(targets: &[String]) -> Option<PathBuf> {
    let cwd = std::env::current_dir().ok()?;
    let absolutize = |p: PathBuf| {
        if p.is_absolute() {
            p
        } else {
            cwd.join(p)
        }
    };
    match targets {
        [] => Some(cwd.join("index.bgl")),
        [target] if target.ends_with(".bgl") => Some(absolutize(PathBuf::from(target))),
        [target] if Path::new(target).is_dir() => {
            Some(absolutize(PathBuf::from(target).join("index.bgl")))
        }
        _ => None,
    }
}
