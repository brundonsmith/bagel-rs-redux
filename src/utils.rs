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
/// otherwise add `path` directly.
fn collect_path(path: &Path, results: &mut HashSet<PathBuf>) {
    if path.is_dir() {
        glob::glob(
            path.join("**/*.bgl")
                .to_str()
                .expect("Non-UTF-8 path"),
        )
        .expect("Invalid glob pattern")
        .filter_map(Result::ok)
        .for_each(|p| { results.insert(p); });
    } else {
        results.insert(path.to_path_buf());
    }
}
