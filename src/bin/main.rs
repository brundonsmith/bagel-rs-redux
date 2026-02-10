use std::path::PathBuf;

use bagel::ast::modules::{ModulePath, ModulesStore};
use bagel::check::{CheckContext, Checkable};
use bagel::config::Config;
use bagel::utils::resolve_targets;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "bagel", about = "The Bagel language toolchain")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Type-check Bagel source files
    Check {
        #[arg(long)]
        watch: bool,
        /// Files, directories, or glob patterns to check
        targets: Vec<String>,
    },
    /// Auto-fix Bagel source files
    Fix {
        /// Files, directories, or glob patterns to fix
        targets: Vec<String>,
    },
    /// Run Bagel tests
    Test {
        #[arg(long)]
        watch: bool,
        /// Files, directories, or glob patterns to test
        targets: Vec<String>,
    },
    /// Compile Bagel to JavaScript
    Build {
        /// Files, directories, or glob patterns to build
        targets: Vec<String>,
    },
    /// Start the Language Server Protocol server
    Lsp,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    let config = match Config::load_from_ancestors(&std::env::current_dir().unwrap()) {
        Ok(config) => config,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    eprintln!("config: {:?}", config);

    match cli.command {
        Command::Check { watch: _, targets } => {
            let files = resolve_targets(targets);
            let store = match ModulesStore::load(files).await {
                Ok(store) => store,
                Err(e) => {
                    eprintln!("Failed to load modules: {:?}", e);
                    std::process::exit(1);
                }
            };
            let mut error_count = 0;
            let module_count = store.modules.len();

            for (path, module) in &store.modules {
                let file_path = match path {
                    ModulePath::File(p) => p.clone(),
                    ModulePath::Url(url) => PathBuf::from(url),
                };

                let mut errors = Vec::new();
                module.ast.check(
                    &CheckContext {
                        config: &config,
                        modules: &store,
                        current_module: Some(module),
                    },
                    &mut |e| {
                        errors.push(e);
                    },
                );

                error_count += errors.len();
                for error in &errors {
                    eprint!("{}", error.write_for_terminal(&file_path));
                }
            }

            if error_count > 0 {
                let file_word = if module_count == 1 { "file" } else { "files" };
                let error_word = if error_count == 1 { "error" } else { "errors" };
                eprintln!("\nFound {error_count} {error_word} in {module_count} {file_word}.",);
                std::process::exit(1);
            }
        }
        Command::Fix { targets } => {
            let files = resolve_targets(targets);
            let store = match ModulesStore::load(files).await {
                Ok(store) => store,
                Err(e) => {
                    eprintln!("Failed to load modules: {:?}", e);
                    std::process::exit(1);
                }
            };
            eprintln!("TODO: fix {} modules", store.modules.len());
        }
        Command::Test { watch, targets } => {
            let files = resolve_targets(targets);
            let store = match ModulesStore::load(files).await {
                Ok(store) => store,
                Err(e) => {
                    eprintln!("Failed to load modules: {:?}", e);
                    std::process::exit(1);
                }
            };
            eprintln!(
                "TODO: test (watch: {watch}) {} modules",
                store.modules.len()
            );
        }
        Command::Build { targets } => {
            let files = resolve_targets(targets);
            let store = match ModulesStore::load(files).await {
                Ok(store) => store,
                Err(e) => {
                    eprintln!("Failed to load modules: {:?}", e);
                    std::process::exit(1);
                }
            };
            eprintln!("TODO: build {} modules", store.modules.len());
        }
        Command::Lsp => {
            bagel::lsp::run_lsp().await;
        }
    }
}
