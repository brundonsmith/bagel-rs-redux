use std::sync::Arc;

use bagel::ast::slice::Slice;
use bagel::check::{BagelError, BagelErrorDetails, CheckContext, Checkable};
use bagel::config::{Config, RuleSeverity};
use bagel::parse::parse;
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

    match cli.command {
        Command::Check { watch: _, targets } => {
            let files = resolve_targets(targets);
            let config = Config::default();
            let mut error_count = 0;

            for file_path in &files {
                let source = match std::fs::read_to_string(file_path) {
                    Ok(s) => s,
                    Err(e) => {
                        eprintln!("Failed to read {}: {e}", file_path.display());
                        continue;
                    }
                };

                let slice = Slice::new(Arc::new(source));

                let ast = match parse::module(slice.clone()) {
                    Ok((_, ast)) => ast,
                    Err(nom_err) => {
                        let bagel_err = match nom_err {
                            nom::Err::Error(e) | nom::Err::Failure(e) => e,
                            nom::Err::Incomplete(_) => BagelError {
                                src: slice,
                                severity: RuleSeverity::Error,
                                details: BagelErrorDetails::ParseError {
                                    message: "Unexpected end of input".to_string(),
                                },
                            },
                        };
                        eprint!("{}", bagel_err.write_for_terminal(file_path));
                        error_count += 1;
                        continue;
                    }
                };

                let mut errors = Vec::new();
                ast.check(&CheckContext { config: &config }, &mut |e| {
                    errors.push(e);
                });

                error_count += errors.len();
                for error in &errors {
                    eprint!("{}", error.write_for_terminal(file_path));
                }
            }

            if error_count > 0 {
                let file_word = if files.len() == 1 { "file" } else { "files" };
                let error_word = if error_count == 1 { "error" } else { "errors" };
                eprintln!(
                    "\nFound {error_count} {error_word} in {} {file_word}.",
                    files.len()
                );
                std::process::exit(1);
            }
        }
        Command::Fix { targets } => {
            let files = resolve_targets(targets);
            eprintln!("TODO: fix {files:?}");
        }
        Command::Test { watch, targets } => {
            let files = resolve_targets(targets);
            eprintln!("TODO: test (watch: {watch}) {files:?}");
        }
        Command::Build { targets } => {
            let files = resolve_targets(targets);
            eprintln!("TODO: build {files:?}");
        }
        Command::Lsp => {
            bagel::lsp::run_lsp().await;
        }
    }
}
