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
        Command::Check { watch, targets } => {
            let files = resolve_targets(targets);
            eprintln!("TODO: check (watch: {watch}) {files:?}");
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
