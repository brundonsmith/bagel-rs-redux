use std::fmt::Write;
use std::path::Path;

use crate::{
    ast::{
        container::AST,
        grammar::{Any, BinaryOperator, Expression, UnaryOperator},
        modules::{Module, ModulesStore},
        slice::Slice,
    },
    config::{Config, RuleSeverity},
    types::{fits::FitsContext, infer::InferTypeContext, NormalizeContext, Type},
};

#[derive(Debug, Clone)]
pub struct CheckContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
    pub current_module: Option<&'a Module>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BagelError {
    // pub module_id: ModuleID,
    pub src: Slice,
    pub severity: RuleSeverity,
    pub details: BagelErrorDetails,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BagelErrorDetails {
    ParseError { message: String },
    MiscError { message: String },
}

// ANSI color codes
const RESET: &str = "\x1b[0m";
const RED: &str = "\x1b[91m";
const YELLOW: &str = "\x1b[93m";
const CYAN: &str = "\x1b[96m";
const GREEN: &str = "\x1b[92m";
const BLUE: &str = "\x1b[94m";
const GRAY: &str = "\x1b[90m";
const WHITE_BG: &str = "\x1b[47m\x1b[30m"; // white background, black text

const KEYWORDS: &[&str] = &["const"];
const LITERAL_KEYWORDS: &[&str] = &["nil", "true", "false"];
const TYPE_KEYWORDS: &[&str] = &["unknown", "boolean", "number", "string"];

/// Returns true if `ch` can appear in an identifier (letter, digit, or underscore).
fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

/// Applies basic syntax highlighting to a line of Bagel source code.
/// Keywords are colored blue, literal keywords (nil/true/false) are yellow,
/// type keywords are cyan, number literals are yellow, and string literals
/// are green.
fn highlight_source_line(line: &str) -> String {
    let mut out = String::with_capacity(line.len() * 2);
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        let ch = chars[i];

        // String literals
        if ch == '\'' || ch == '"' {
            let quote = ch;
            out.push_str(YELLOW);
            out.push(ch);
            i += 1;
            while i < len && chars[i] != quote {
                if chars[i] == '\\' && i + 1 < len {
                    out.push(chars[i]);
                    i += 1;
                }
                out.push(chars[i]);
                i += 1;
            }
            if i < len {
                out.push(chars[i]);
                i += 1;
            }
            out.push_str(RESET);
            continue;
        }

        // Identifiers and keywords
        if ch.is_alphabetic() || ch == '_' {
            let start = i;
            while i < len && is_ident_char(chars[i]) {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();

            // if KEYWORDS.contains(&word.as_str()) {
            //     let _ = write!(out, "{CYAN}{word}{RESET}");
            // } else
            if LITERAL_KEYWORDS.contains(&word.as_str()) {
                let _ = write!(out, "{BLUE}{word}{RESET}");
            } else if TYPE_KEYWORDS.contains(&word.as_str()) {
                let _ = write!(out, "{GRAY}{word}{RESET}");
            } else {
                out.push_str(&word);
            }
            continue;
        }

        // Number literals
        if ch.is_ascii_digit() {
            let start = i;
            while i < len && (chars[i].is_ascii_digit() || chars[i] == '.') {
                i += 1;
            }
            let num: String = chars[start..i].iter().collect();
            let _ = write!(out, "{YELLOW}{num}{RESET}");
            continue;
        }

        // Everything else
        out.push(ch);
        i += 1;
    }

    out
}

impl BagelError {
    fn message(&self) -> &str {
        match &self.details {
            BagelErrorDetails::ParseError { message } => message,
            BagelErrorDetails::MiscError { message } => message,
        }
    }

    /// Computes (1-indexed line, 1-indexed column) from a byte offset into a source string.
    fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for (i, ch) in source.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    /// Formats this error for terminal display, mirroring the TypeScript
    /// compiler's `--pretty` output format with ANSI colors.
    ///
    /// The `file_path` argument is the path to display in the error header.
    pub fn write_for_terminal(&self, file_path: &Path) -> String {
        let source = self.src.full_string.as_str();
        let (line, col) = Self::offset_to_line_col(source, self.src.start);
        let display_path = file_path.display();

        let severity_label = match self.severity {
            RuleSeverity::Error => format!("{RED}error{RESET}"),
            RuleSeverity::Warn => format!("{YELLOW}warning{RESET}"),
            RuleSeverity::Autofix => format!("{GRAY}autofix{RESET}"),
        };

        let mut out = String::new();

        // Header line: path:line:col - error: message
        let _ = writeln!(
            out,
            "{CYAN}{display_path}{RESET}:{YELLOW}{line}{RESET}:{YELLOW}{col}{RESET} - {severity_label}: {}",
            self.message()
        );

        // Source code snippet
        let source_line = source.lines().nth(line - 1).unwrap_or("");
        let line_num_str = line.to_string();
        let gutter_width = line_num_str.len();

        // Blank line
        let _ = writeln!(out);

        // Source line with line number in white-bg gutter
        let highlighted = highlight_source_line(source_line);
        let _ = writeln!(out, "{WHITE_BG}{line_num_str}{RESET} {highlighted}");

        // Squiggle underline
        let error_len = (self.src.end - self.src.start).max(1);
        let padding = " ".repeat(gutter_width + 1 + (col - 1));
        let squiggle = "~".repeat(error_len);
        let _ = writeln!(out, "{padding}{RED}{squiggle}{RESET}");

        out
    }
}

pub trait Checkable {
    /// Checks a given AST node and its children for any reportable errors. This
    /// includes:
    /// - Any Malformed syntax subtrees
    /// - Type errors
    /// - References to undeclared identifiers
    /// - Configurable "rules" as defined in the Config (skipping the ones set
    ///   to RuleSeverityOrOff::Off)
    ///
    /// The implementation of check() should...
    /// 1. Recurse to all children of the current AST node (call check() on them)
    /// 2. Report any errors on the current node using report_error()
    ///
    /// Note that several atomic AST nodes will have nothing to check
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F);
}

impl<TKind> Checkable for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + TryFrom<TKind>,
    Any: From<TKind>,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        match self.details() {
            // Malformed nodes should be reported as errors
            None => {
                if let Some(message) = self.malformed_message() {
                    report_error(BagelError {
                        src: self.slice().clone(),
                        severity: RuleSeverity::Error,
                        details: BagelErrorDetails::ParseError {
                            message: message.to_string(),
                        },
                    });
                }
            }

            // Process valid nodes
            Some(details) => {
                // Recurse to all children generically
                details.for_each_child(&mut |child| {
                    child.check(ctx, report_error);
                });

                // Per-variant checking logic (type checks only)
                match details {
                    Any::Expression(expression) => {
                        match expression {
                            Expression::BinaryOperation(bin_op) => {
                                // Type-check operands
                                let norm_ctx = NormalizeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let infer_ctx = InferTypeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let left_type =
                                    bin_op.left.infer_type(infer_ctx).normalize(norm_ctx);
                                let right_type =
                                    bin_op.right.infer_type(infer_ctx).normalize(norm_ctx);

                                let (allowed, op_str) = match bin_op.operator.unpack() {
                                    BinaryOperator::Add => (
                                        Some(Type::Union {
                                            variants: vec![
                                                Type::Number {
                                                    min_value: None,
                                                    max_value: None,
                                                },
                                                Type::String { value: None },
                                            ],
                                        }),
                                        "+",
                                    ),
                                    BinaryOperator::Subtract => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        "-",
                                    ),
                                    BinaryOperator::Multiply => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        "*",
                                    ),
                                    BinaryOperator::Divide => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        "/",
                                    ),
                                    BinaryOperator::And => (
                                        Some(Type::Union {
                                            variants: vec![
                                                Type::Boolean { value: None },
                                                Type::Nil,
                                            ],
                                        }),
                                        "&&",
                                    ),
                                    BinaryOperator::Or => (
                                        Some(Type::Union {
                                            variants: vec![
                                                Type::Boolean { value: None },
                                                Type::Nil,
                                            ],
                                        }),
                                        "||",
                                    ),
                                    BinaryOperator::Equal => (None, "=="),
                                    BinaryOperator::NotEqual => (None, "!="),
                                    BinaryOperator::LessThan => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        "<",
                                    ),
                                    BinaryOperator::LessThanOrEqual => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        "<=",
                                    ),
                                    BinaryOperator::GreaterThan => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        ">",
                                    ),
                                    BinaryOperator::GreaterThanOrEqual => (
                                        Some(Type::Number {
                                            min_value: None,
                                            max_value: None,
                                        }),
                                        ">=",
                                    ),
                                    BinaryOperator::NullishCoalescing => (None, "??"),
                                };

                                if let Some(allowed) = allowed {
                                    let fits_ctx = FitsContext {
                                        modules: Some(ctx.modules),
                                    };
                                    let left_issues =
                                        left_type.clone().fit_issues(allowed.clone(), fits_ctx);
                                    let fits_ctx = FitsContext {
                                        modules: Some(ctx.modules),
                                    };
                                    let right_issues =
                                        right_type.clone().fit_issues(allowed, fits_ctx);
                                    if !left_issues.is_empty() {
                                        report_error(BagelError {
                                            src: bin_op.left.slice().clone(),
                                            severity: RuleSeverity::Error,
                                            details: BagelErrorDetails::MiscError {
                                                message: format!(
                                                    "Operator '{}' cannot be applied to type '{}'",
                                                    op_str, left_type
                                                ),
                                            },
                                        });
                                    }
                                    if !right_issues.is_empty() {
                                        report_error(BagelError {
                                            src: bin_op.right.slice().clone(),
                                            severity: RuleSeverity::Error,
                                            details: BagelErrorDetails::MiscError {
                                                message: format!(
                                                    "Operator '{}' cannot be applied to type '{}'",
                                                    op_str, right_type
                                                ),
                                            },
                                        });
                                    }
                                }
                            }
                            Expression::UnaryOperation(unary_op) => {
                                // Type-check operand
                                let norm_ctx = NormalizeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let infer_ctx = InferTypeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let operand_type =
                                    unary_op.operand.infer_type(infer_ctx).normalize(norm_ctx);

                                match unary_op.operator.unpack() {
                                    UnaryOperator::Not => {
                                        let allowed = Type::Union {
                                            variants: vec![
                                                Type::Boolean { value: None },
                                                Type::Nil,
                                            ],
                                        };
                                        let fits_ctx = FitsContext {
                                            modules: Some(ctx.modules),
                                        };
                                        let issues =
                                            operand_type.clone().fit_issues(allowed, fits_ctx);
                                        if !issues.is_empty() {
                                            report_error(BagelError {
                                                src: unary_op.operand.slice().clone(),
                                                severity: RuleSeverity::Error,
                                                details: BagelErrorDetails::MiscError {
                                                    message: format!(
                                                    "Operator '!' cannot be applied to type '{}'",
                                                    operand_type
                                                ),
                                                },
                                            });
                                        }
                                    }
                                }
                            }
                            Expression::IfElseExpression(if_else) => {
                                // Type-check: condition must be boolean or nil
                                let norm_ctx = NormalizeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let infer_ctx = InferTypeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let cond_type =
                                    if_else.condition.infer_type(infer_ctx).normalize(norm_ctx);
                                let allowed = Type::Union {
                                    variants: vec![Type::Boolean { value: None }, Type::Nil],
                                };
                                let fits_ctx = FitsContext {
                                    modules: Some(ctx.modules),
                                };
                                let issues = cond_type.clone().fit_issues(allowed, fits_ctx);
                                if !issues.is_empty() {
                                    report_error(BagelError {
                                        src: if_else.condition.slice().clone(),
                                        severity: RuleSeverity::Error,
                                        details: BagelErrorDetails::MiscError {
                                            message: format!(
                                                "Value of type '{}' can't be used as a condition",
                                                cond_type
                                            ),
                                        },
                                    });
                                }
                            }
                            Expression::PropertyAccessExpression(prop_access) => {
                                // Check that the property exists on the subject type
                                let norm_ctx = NormalizeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let infer_ctx = InferTypeContext {
                                    modules: Some(ctx.modules),
                                    current_module: ctx.current_module,
                                };
                                let subject_type = prop_access
                                    .subject
                                    .infer_type(infer_ctx)
                                    .normalize(norm_ctx);
                                let property_name =
                                    prop_access.property.slice().as_str().to_string();

                                match &subject_type {
                                    Type::Object { fields } | Type::Interface { fields, .. }
                                        if !fields.contains_key(&property_name) =>
                                    {
                                        report_error(BagelError {
                                            src: prop_access.property.slice().clone(),
                                            severity: RuleSeverity::Error,
                                            details: BagelErrorDetails::MiscError {
                                                message: format!(
                                                    "Property '{}' does not exist on type '{}'",
                                                    property_name, subject_type
                                                ),
                                            },
                                        });
                                    }
                                    Type::Unknown | Type::Any => {}
                                    Type::Object { .. } | Type::Interface { .. } => {}
                                    _ => {
                                        report_error(BagelError {
                                            src: prop_access.property.slice().clone(),
                                            severity: RuleSeverity::Error,
                                            details: BagelErrorDetails::MiscError {
                                                message: format!(
                                                    "Property '{}' does not exist on type '{}'",
                                                    property_name, subject_type
                                                ),
                                            },
                                        });
                                    }
                                }
                            }
                            _ => {}
                        }

                        // Generalized expected-type check: if this expression
                        // appears in a context that expects a specific type,
                        // verify that its inferred type fits.
                        let expr_node: AST<Expression> = match self {
                            AST::Valid(inner, _) => AST::<Expression>::new(inner.clone()),
                            AST::Malformed { inner, message } => {
                                AST::<Expression>::new_malformed(inner.clone(), message.clone())
                            }
                        };
                        if let Some(expected) = expr_node.expected_type() {
                            let infer_ctx = InferTypeContext {
                                modules: Some(ctx.modules),
                                current_module: ctx.current_module,
                            };
                            let inferred = expr_node.infer_type(infer_ctx);
                            let fits_ctx = FitsContext {
                                modules: Some(ctx.modules),
                            };
                            let issues = inferred.clone().fit_issues(expected.clone(), fits_ctx);
                            if !issues.is_empty() {
                                report_error(BagelError {
                                    src: self.slice().clone(),
                                    severity: RuleSeverity::Error,
                                    details: BagelErrorDetails::MiscError {
                                        message: issues.join("\n"),
                                    },
                                });
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

impl<T> Checkable for Option<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        self.iter().for_each(|x| x.check(ctx, report_error));
    }
}

impl<T> Checkable for Vec<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        self.iter().for_each(|x| x.check(ctx, report_error));
    }
}

impl<T, U> Checkable for (T, U)
where
    T: Checkable,
    U: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        self.0.check(ctx, report_error);
        self.1.check(ctx, report_error);
    }
}

impl<T, U, V> Checkable for (T, U, V)
where
    T: Checkable,
    U: Checkable,
    V: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        self.0.check(ctx, report_error);
        self.1.check(ctx, report_error);
        self.2.check(ctx, report_error);
    }
}

impl Checkable for Slice {
    fn check<'a, F: FnMut(BagelError)>(&self, _ctx: &CheckContext<'a>, _report_error: &mut F) {
        // Slices are just text ranges, nothing to check
    }
}

// impl<'a> CheckContext<'a> {
//     pub fn in_func(&self, func: AST<Func>) -> CheckContext<'a> {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: Some(FuncOrProc::Func(func)),
//             in_expression_context: self.in_expression_context,
//         }
//     }

//     pub fn in_proc(&self, proc: AST<Proc>) -> CheckContext<'a> {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: Some(FuncOrProc::Proc(proc)),
//             in_expression_context: self.in_expression_context,
//         }
//     }

//     pub fn in_expression_context(&self) -> Self {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: self.nearest_func_or_proc.clone(),
//             in_expression_context: true,
//         }
//     }

//     pub fn in_statement_context(&self) -> Self {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: self.nearest_func_or_proc.clone(),
//             in_expression_context: false,
//         }
//     }
// }
