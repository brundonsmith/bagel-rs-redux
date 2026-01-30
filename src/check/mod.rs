use crate::{
    ast::{container::AST, grammar::Any, slice::Slice},
    config::{Config, RuleSeverity},
    types::{fits::FitsContext, infer::InferTypeContext, Type},
};

#[derive(Debug, Clone)]
pub struct CheckContext<'a> {
    pub config: &'a Config,
    // pub modules: &'a ModulesStore,
    // pub current_module: &'a ParsedModule,
    // pub nearest_func_or_proc: Option<FuncOrProc>,
    // pub in_expression_context: bool,
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
            Some(details) => match details {
                Any::Module(module) => {
                    // Recurse to all declarations
                    module.declarations.check(ctx, report_error);
                }

                Any::Declaration(declaration) => {
                    // Recurse to children
                    declaration.identifier.check(ctx, report_error);
                    declaration.type_annotation.check(ctx, report_error);
                    declaration.value.check(ctx, report_error);

                    // Check type compatibility if there's a type annotation
                    if let Some((_colon, type_expr)) = &declaration.type_annotation {
                        // Convert TypeExpression to Type
                        let declared_type = Type::from(type_expr.unpack());

                        // Infer the type of the value expression
                        let infer_ctx = InferTypeContext {};
                        let inferred_type = declaration.value.infer_type(infer_ctx);

                        // Check if the inferred type fits the declared type
                        let fits_ctx = FitsContext {};
                        let issues = inferred_type.fit_issues(declared_type, fits_ctx);
                        if issues.len() > 0 {
                            report_error(BagelError {
                                src: declaration.value.slice().clone(),
                                severity: RuleSeverity::Error,
                                details: BagelErrorDetails::MiscError {
                                    message: issues.join("\n"),
                                },
                            });
                        }
                    }
                }

                Any::Expression(expression) => {
                    use crate::ast::grammar::Expression::*;
                    match expression {
                        NilLiteral(_) | BooleanLiteral(_) | NumberLiteral(_) | StringLiteral(_) => {
                            // Leaf nodes, nothing to check
                        }
                        LocalIdentifier(local_id) => {
                            // Recurse to identifier
                            local_id.identifier.check(ctx, report_error);
                        }
                        BinaryOperation(bin_op) => {
                            // Recurse to children
                            bin_op.left.check(ctx, report_error);
                            bin_op.operator.check(ctx, report_error);
                            bin_op.right.check(ctx, report_error);
                        }
                        Invocation(inv) => {
                            // Recurse to children
                            inv.function.check(ctx, report_error);
                            inv.arguments.check(ctx, report_error);
                        }
                        FunctionExpression(func) => {
                            // Recurse to children
                            func.parameters.check(ctx, report_error);
                            func.body.check(ctx, report_error);
                        }
                        ArrayLiteral(arr) => {
                            // Recurse to elements
                            arr.elements.check(ctx, report_error);
                        }
                        ObjectLiteral(obj) => {
                            // Recurse to fields
                            obj.fields.check(ctx, report_error);
                        }
                    }
                }

                Any::TypeExpression(type_expression) => {
                    use crate::ast::grammar::TypeExpression::*;
                    match type_expression {
                        UnknownTypeExpression(_)
                        | NilTypeExpression(_)
                        | BooleanTypeExpression(_)
                        | NumberTypeExpression(_)
                        | StringTypeExpression(_) => {
                            // Leaf nodes, nothing to check
                        }
                        TupleTypeExpression(tuple) => {
                            // Recurse to elements
                            tuple.elements.check(ctx, report_error);
                        }
                        ArrayTypeExpression(array) => {
                            // Recurse to element type
                            array.element.check(ctx, report_error);
                        }
                        ObjectTypeExpression(obj) => {
                            // Recurse to field types
                            obj.fields.check(ctx, report_error);
                        }
                        FunctionTypeExpression(func) => {
                            // Recurse to parameter types and return type
                            func.parameters.check(ctx, report_error);
                            func.return_type.check(ctx, report_error);
                        }
                        UnionTypeExpression(union) => {
                            // Recurse to variants
                            union.variants.check(ctx, report_error);
                        }
                    }
                }

                Any::PlainIdentifier(_) => {
                    // Leaf node, nothing to check
                }

                Any::BinaryOperator(_) => {
                    // Leaf node, nothing to check
                }
            },
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
