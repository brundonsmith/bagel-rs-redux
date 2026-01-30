use std::fmt::Write;

use crate::{
    ast::{container::AST, grammar::Any},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct CompileContext<'a> {
    pub config: &'a Config,
    // pub modules: &'a ModulesStore,
    // pub current_module: &'a ParsedModule,
    // pub include_types: bool,
    // pub qualify_identifiers_with: Option<&'a HashMap<ModuleID, usize>>,
    // pub qualify_all_identifiers: bool,
}

pub trait Compilable {
    /// Given some Bagel AST node, compile it into JavaScript code
    ///
    /// Each implementation is responsible for compiling its own child nodes
    /// (as appropriate, in context)
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> core::fmt::Result;
}

impl<TKind> Compilable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> core::fmt::Result {
        use crate::ast::grammar::*;

        match self.details() {
            // Malformed nodes - emit original text as best effort
            None => write!(f, "{}", self.slice().as_str()),

            // Process valid nodes
            Some(details) => match details {
            Any::Module(module) => {
                // Compile all declarations separated by semicolons and newlines
                for (i, decl) in module.declarations.iter().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    decl.compile(ctx, f)?;
                    write!(f, ";")?;
                }
                Ok(())
            }

            Any::Declaration(declaration) => {
                // const identifier = value  ->  const identifier = value
                write!(f, "const ")?;
                declaration.identifier.compile(ctx, f)?;
                write!(f, " = ")?;
                declaration.value.compile(ctx, f)?;
                Ok(())
            }

            Any::Expression(expression) => {
                match expression {
                    Expression::NilLiteral(_) => {
                        // nil -> null
                        write!(f, "null")
                    }

                    Expression::BooleanLiteral(lit) => {
                        // true/false -> true/false (same in JS)
                        if lit.value {
                            write!(f, "true")
                        } else {
                            write!(f, "false")
                        }
                    }

                    Expression::NumberLiteral(_) => {
                        // Use the original slice text to preserve exact formatting
                        write!(f, "{}", self.slice().as_str())
                    }

                    Expression::LocalIdentifier(local_id) => local_id.identifier.compile(ctx, f),

                    Expression::BinaryOperation(bin_op) => {
                        // Emit with parentheses to preserve precedence
                        bin_op.left.compile(ctx, f)?;
                        write!(f, " ")?;
                        bin_op.operator.compile(ctx, f)?;
                        write!(f, " ")?;
                        bin_op.right.compile(ctx, f)
                    }

                    Expression::Invocation(inv) => {
                        // function(arg1, arg2) -> function(arg1, arg2) (same in JS)
                        inv.function.compile(ctx, f)?;
                        write!(f, "(")?;

                        for (i, arg) in inv.arguments.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            arg.compile(ctx, f)?;
                        }

                        write!(f, ")")
                    }

                    Expression::FunctionExpression(func) => {
                        // (param1, param2) => body  ->  (param1, param2) => body (same in JS)
                        if func.parameters.len() == 1 && func.open_paren.is_none() {
                            // Single parameter without parens
                            func.parameters[0].compile(ctx, f)?;
                        } else {
                            // Multiple parameters or explicit parens
                            write!(f, "(")?;
                            for (i, param) in func.parameters.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                param.compile(ctx, f)?;
                            }
                            write!(f, ")")?;
                        }

                        write!(f, " => ")?;
                        func.body.compile(ctx, f)
                    }
                }
            }

            Any::TypeExpression(_type_expression) => {
                // Type expressions are not emitted in JavaScript compilation
                // (JavaScript is untyped at runtime)
                Ok(())
            }

            Any::PlainIdentifier(_) => {
                // Use the original slice text
                write!(f, "{}", self.slice().as_str())
            }

            Any::BinaryOperator(op) => {
                // +, -, *, / are the same in JavaScript
                write!(f, "{}", op.as_str())
            }
            },
        }
    }
}
