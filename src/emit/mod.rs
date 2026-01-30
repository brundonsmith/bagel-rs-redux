use std::fmt::Write;

use crate::{
    ast::{container::AST, grammar::Any},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct EmitContext<'a> {
    pub config: &'a Config,
}

pub trait Emittable {
    /// Write the AST node back out to text as valid, formatted, fixed (if
    /// necessary, based on rules) Bagel code
    fn emit<W: Write>(&self, ctx: EmitContext, f: &mut W) -> core::fmt::Result;
}

impl<TKind> Emittable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn emit<W: Write>(&self, ctx: EmitContext, f: &mut W) -> core::fmt::Result {
        use crate::ast::grammar::*;

        match self.details() {
            // Malformed nodes - emit original text
            None => write!(f, "{}", self.slice().as_str()),

            // Process valid nodes
            Some(details) => match details {
            Any::Module(module) => {
                // Emit all declarations separated by newlines
                for (i, decl) in module.declarations.iter().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    decl.emit(ctx, f)?;
                }
                Ok(())
            }

            Any::Declaration(declaration) => {
                // const identifier: type = value
                write!(f, "const ")?;
                declaration.identifier.emit(ctx, f)?;

                // Emit type annotation if present
                if let Some((_, type_expr)) = &declaration.type_annotation {
                    write!(f, ": ")?;
                    type_expr.emit(ctx, f)?;
                }

                write!(f, " = ")?;
                declaration.value.emit(ctx, f)?;
                Ok(())
            }

            Any::Expression(expression) => {
                match expression {
                    Expression::NilLiteral(_) => write!(f, "nil"),

                    Expression::BooleanLiteral(lit) => {
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

                    Expression::LocalIdentifier(local_id) => local_id.identifier.emit(ctx, f),

                    Expression::BinaryOperation(bin_op) => {
                        bin_op.left.emit(ctx, f)?;
                        write!(f, " ")?;
                        bin_op.operator.emit(ctx, f)?;
                        write!(f, " ")?;
                        bin_op.right.emit(ctx, f)
                    }

                    Expression::Invocation(inv) => {
                        inv.function.emit(ctx, f)?;
                        write!(f, "(")?;

                        for (i, arg) in inv.arguments.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            arg.emit(ctx, f)?;
                        }

                        if inv.trailing_comma.is_some() {
                            write!(f, ",")?;
                        }

                        write!(f, ")")
                    }

                    Expression::FunctionExpression(func) => {
                        // (param1, param2) => body  or  param => body
                        if func.parameters.len() == 1 && func.open_paren.is_none() {
                            // Single parameter without parens
                            func.parameters[0].emit(ctx, f)?;
                        } else {
                            // Multiple parameters or explicit parens
                            write!(f, "(")?;
                            for (i, param) in func.parameters.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                param.emit(ctx, f)?;
                            }
                            if func.trailing_comma.is_some() {
                                write!(f, ",")?;
                            }
                            write!(f, ")")?;
                        }

                        write!(f, " => ")?;
                        func.body.emit(ctx, f)
                    }
                }
            }

            Any::TypeExpression(type_expression) => match type_expression {
                TypeExpression::UnknownTypeExpression(_) => write!(f, "unknown"),
                TypeExpression::NilTypeExpression(_) => write!(f, "nil"),
                TypeExpression::BooleanTypeExpression(_) => write!(f, "boolean"),
                TypeExpression::NumberTypeExpression(_) => write!(f, "number"),
                TypeExpression::StringTypeExpression(_) => write!(f, "string"),

                TypeExpression::TupleTypeExpression(tuple) => {
                    write!(f, "[")?;
                    for (i, elem) in tuple.elements.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        elem.emit(ctx, f)?;
                    }
                    if tuple.trailing_comma.is_some() {
                        write!(f, ",")?;
                    }
                    write!(f, "]")
                }

                TypeExpression::ArrayTypeExpression(array) => {
                    array.element.emit(ctx, f)?;
                    write!(f, "[]")
                }

                TypeExpression::ObjectTypeExpression(obj) => {
                    write!(f, "{{")?;
                    for (i, (name, _, type_expr)) in obj.fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        name.emit(ctx, f)?;
                        write!(f, ": ")?;
                        type_expr.emit(ctx, f)?;
                    }
                    if obj.trailing_comma.is_some() {
                        write!(f, ",")?;
                    }
                    write!(f, "}}")
                }

                TypeExpression::FunctionTypeExpression(func) => {
                    write!(f, "(")?;
                    for (i, (name, _, type_expr)) in func.parameters.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        name.emit(ctx, f)?;
                        write!(f, ": ")?;
                        type_expr.emit(ctx, f)?;
                    }
                    if func.trailing_comma.is_some() {
                        write!(f, ",")?;
                    }
                    write!(f, ") => ")?;
                    func.return_type.emit(ctx, f)
                }

                TypeExpression::UnionTypeExpression(union) => {
                    for (i, variant) in union.variants.iter().enumerate() {
                        if i > 0 {
                            write!(f, " | ")?;
                        }
                        variant.emit(ctx, f)?;
                    }
                    Ok(())
                }
            },

            Any::PlainIdentifier(_) => {
                // Use the original slice text
                write!(f, "{}", self.slice().as_str())
            }

            Any::BinaryOperator(op) => {
                write!(f, "{}", op.as_str())
            }
            },
        }
    }
}
