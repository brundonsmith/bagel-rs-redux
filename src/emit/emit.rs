use std::fmt::Write;
use std::sync::Arc;

use crate::{
    ast::container::{ASTInner, AST},
    ast::grammar::Any,
    ast::modules::ModulesStore,
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct EmitContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
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

                Any::Declaration(declaration) => match declaration {
                    Declaration::ConstDeclaration(decl) => {
                        // export? const identifier: type = value
                        if decl.export_keyword.is_some() {
                            write!(f, "export ")?;
                        }
                        write!(f, "const ")?;
                        decl.identifier.emit(ctx, f)?;

                        // Emit type annotation if present
                        if let Some((_, type_expr)) = &decl.type_annotation {
                            write!(f, ": ")?;
                            type_expr.emit(ctx, f)?;
                        }

                        write!(f, " = ")?;
                        decl.value.emit(ctx, f)?;
                        Ok(())
                    }
                    Declaration::ImportDeclaration(decl) => {
                        write!(f, "from ")?;
                        decl.path.emit(ctx, f)?;
                        write!(f, " import {{ ")?;
                        for (i, specifier) in decl.imports.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            specifier.name.emit(ctx, f)?;
                            if let Some((_, alias)) = &specifier.alias {
                                write!(f, " as ")?;
                                alias.emit(ctx, f)?;
                            }
                        }
                        if decl.trailing_comma.is_some() {
                            write!(f, ",")?;
                        }
                        write!(f, " }}")
                    }
                },

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

                        Expression::StringLiteral(str_lit) => {
                            write!(f, "'")?;
                            write!(f, "{}", str_lit.contents.as_str())?;
                            write!(f, "'")
                        }

                        Expression::LocalIdentifier(local_id) => local_id.identifier.emit(ctx, f),

                        Expression::BinaryOperation(bin_op) => {
                            bin_op.left.emit(ctx, f)?;
                            write!(f, " ")?;
                            bin_op.operator.emit(ctx, f)?;
                            write!(f, " ")?;
                            bin_op.right.emit(ctx, f)
                        }

                        Expression::UnaryOperation(unary_op) => {
                            unary_op.operator.emit(ctx, f)?;
                            unary_op.operand.emit(ctx, f)
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
                                // Single parameter without parens (no type annotation possible)
                                func.parameters[0].0.emit(ctx, f)?;
                            } else {
                                // Multiple parameters or explicit parens
                                write!(f, "(")?;
                                for (i, (param_name, type_ann)) in
                                    func.parameters.iter().enumerate()
                                {
                                    if i > 0 {
                                        write!(f, ", ")?;
                                    }
                                    param_name.emit(ctx, f)?;
                                    if let Some((_colon, type_expr)) = type_ann {
                                        write!(f, ": ")?;
                                        type_expr.emit(ctx, f)?;
                                    }
                                }
                                if func.trailing_comma.is_some() {
                                    write!(f, ",")?;
                                }
                                write!(f, ")")?;
                            }

                            if let Some((_colon, ret_type)) = &func.return_type {
                                write!(f, ": ")?;
                                ret_type.emit(ctx, f)?;
                            }

                            write!(f, " => ")?;
                            func.body.emit(ctx, f)
                        }

                        Expression::ArrayLiteral(arr) => {
                            write!(f, "[")?;
                            for (i, elem) in arr.elements.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                elem.emit(ctx, f)?;
                            }
                            if arr.trailing_comma.is_some() {
                                write!(f, ",")?;
                            }
                            write!(f, "]")
                        }

                        Expression::ObjectLiteral(obj) => {
                            write!(f, "{{ ")?;
                            for (i, (key, _, value)) in obj.fields.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                key.emit(ctx, f)?;
                                write!(f, ": ")?;
                                value.emit(ctx, f)?;
                            }
                            if obj.trailing_comma.is_some() {
                                write!(f, ",")?;
                            }
                            write!(f, " }}")
                        }

                        Expression::ParenthesizedExpression(paren) => {
                            write!(f, "(")?;
                            paren.expression.emit(ctx, f)?;
                            write!(f, ")")
                        }

                        Expression::PropertyAccessExpression(prop_access) => {
                            prop_access.subject.emit(ctx, f)?;
                            write!(f, ".")?;
                            prop_access.property.emit(ctx, f)
                        }

                        Expression::IfElseExpression(if_else) => {
                            write!(f, "if ")?;
                            if_else.condition.emit(ctx, f)?;
                            write!(f, " {{ ")?;
                            if_else.consequent.emit(ctx, f)?;
                            write!(f, " }}")?;

                            match &if_else.else_clause {
                                Some(ElseClause::ElseBlock { expression, .. }) => {
                                    write!(f, " else {{ ")?;
                                    expression.emit(ctx, f)?;
                                    write!(f, " }}")
                                }
                                Some(ElseClause::ElseIf {
                                    if_else: nested, ..
                                }) => {
                                    write!(f, " else ")?;
                                    nested.emit(ctx, f)
                                }
                                None => Ok(()),
                            }
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
                        write!(f, "{{ ")?;
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
                        write!(f, " }}")
                    }

                    TypeExpression::FunctionTypeExpression(func) => {
                        write!(f, "(")?;
                        for (i, (name_colon, type_expr)) in func.parameters.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            if let Some((name, _)) = name_colon {
                                name.emit(ctx, f)?;
                                write!(f, ": ")?;
                            }
                            type_expr.emit(ctx, f)?;
                        }
                        if func.trailing_comma.is_some() {
                            write!(f, ",")?;
                        }
                        write!(f, ") => ")?;
                        func.return_type.emit(ctx, f)
                    }

                    TypeExpression::RangeTypeExpression(range) => {
                        if let Some(start) = &range.start {
                            write!(f, "{}", start.as_str())?;
                        }
                        write!(f, "..")?;
                        if let Some(end) = &range.end {
                            write!(f, "{}", end.as_str())?;
                        }
                        Ok(())
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

                    TypeExpression::ParenthesizedTypeExpression(paren) => {
                        write!(f, "(")?;
                        paren.expression.emit(ctx, f)?;
                        write!(f, ")")
                    }

                    TypeExpression::TypeOfTypeExpression(type_of) => {
                        write!(f, "typeof ")?;
                        type_of.expression.emit(ctx, f)
                    }
                },

                Any::PlainIdentifier(_) => {
                    // Use the original slice text
                    write!(f, "{}", self.slice().as_str())
                }

                Any::BinaryOperator(op) => {
                    write!(f, "{}", op.as_str())
                }

                Any::UnaryOperator(op) => {
                    write!(f, "{}", op.as_str())
                }

                Any::FunctionBody(body) => match body {
                    FunctionBody::Expression(expr) => expr.emit(ctx, f),
                    FunctionBody::Block(block) => block.emit(ctx, f),
                },

                Any::Statement(statement) => match statement {
                    Statement::Expression(expr) => {
                        AST::<Any>::new(Arc::new(ASTInner {
                            parent: Default::default(),
                            slice: self.slice().clone(),
                            details: Any::Expression(expr.clone()),
                        }))
                        .emit(ctx, f)
                    }
                    Statement::Block(block) => {
                        writeln!(f, "{{")?;
                        for stmt in &block.statements {
                            stmt.emit(ctx, f)?;
                            write!(f, "\n")?;
                        }
                        write!(f, "}}")
                    }
                },
            },
        }
    }
}
