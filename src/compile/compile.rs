use std::fmt::Write;

use crate::{
    ast::grammar::{ElseClause, IfElseExpression},
    ast::{container::AST, grammar::Any, modules::ModulesStore},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct CompileContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
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

                Any::Declaration(declaration) => match declaration {
                    Declaration::ConstDeclaration(decl) => {
                        // export? const identifier = value  ->  export? const identifier = value
                        if decl.export_keyword.is_some() {
                            write!(f, "export ")?;
                        }
                        write!(f, "const ")?;
                        decl.identifier.compile(ctx, f)?;
                        write!(f, " = ")?;
                        decl.value.compile(ctx, f)?;
                        Ok(())
                    }
                    Declaration::ImportDeclaration(decl) => {
                        // from 'path' import { foo, bar as other }
                        //   -> import { foo, bar as other } from 'path'
                        write!(f, "import {{ ")?;
                        for (i, specifier) in decl.imports.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            specifier.name.compile(ctx, f)?;
                            if let Some((_, alias)) = &specifier.alias {
                                write!(f, " as ")?;
                                alias.compile(ctx, f)?;
                            }
                        }
                        write!(f, " }} from ")?;
                        let path_contents = decl.path.unpack().contents;
                        let path_str = path_contents.as_str();
                        let compiled_path = path_str
                            .strip_suffix(".bgl")
                            .map(|stem| format!("{}.js", stem))
                            .unwrap_or_else(|| path_str.to_string());
                        write!(f, "'{}'", compiled_path)?;
                        Ok(())
                    }
                },

                Any::Expression(expression) => {
                    match expression {
                        Expression::NilLiteral(_) => {
                            // nil -> undefined
                            write!(f, "undefined")
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

                        Expression::StringLiteral(str_lit) => {
                            // 'string' -> 'string' (same in JS)
                            write!(f, "'")?;
                            write!(f, "{}", str_lit.contents.as_str())?;
                            write!(f, "'")
                        }

                        Expression::LocalIdentifier(local_id) => {
                            local_id.identifier.compile(ctx, f)
                        }

                        Expression::BinaryOperation(bin_op) => {
                            bin_op.left.compile(ctx, f)?;
                            write!(f, " ")?;
                            bin_op.operator.compile(ctx, f)?;
                            write!(f, " ")?;
                            bin_op.right.compile(ctx, f)
                        }

                        Expression::UnaryOperation(unary_op) => {
                            unary_op.operator.compile(ctx, f)?;
                            unary_op.operand.compile(ctx, f)
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
                            // (param1: T, param2: U) => body  ->  (param1, param2) => body (strip types for JS)
                            if func.parameters.len() == 1 && func.open_paren.is_none() {
                                // Single parameter without parens
                                func.parameters[0].0.compile(ctx, f)?;
                            } else {
                                // Multiple parameters or explicit parens
                                write!(f, "(")?;
                                for (i, (param_name, _type_ann)) in
                                    func.parameters.iter().enumerate()
                                {
                                    if i > 0 {
                                        write!(f, ", ")?;
                                    }
                                    param_name.compile(ctx, f)?;
                                }
                                write!(f, ")")?;
                            }

                            write!(f, " => ")?;
                            func.body.compile(ctx, f)
                        }

                        Expression::ArrayLiteral(arr) => {
                            // [1, 2, 3] -> [1, 2, 3] (same in JS)
                            write!(f, "[")?;
                            for (i, elem) in arr.elements.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                elem.compile(ctx, f)?;
                            }
                            write!(f, "]")
                        }

                        Expression::ObjectLiteral(obj) => {
                            // {a: 1, b: 2} -> {a: 1, b: 2} (same in JS)
                            write!(f, "{{")?;
                            for (i, (key, _, value)) in obj.fields.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                key.compile(ctx, f)?;
                                write!(f, ": ")?;
                                value.compile(ctx, f)?;
                            }
                            write!(f, "}}")
                        }

                        Expression::IfElseExpression(if_else) => {
                            // if cond { cons } else { alt } -> (cond ? cons : alt)
                            // if cond { cons } -> (cond ? cons : null)
                            // else if chains -> nested ternaries
                            write!(f, "(")?;
                            compile_if_else_ternary(if_else, ctx, f)?;
                            write!(f, ")")
                        }

                        Expression::ParenthesizedExpression(paren) => {
                            write!(f, "(")?;
                            paren.expression.compile(ctx, f)?;
                            write!(f, ")")
                        }

                        Expression::PropertyAccessExpression(prop_access) => {
                            // Strip `js.` — its members are JS globals
                            let is_js_global = matches!(
                                prop_access.subject.details(),
                                Some(Any::Expression(Expression::LocalIdentifier(id)))
                                    if id.identifier.slice().as_str() == "js"
                            );

                            if is_js_global {
                                prop_access.property.compile(ctx, f)
                            } else {
                                prop_access.subject.compile(ctx, f)?;
                                write!(f, ".")?;
                                prop_access.property.compile(ctx, f)
                            }
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

                Any::BinaryOperator(op) => match op {
                    BinaryOperator::Equal => write!(f, "==="),
                    BinaryOperator::NotEqual => write!(f, "!=="),
                    _ => write!(f, "{}", op.as_str()),
                },

                Any::UnaryOperator(op) => {
                    write!(f, "{}", op.as_str())
                }

                Any::FunctionBody(body) => match body {
                    FunctionBody::Expression(expr) => expr.compile(ctx, f),
                    FunctionBody::Block(block) => block.compile(ctx, f),
                },

                Any::Statement(statement) => match statement {
                    Statement::Invocation(inv) => {
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
                    Statement::Block(block) => {
                        write!(f, "{{ ");

                        for s in block.statements.iter() {
                            s.compile(ctx, f)?;
                            write!(f, "; ")?;
                        }

                        write!(f, "}}");

                        Ok(())
                    }
                },
            },
        }
    }
}

fn compile_if_else_ternary<W: Write>(
    if_else: &IfElseExpression,
    ctx: CompileContext,
    f: &mut W,
) -> core::fmt::Result {
    if_else.condition.compile(ctx, f)?;
    write!(f, " ? ")?;
    if_else.consequent.compile(ctx, f)?;
    write!(f, " : ")?;

    match &if_else.else_clause {
        Some(ElseClause::ElseBlock { expression, .. }) => expression.compile(ctx, f),
        Some(ElseClause::ElseIf {
            if_else: nested, ..
        }) => compile_if_else_ternary(&nested.unpack(), ctx, f),
        None => write!(f, "null"),
    }
}
