use std::fmt::Write;

use crate::{
    ast::{
        container::AST,
        grammar::{Any, ElseClause, Expression, IfElseExpression},
        modules::{Module, ModulesStore},
    },
    config::Config,
    types::normalize::{resolve_identifier, NormalizeContext, ResolvedIdentifier},
};

#[derive(Debug, Clone, Copy)]
pub struct CompileContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
    pub prefix_identifiers_with_module_ids: bool,
    pub current_module: Option<&'a Module>,
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
        match self.details() {
            // Malformed nodes - emit original text as best effort
            None => Ok(()),

            // Intercept LocalIdentifier when bundling to resolve prefixes with scope awareness
            Some(Any::Expression(Expression::LocalIdentifier(local_id)))
                if ctx.prefix_identifiers_with_module_ids =>
            {
                let name = local_id.slice.as_str();
                let node: AST<Any> = match self {
                    AST::Valid(inner, _) => {
                        AST::<Any>::Valid(inner.clone(), std::marker::PhantomData)
                    }
                    AST::Malformed(slice, message) => {
                        AST::<Any>::Malformed(slice.clone(), message.clone())
                    }
                };
                let norm_ctx = NormalizeContext {
                    modules: Some(ctx.modules),
                    current_module: ctx.current_module,
                    param_type_overrides: None,
                };

                match resolve_identifier(name, &node, norm_ctx) {
                    Some(ResolvedIdentifier::FunctionParam { .. }) => {
                        write!(f, "{}", name)
                    }
                    Some(ResolvedIdentifier::ConstDeclaration {
                        module: Some(source_module),
                        ..
                    }) => {
                        let source_id = ctx.modules.module_id(&source_module.path).unwrap_or(0);
                        write!(f, "module_{}_{}", source_id, name)
                    }
                    Some(ResolvedIdentifier::ConstDeclaration { module: None, .. }) => {
                        let module_id = ctx
                            .current_module
                            .and_then(|m| ctx.modules.module_id(&m.path))
                            .unwrap_or(0);
                        write!(f, "module_{}_{}", module_id, name)
                    }
                    Some(ResolvedIdentifier::TypeDeclaration { .. }) => {
                        // Type declarations are erased; emit the name as-is
                        write!(f, "{}", name)
                    }
                    None => write!(f, "{}", name),
                }
            }

            // Process valid nodes
            Some(details) => details.compile(ctx, f),
        }
    }
}

impl Compilable for Any {
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> core::fmt::Result {
        use crate::ast::grammar::*;

        match self {
            Any::Module(module) => {
                // Compile all declarations separated by semicolons and newlines
                // (skip type declarations since they're erased at runtime)
                let mut first = true;
                for decl in &module.declarations {
                    let is_type_decl =
                        matches!(decl.unpack(), Some(Declaration::TypeDeclaration(_)));
                    if is_type_decl {
                        continue;
                    }
                    if !first {
                        writeln!(f)?;
                    }
                    first = false;
                    decl.compile(ctx, f)?;
                    write!(f, ";")?;
                }
                Ok(())
            }

            Any::Declaration(declaration) => match declaration {
                Declaration::ConstDeclaration(decl) => {
                    if ctx.prefix_identifiers_with_module_ids {
                        // In bundle mode: strip export, prefix identifier with module ID
                        let module_id = ctx
                            .current_module
                            .and_then(|m| ctx.modules.module_id(&m.path))
                            .unwrap_or(0);
                        write!(f, "const module_{}_", module_id)?;
                        decl.identifier.compile(ctx, f)?;
                        write!(f, " = ")?;
                        decl.value.compile(ctx, f)?;
                    } else {
                        // export? const identifier = value  ->  export? const identifier = value
                        if decl.export_keyword.is_some() {
                            write!(f, "export ")?;
                        }
                        write!(f, "const ")?;
                        decl.identifier.compile(ctx, f)?;
                        write!(f, " = ")?;
                        decl.value.compile(ctx, f)?;
                    }
                    Ok(())
                }
                Declaration::TypeDeclaration(_) => {
                    // Type declarations are erased at runtime
                    Ok(())
                }
                Declaration::ImportDeclaration(decl) => {
                    if ctx.prefix_identifiers_with_module_ids {
                        // In bundle mode: resolve source module and emit aliased bindings
                        let current_id = ctx
                            .current_module
                            .and_then(|m| ctx.modules.module_id(&m.path))
                            .unwrap();
                        let source_id = decl.path.unpack().and_then(|path_lit| {
                            ctx.current_module.and_then(|current| {
                                ctx.modules
                                    .find_imported(current, path_lit.contents.as_str())
                                    .and_then(|source| ctx.modules.module_id(&source.path))
                            })
                        });

                        // Only emit bindings for aliased imports;``
                        // non-aliased imports are handled at the LocalIdentifier level``
                        let aliased_specs: Vec<_> = decl
                            .imports
                            .iter()
                            .filter_map(|spec| {
                                spec.alias
                                    .as_ref()
                                    .map(|(_, alias)| (spec.name.unpack(), alias.unpack()))
                            })
                            .collect();

                        for (i, (original_name, alias_name)) in aliased_specs.iter().enumerate() {
                            if i > 0 {
                                write!(f, "\n")?;
                            }
                            match (original_name, alias_name) {
                                (Some(orig), Some(alias)) => {
                                    write!(
                                        f,
                                        "const module_{}_{} = module_",
                                        current_id,
                                        alias.slice.as_str(),
                                    )?;

                                    if let Some(source_id) = source_id {
                                        write!(f, "{}", source_id)?;
                                    } else {
                                        write!(f, "unknown")?;
                                    }

                                    write!(f, "_{}", orig.slice.as_str())?;
                                }
                                _ => {}
                            }
                        }
                        Ok(())
                    } else {
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
                        match decl.path.unpack() {
                            Some(path_lit) => {
                                let path_str = path_lit.contents.as_str();
                                let compiled_path = path_str
                                    .strip_suffix(".bgl")
                                    .map(|stem| format!("{}.js", stem))
                                    .unwrap_or_else(|| path_str.to_string());
                                write!(f, "'{}'", compiled_path)?;
                            }
                            None => {
                                write!(f, "{}", decl.path.slice().as_str())?;
                            }
                        }
                        Ok(())
                    }
                }
            },

            Any::Expression(expression) => match expression {
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

                Expression::NumberLiteral(num) => {
                    // Use the original slice text to preserve exact formatting
                    write!(f, "{}", num.slice.as_str())
                }

                Expression::StringLiteral(str_lit) => {
                    // 'string' -> 'string' (same in JS)
                    write!(f, "'")?;
                    write!(f, "{}", str_lit.contents.as_str())?;
                    write!(f, "'")
                }

                Expression::LocalIdentifier(local_id) => {
                    write!(f, "{}", local_id.slice.as_str())
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
                        for (i, (param_name, _type_ann)) in func.parameters.iter().enumerate() {
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

                Expression::PipeCallExpression(pipe) => {
                    // Desugar: subject..function(args) -> function(subject, args)
                    // Partial nodes (no function name) compile to nothing useful,
                    // but we still emit the subject for best-effort output.
                    match &pipe.function {
                        Some(func) => {
                            func.compile(ctx, f)?;
                            write!(f, "(")?;
                            pipe.subject.compile(ctx, f)?;
                            for arg in &pipe.arguments {
                                write!(f, ", ")?;
                                arg.compile(ctx, f)?;
                            }
                            write!(f, ")")
                        }
                        None => pipe.subject.compile(ctx, f),
                    }
                }

                Expression::PropertyAccessExpression(prop_access) => {
                    // Strip `js.` — its members are JS globals
                    let is_js_global = matches!(
                        prop_access.subject.details(),
                        Some(Any::Expression(Expression::LocalIdentifier(id)))
                            if id.slice.as_str() == "js"
                    );

                    if is_js_global {
                        prop_access.property.compile(ctx, f)
                    } else {
                        prop_access.subject.compile(ctx, f)?;
                        write!(f, ".")?;
                        prop_access.property.compile(ctx, f)
                    }
                }
            },

            Any::TypeExpression(_type_expression) => {
                // Type expressions are not emitted in JavaScript compilation
                // (JavaScript is untyped at runtime)
                Ok(())
            }

            Any::PlainIdentifier(id) => {
                // Use the original slice text
                write!(f, "{}", id.slice.as_str())
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
                Statement::Expression(expr) => Any::Expression(expr.clone()).compile(ctx, f),
                Statement::Block(block) => {
                    write!(f, "{{ ")?;

                    for s in block.statements.iter() {
                        s.compile(ctx, f)?;
                        write!(f, "; ")?;
                    }

                    write!(f, "}}")?;

                    Ok(())
                }
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
        }) => match nested.unpack() {
            Some(nested_data) => compile_if_else_ternary(&nested_data, ctx, f),
            None => write!(f, "{}", nested.slice().as_str()),
        },
        None => write!(f, "null"),
    }
}
