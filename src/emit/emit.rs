use std::fmt::Write;

use crate::{
    ast::{
        container::AST,
        grammar::{
            Any, BinaryOperation, Declaration, ElseClause, Expression, FunctionBody, Statement,
            TypeExpression, UnaryOperation,
        },
        modules::ModulesStore,
        slice::Slice,
    },
    config::Config,
    parse::{whitespace_or_comments, Dressing},
};

#[derive(Debug, Clone, Copy)]
pub struct EmitContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
    pub current_indentation: usize,
}

impl<'a> EmitContext<'a> {
    pub fn indented(self) -> Self {
        Self {
            config: self.config,
            modules: self.modules,
            current_indentation: self.current_indentation + 1,
        }
    }

    pub fn indent_str(self) -> String {
        " ".repeat(self.current_indentation * self.config.rules.indentation.spaces as usize)
    }
}

fn emit_indent<W: Write>(ctx: EmitContext, f: &mut W) -> core::fmt::Result {
    write!(f, "{}", ctx.indent_str())
}

fn emit_dressing<W: Write>(
    dressing: &[Dressing],
    ctx: EmitContext,
    f: &mut W,
) -> core::fmt::Result {
    let indent_str = ctx.indent_str();
    let mut prev_was_blank_line = false;
    let mut prev_was_newline_terminated = false;
    dressing.iter().try_for_each(|item| match item {
        Dressing::BlankLine => {
            // Collapse consecutive blank lines into one
            if prev_was_blank_line {
                return Ok(());
            }
            prev_was_blank_line = true;
            // If previous item already ended with a newline, we only need one more
            // newline to create the blank line gap
            if prev_was_newline_terminated {
                write!(f, "\n")?;
            } else {
                write!(f, "\n\n")?;
            }
            prev_was_newline_terminated = true;
            Ok(())
        }
        Dressing::InlineComment(content) => {
            prev_was_blank_line = false;
            prev_was_newline_terminated = true;
            write!(f, " //{}\n", content.as_str())
        }
        Dressing::BlockComment(lines) => {
            prev_was_blank_line = false;
            prev_was_newline_terminated = true;
            let max_width = (ctx.config.rules.line_width.width as usize)
                .saturating_sub(
                    ctx.current_indentation * ctx.config.rules.indentation.spaces as usize,
                )
                .saturating_sub(3);
            let all_lines: Vec<&str> = lines
                .iter()
                .flat_map(|line| line.as_str().split('\n'))
                .map(|l| l.trim())
                .collect();
            let trimmed_lines = all_lines
                .iter()
                .copied()
                .skip_while(|l| l.is_empty())
                .collect::<Vec<_>>();
            let end = trimmed_lines
                .iter()
                .rposition(|l| !l.is_empty())
                .map(|i| i + 1)
                .unwrap_or(0);
            trimmed_lines[..end].iter().try_for_each(|&text| {
                if text.is_empty() {
                    writeln!(f, "{}//", indent_str)
                } else if text.len() <= max_width {
                    writeln!(f, "{}// {}", indent_str, text)
                } else {
                    // Word-wrap long comment lines
                    let words = text.split_whitespace().collect::<Vec<_>>();
                    let mut current_line = String::new();
                    words.iter().try_for_each(|word| {
                        if current_line.is_empty() {
                            current_line.push_str(word);
                            Ok(())
                        } else if current_line.len() + 1 + word.len() <= max_width {
                            current_line.push(' ');
                            current_line.push_str(word);
                            Ok(())
                        } else {
                            let result = writeln!(f, "{}// {}", indent_str, current_line);
                            current_line.clear();
                            current_line.push_str(word);
                            result
                        }
                    })?;
                    if !current_line.is_empty() {
                        writeln!(f, "{}// {}", indent_str, current_line)?;
                    }
                    Ok(())
                }
            })
        }
    })
}

/// Emit the gap between two module-level declarations.
///
/// Block comments between declarations are always preceded by a blank line
/// (to visually separate them from the code above).
fn emit_declaration_gap<W: Write>(
    prev: &Slice,
    next: &Slice,
    ctx: EmitContext,
    f: &mut W,
) -> core::fmt::Result {
    let gap = prev.gap_between(next);
    let dressing = (gap.len() > 0)
        .then(|| whitespace_or_comments(gap).ok().map(|(_, d)| d))
        .flatten()
        .filter(|d| !d.is_empty());

    match dressing {
        Some(ref items) if matches!(items.first(), Some(Dressing::BlockComment(_))) => {
            // Ensure blank line before a block comment
            write!(f, "\n\n")?;
            emit_dressing(items, ctx, f)
        }
        Some(ref items) => emit_dressing(items, ctx, f),
        None => write!(f, "\n"),
    }
}

fn emit_gap<W: Write>(
    prev: &Slice,
    next: &Slice,
    default: &str,
    ctx: EmitContext,
    f: &mut W,
) -> core::fmt::Result {
    let gap = prev.gap_between(next);
    if gap.len() == 0 {
        return write!(f, "{}", default);
    }
    match whitespace_or_comments(gap) {
        Ok((_, dressing)) if !dressing.is_empty() => emit_dressing(&dressing, ctx, f),
        _ => write!(f, "{}", default),
    }
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
        match self.details() {
            // Malformed nodes - emit original text
            None => write!(f, "{}", self.slice().as_str()),

            // For Module nodes, emit leading dressing before the first declaration,
            // then emit all formatted declarations. If the module didn't parse to
            // the end of the source, append the remaining unparsed source as-is.
            Some(Any::Module(module)) => {
                let slice = self.slice();

                if let Some(first_decl) = module.declarations.first() {
                    let leading_edge = module_leading_edge(&slice);
                    let gap = leading_edge.gap_between(first_decl.slice());
                    if gap.len() > 0 {
                        if let Ok((_, dressing)) = whitespace_or_comments(gap) {
                            if !dressing.is_empty() {
                                emit_dressing(&dressing, ctx, f)?;
                            }
                        }
                    }
                }
                Any::Module(module.clone()).emit(ctx, f)?;

                // Append any unparsed remainder of the source as-is
                let remaining_source = &slice.full_string[slice.end..];
                if !remaining_source.trim().is_empty() {
                    write!(f, "\n{}", remaining_source.trim())?;
                }
                Ok(())
            }

            // Process valid nodes
            Some(details) => details.emit(ctx, f),
        }
    }
}

/// Returns the "leading" slice of a module — a zero-width slice at the very
/// start of the module's own slice, so we can `emit_gap` from it to the first
/// declaration and pick up any comments/blank-lines that precede code.
fn module_leading_edge(module_slice: &Slice) -> Slice {
    module_slice.clone().slice_range(0, Some(0))
}

impl Emittable for Any {
    fn emit<W: Write>(&self, ctx: EmitContext, f: &mut W) -> core::fmt::Result {
        match self {
            Any::Module(module) => {
                module
                    .declarations
                    .iter()
                    .enumerate()
                    .try_for_each(|(i, decl)| {
                        if i > 0 {
                            emit_declaration_gap(
                                module.declarations[i - 1].slice(),
                                decl.slice(),
                                ctx,
                                f,
                            )?;
                        }
                        decl.emit(ctx, f)
                    })
            }

            Any::Declaration(declaration) => match declaration {
                Declaration::ConstDeclaration(decl) => {
                    // export? const identifier: type = value
                    if let Some(export_kw) = &decl.export_keyword {
                        write!(f, "export")?;
                        emit_gap(export_kw, &decl.const_keyword, " ", ctx, f)?;
                    }
                    write!(f, "const")?;
                    emit_gap(&decl.const_keyword, decl.identifier.slice(), " ", ctx, f)?;
                    decl.identifier.emit(ctx, f)?;

                    // Emit type annotation if present
                    if let Some((colon, type_expr)) = &decl.type_annotation {
                        emit_gap(decl.identifier.slice(), colon, "", ctx, f)?;
                        write!(f, ":")?;
                        emit_gap(colon, type_expr.slice(), " ", ctx, f)?;
                        type_expr.emit(ctx, f)?;
                        emit_gap(type_expr.slice(), &decl.equals, " ", ctx, f)?;
                    } else {
                        emit_gap(decl.identifier.slice(), &decl.equals, " ", ctx, f)?;
                    }

                    write!(f, "=")?;
                    emit_gap(&decl.equals, decl.value.slice(), " ", ctx, f)?;
                    decl.value.emit(ctx, f)
                }
                Declaration::TypeDeclaration(decl) => {
                    if let Some(export_kw) = &decl.export_keyword {
                        write!(f, "export")?;
                        emit_gap(export_kw, &decl.type_keyword, " ", ctx, f)?;
                    }
                    write!(f, "type")?;
                    emit_gap(&decl.type_keyword, decl.identifier.slice(), " ", ctx, f)?;
                    decl.identifier.emit(ctx, f)?;
                    emit_gap(decl.identifier.slice(), &decl.equals, " ", ctx, f)?;
                    write!(f, "=")?;
                    emit_gap(&decl.equals, decl.value.slice(), " ", ctx, f)?;
                    decl.value.emit(ctx, f)
                }
                Declaration::ImportDeclaration(decl) => {
                    write!(f, "from")?;
                    emit_gap(&decl.from_keyword, decl.path.slice(), " ", ctx, f)?;
                    decl.path.emit(ctx, f)?;
                    emit_gap(decl.path.slice(), &decl.import_keyword, " ", ctx, f)?;
                    write!(f, "import")?;
                    emit_gap(&decl.import_keyword, &decl.open_brace, " ", ctx, f)?;
                    write!(f, "{{")?;
                    for (i, specifier) in decl.imports.iter().enumerate() {
                        if i > 0 {
                            let comma = &decl.commas[i - 1];
                            emit_gap(
                                decl.imports[i - 1]
                                    .alias
                                    .as_ref()
                                    .map(|(_, a)| a.slice())
                                    .unwrap_or(decl.imports[i - 1].name.slice()),
                                comma,
                                "",
                                ctx,
                                f,
                            )?;
                            write!(f, ",")?;
                            emit_gap(comma, specifier.name.slice(), " ", ctx, f)?;
                        } else {
                            emit_gap(&decl.open_brace, specifier.name.slice(), " ", ctx, f)?;
                        }
                        specifier.name.emit(ctx, f)?;
                        if let Some((as_kw, alias)) = &specifier.alias {
                            emit_gap(specifier.name.slice(), as_kw, " ", ctx, f)?;
                            write!(f, "as")?;
                            emit_gap(as_kw, alias.slice(), " ", ctx, f)?;
                            alias.emit(ctx, f)?;
                        }
                    }
                    if let Some(trailing) = &decl.trailing_comma {
                        write!(f, ",")?;
                        if let Some(close) = &decl.close_brace {
                            emit_gap(trailing, close, " ", ctx, f)?;
                        }
                    } else if let Some(close) = &decl.close_brace {
                        let last_end = decl
                            .imports
                            .last()
                            .map(|s| {
                                s.alias
                                    .as_ref()
                                    .map(|(_, a)| a.slice())
                                    .unwrap_or(s.name.slice())
                            })
                            .unwrap_or(&decl.open_brace);
                        emit_gap(last_end, close, " ", ctx, f)?;
                    }
                    write!(f, "}}")
                }
            },

            Any::Expression(expression) => match expression {
                Expression::NilLiteral(_) => write!(f, "nil"),

                Expression::BooleanLiteral(lit) => {
                    if lit.value {
                        write!(f, "true")
                    } else {
                        write!(f, "false")
                    }
                }

                Expression::NumberLiteral(num) => {
                    write!(f, "{}", num.slice.as_str())
                }

                Expression::StringLiteral(str_lit) => {
                    write!(f, "'")?;
                    write!(f, "{}", str_lit.contents.as_str())?;
                    write!(f, "'")
                }

                Expression::LocalIdentifier(local_id) => {
                    write!(f, "{}", local_id.slice.as_str())
                }

                Expression::BinaryOperation(bin_op) => {
                    bin_op.left.emit(ctx, f)?;
                    emit_gap(bin_op.left.slice(), bin_op.operator.slice(), " ", ctx, f)?;
                    bin_op.operator.emit(ctx, f)?;
                    emit_gap(bin_op.operator.slice(), bin_op.right.slice(), " ", ctx, f)?;
                    bin_op.right.emit(ctx, f)
                }

                Expression::UnaryOperation(unary_op) => {
                    unary_op.operator.emit(ctx, f)?;
                    emit_gap(
                        unary_op.operator.slice(),
                        unary_op.operand.slice(),
                        "",
                        ctx,
                        f,
                    )?;
                    unary_op.operand.emit(ctx, f)
                }

                Expression::Invocation(inv) => {
                    inv.function.emit(ctx, f)?;
                    emit_gap(inv.function.slice(), &inv.open_paren, "", ctx, f)?;
                    write!(f, "(")?;

                    for (i, arg) in inv.arguments.iter().enumerate() {
                        if i > 0 {
                            let comma = &inv.commas[i - 1];
                            emit_gap(inv.arguments[i - 1].slice(), comma, "", ctx, f)?;
                            write!(f, ",")?;
                            emit_gap(comma, arg.slice(), " ", ctx, f)?;
                        } else {
                            emit_gap(&inv.open_paren, arg.slice(), "", ctx, f)?;
                        }
                        arg.emit(ctx, f)?;
                    }

                    if let Some(trailing) = &inv.trailing_comma {
                        write!(f, ",")?;
                        if let Some(close) = &inv.close_paren {
                            emit_gap(trailing, close, "", ctx, f)?;
                        }
                    } else if let Some(close) = &inv.close_paren {
                        let last_end = inv
                            .arguments
                            .last()
                            .map(|a| a.slice())
                            .unwrap_or(&inv.open_paren);
                        emit_gap(last_end, close, "", ctx, f)?;
                    }

                    write!(f, ")")
                }

                Expression::FunctionExpression(func) => {
                    // (param1, param2) => body  or  param => body
                    if func.parameters.len() == 1 && func.open_paren.is_none() {
                        // Single parameter without parens (no type annotation possible)
                        func.parameters[0].0.emit(ctx, f)?;
                    } else {
                        write!(f, "(")?;
                        let open_paren = func.open_paren.as_ref().unwrap();
                        for (i, (param_name, type_ann)) in func.parameters.iter().enumerate() {
                            if i > 0 {
                                let comma = &func.commas[i - 1];
                                let prev_end = func.parameters[i - 1]
                                    .1
                                    .as_ref()
                                    .map(|(_, t)| t.slice())
                                    .unwrap_or(func.parameters[i - 1].0.slice());
                                emit_gap(prev_end, comma, "", ctx, f)?;
                                write!(f, ",")?;
                                emit_gap(comma, param_name.slice(), " ", ctx, f)?;
                            } else {
                                emit_gap(open_paren, param_name.slice(), "", ctx, f)?;
                            }
                            param_name.emit(ctx, f)?;
                            if let Some((colon, type_expr)) = type_ann {
                                emit_gap(param_name.slice(), colon, "", ctx, f)?;
                                write!(f, ":")?;
                                emit_gap(colon, type_expr.slice(), " ", ctx, f)?;
                                type_expr.emit(ctx, f)?;
                            }
                        }
                        if func.trailing_comma.is_some() {
                            write!(f, ",")?;
                        }
                        write!(f, ")")?;
                    }

                    let close_or_last = func
                        .close_paren
                        .as_ref()
                        .or(func.parameters.last().map(|(p, _)| p.slice()));

                    if let Some((colon, ret_type)) = &func.return_type {
                        if let Some(prev) = close_or_last {
                            emit_gap(prev, colon, "", ctx, f)?;
                        }
                        write!(f, ":")?;
                        emit_gap(colon, ret_type.slice(), " ", ctx, f)?;
                        ret_type.emit(ctx, f)?;
                        emit_gap(ret_type.slice(), &func.arrow, " ", ctx, f)?;
                    } else if let Some(prev) = close_or_last {
                        emit_gap(prev, &func.arrow, " ", ctx, f)?;
                    }

                    write!(f, "=>")?;
                    emit_gap(&func.arrow, func.body.slice(), " ", ctx, f)?;
                    func.body.emit(ctx, f)
                }

                Expression::ArrayLiteral(arr) => {
                    write!(f, "[")?;
                    for (i, elem) in arr.elements.iter().enumerate() {
                        if i > 0 {
                            let comma = &arr.commas[i - 1];
                            emit_gap(arr.elements[i - 1].slice(), comma, "", ctx, f)?;
                            write!(f, ",")?;
                            emit_gap(comma, elem.slice(), " ", ctx, f)?;
                        } else {
                            emit_gap(&arr.open_bracket, elem.slice(), "", ctx, f)?;
                        }
                        elem.emit(ctx, f)?;
                    }
                    if let Some(trailing) = &arr.trailing_comma {
                        write!(f, ",")?;
                        if let Some(close) = &arr.close_bracket {
                            emit_gap(trailing, close, "", ctx, f)?;
                        }
                    } else if let Some(close) = &arr.close_bracket {
                        let last_end = arr
                            .elements
                            .last()
                            .map(|e| e.slice())
                            .unwrap_or(&arr.open_bracket);
                        emit_gap(last_end, close, "", ctx, f)?;
                    }
                    write!(f, "]")
                }

                Expression::ObjectLiteral(obj) => {
                    write!(f, "{{")?;
                    for (i, (key, colon, value)) in obj.fields.iter().enumerate() {
                        if i > 0 {
                            let comma = &obj.commas[i - 1];
                            let (_, _, prev_value) = &obj.fields[i - 1];
                            emit_gap(prev_value.slice(), comma, "", ctx, f)?;
                            write!(f, ",")?;
                            emit_gap(comma, key.slice(), " ", ctx, f)?;
                        } else {
                            emit_gap(&obj.open_brace, key.slice(), " ", ctx, f)?;
                        }
                        key.emit(ctx, f)?;
                        emit_gap(key.slice(), colon, "", ctx, f)?;
                        write!(f, ":")?;
                        emit_gap(colon, value.slice(), " ", ctx, f)?;
                        value.emit(ctx, f)?;
                    }
                    if let Some(trailing) = &obj.trailing_comma {
                        write!(f, ",")?;
                        if let Some(close) = &obj.close_brace {
                            emit_gap(trailing, close, " ", ctx, f)?;
                        }
                    } else if let Some(close) = &obj.close_brace {
                        let last_end = obj
                            .fields
                            .last()
                            .map(|(_, _, v)| v.slice())
                            .unwrap_or(&obj.open_brace);
                        emit_gap(last_end, close, " ", ctx, f)?;
                    }
                    write!(f, "}}")
                }

                Expression::ParenthesizedExpression(paren) => {
                    write!(f, "(")?;
                    emit_gap(&paren.open_paren, paren.expression.slice(), "", ctx, f)?;
                    paren.expression.emit(ctx, f)?;
                    emit_gap(paren.expression.slice(), &paren.close_paren, "", ctx, f)?;
                    write!(f, ")")
                }

                Expression::PropertyAccessExpression(prop_access) => {
                    prop_access.subject.emit(ctx, f)?;
                    emit_gap(prop_access.subject.slice(), &prop_access.dot, "", ctx, f)?;
                    write!(f, ".")?;
                    emit_gap(&prop_access.dot, prop_access.property.slice(), "", ctx, f)?;
                    prop_access.property.emit(ctx, f)
                }

                Expression::PipeCallExpression(pipe) => {
                    pipe.subject.emit(ctx, f)?;
                    emit_gap(pipe.subject.slice(), &pipe.double_dot, "", ctx, f)?;
                    write!(f, "..")?;

                    let mut prev_slice = &pipe.double_dot;

                    if let Some(func) = &pipe.function {
                        emit_gap(prev_slice, func.slice(), "", ctx, f)?;
                        func.emit(ctx, f)?;
                        prev_slice = func.slice();
                    }

                    if let Some(open_paren) = &pipe.open_paren {
                        emit_gap(prev_slice, open_paren, "", ctx, f)?;
                        write!(f, "(")?;

                        for (i, arg) in pipe.arguments.iter().enumerate() {
                            if i > 0 {
                                let comma = &pipe.commas[i - 1];
                                emit_gap(pipe.arguments[i - 1].slice(), comma, "", ctx, f)?;
                                write!(f, ",")?;
                                emit_gap(comma, arg.slice(), " ", ctx, f)?;
                            } else {
                                emit_gap(open_paren, arg.slice(), "", ctx, f)?;
                            }
                            arg.emit(ctx, f)?;
                        }

                        if let Some(trailing) = &pipe.trailing_comma {
                            write!(f, ",")?;
                            if let Some(close) = &pipe.close_paren {
                                emit_gap(trailing, close, "", ctx, f)?;
                            }
                        } else if let Some(close) = &pipe.close_paren {
                            let last_end = pipe
                                .arguments
                                .last()
                                .map(|a| a.slice())
                                .unwrap_or(open_paren);
                            emit_gap(last_end, close, "", ctx, f)?;
                        }

                        write!(f, ")")?;
                    }

                    Ok(())
                }

                Expression::IfElseExpression(if_else) => {
                    write!(f, "if")?;
                    emit_gap(&if_else.if_keyword, if_else.condition.slice(), " ", ctx, f)?;
                    if_else.condition.emit(ctx, f)?;
                    emit_gap(if_else.condition.slice(), &if_else.open_brace, " ", ctx, f)?;
                    write!(f, "{{")?;
                    emit_gap(&if_else.open_brace, if_else.consequent.slice(), " ", ctx, f)?;
                    if_else.consequent.emit(ctx, f)?;
                    emit_gap(
                        if_else.consequent.slice(),
                        &if_else.close_brace,
                        " ",
                        ctx,
                        f,
                    )?;
                    write!(f, "}}")?;

                    match &if_else.else_clause {
                        Some(ElseClause::ElseBlock {
                            else_keyword,
                            open_brace,
                            expression,
                            close_brace,
                        }) => {
                            emit_gap(&if_else.close_brace, else_keyword, " ", ctx, f)?;
                            write!(f, "else")?;
                            emit_gap(else_keyword, open_brace, " ", ctx, f)?;
                            write!(f, "{{")?;
                            emit_gap(open_brace, expression.slice(), " ", ctx, f)?;
                            expression.emit(ctx, f)?;
                            emit_gap(expression.slice(), close_brace, " ", ctx, f)?;
                            write!(f, "}}")
                        }
                        Some(ElseClause::ElseIf {
                            else_keyword,
                            if_else: nested,
                        }) => {
                            emit_gap(&if_else.close_brace, else_keyword, " ", ctx, f)?;
                            write!(f, "else")?;
                            emit_gap(else_keyword, nested.slice(), " ", ctx, f)?;
                            nested.emit(ctx, f)
                        }
                        None => Ok(()),
                    }
                }
            },

            Any::TypeExpression(type_expression) => match type_expression {
                TypeExpression::UnknownTypeExpression(_) => write!(f, "unknown"),
                TypeExpression::NilTypeExpression(_) => write!(f, "nil"),
                TypeExpression::BooleanTypeExpression(_) => write!(f, "boolean"),
                TypeExpression::NumberTypeExpression(_) => write!(f, "number"),
                TypeExpression::StringTypeExpression(_) => write!(f, "string"),

                TypeExpression::BooleanLiteralTypeExpression(lit) => {
                    write!(f, "{}", if lit.value { "true" } else { "false" })
                }
                TypeExpression::NumberLiteralTypeExpression(lit) => {
                    write!(f, "{}", lit.slice.as_str())
                }
                TypeExpression::StringLiteralTypeExpression(lit) => {
                    write!(f, "'{}'", lit.contents.as_str())
                }

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

                TypeExpression::NillableTypeExpression(nillable) => {
                    nillable.subject.emit(ctx, f)?;
                    write!(f, "?")
                }

                TypeExpression::NamedTypeExpression(named) => named.identifier.emit(ctx, f),
            },

            Any::PlainIdentifier(id) => {
                write!(f, "{}", id.slice.as_str())
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
                Statement::Expression(expr) => Any::Expression(expr.clone()).emit(ctx, f),
                Statement::Block(block) => {
                    let inner_ctx = ctx.indented();
                    writeln!(f, "{{")?;
                    for stmt in &block.statements {
                        emit_indent(inner_ctx, f)?;
                        stmt.emit(inner_ctx, f)?;
                        writeln!(f)?;
                    }
                    emit_indent(ctx, f)?;
                    write!(f, "}}")
                }
            },
        }
    }
}

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    pub fn estimated_length(&self) -> Option<usize> {
        match self.details() {
            Some(ast) => match ast {
                Any::Module(module) => module
                    .declarations
                    .iter()
                    .map(|d| d.estimated_length().unwrap_or(0))
                    .max(),
                Any::Declaration(declaration) => match declaration {
                    Declaration::ConstDeclaration(_) => None,
                    Declaration::TypeDeclaration(_) => None,
                    Declaration::ImportDeclaration(_) => None,
                },
                Any::Expression(expression) => match expression {
                    Expression::NilLiteral(nil_literal) => Some("nil".len()),
                    Expression::BooleanLiteral(boolean_literal) => match boolean_literal.value {
                        true => Some("true".len()),
                        false => Some("false".len()),
                    },
                    Expression::NumberLiteral(number_literal) => Some(number_literal.slice.len()),
                    Expression::StringLiteral(string_literal) => {
                        Some(1 + string_literal.contents.len() + 1)
                    }
                    Expression::BinaryOperation(BinaryOperation {
                        left,
                        operator,
                        right,
                    }) => Some(
                        left.estimated_length().unwrap_or(0)
                            + operator.estimated_length().unwrap_or(0)
                            + right.estimated_length().unwrap_or(0),
                    ),
                    Expression::UnaryOperation(UnaryOperation { operator, operand }) => Some(
                        operator.estimated_length().unwrap_or(0)
                            + operand.estimated_length().unwrap_or(0),
                    ),
                    Expression::LocalIdentifier(local_identifier) => {
                        Some(local_identifier.slice.len())
                    }
                    Expression::Invocation(invocation) => None,
                    Expression::FunctionExpression(function_expression) => None,
                    Expression::ArrayLiteral(array_literal) => Some(
                        1 + array_literal
                            .elements
                            .iter()
                            .map(|e| e.estimated_length().unwrap_or(0) + 1)
                            .sum::<usize>()
                            - 1 // trailing comma
                            + 1,
                    ),
                    Expression::ObjectLiteral(object_literal) => None,
                    Expression::IfElseExpression(if_else_expression) => None,
                    Expression::ParenthesizedExpression(parenthesized_expression) => {
                        parenthesized_expression
                            .expression
                            .estimated_length()
                            .map(|e| 1 + e + 1)
                    }
                    Expression::PropertyAccessExpression(property_access_expression) => None,
                    Expression::PipeCallExpression(_) => None,
                },
                Any::TypeExpression(type_expression) => None,
                Any::Statement(statement) => match statement {
                    Statement::Expression(expression) => None,
                    Statement::Block(block) => block
                        .statements
                        .iter()
                        .map(|s| s.estimated_length().unwrap_or(0))
                        .max(),
                },
                Any::PlainIdentifier(plain_identifier) => {
                    Some(plain_identifier.slice.as_str().len())
                }
                Any::BinaryOperator(binary_operator) => Some(binary_operator.as_str().len()),
                Any::UnaryOperator(unary_operator) => Some(unary_operator.as_str().len()),
                Any::FunctionBody(function_body) => None,
            },
            None => None,
        }
    }
}
