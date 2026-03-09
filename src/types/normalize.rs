use std::collections::HashSet;
use std::sync::Arc;

use crate::ast::container::AST;
use crate::ast::grammar::{
    self, Any, BinaryOperator, Declaration, Expression, FunctionBody, UnaryOperator,
};
use crate::ast::modules::{Module, ModulesStore};
use crate::ast::slice::Slice;
use crate::types::fits::FitsContext;
use crate::types::infer::InferTypeContext;
use crate::types::Type;

#[derive(Debug, Clone, Copy)]
pub struct NormalizeContext<'a> {
    pub modules: Option<&'a ModulesStore>,
    pub current_module: Option<&'a Module>,
    pub param_type_overrides: Option<&'a ParamTypeOverrides>,
    pub type_bindings: Option<&'a TypeBindings>,
}

#[derive(Debug, Clone)]
pub struct ParamTypeOverrides {
    pub func_node: AST<grammar::FunctionExpression>,
    pub arg_types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct TypeBindings {
    pub bindings: Vec<(String, Type)>,
}

/// The result of resolving a local identifier to its declaration site.
#[derive(Debug, Clone)]
pub enum ResolvedIdentifier<'a> {
    /// A const declaration, possibly in a different module
    ConstDeclaration {
        decl: grammar::ConstDeclaration,
        module: Option<&'a Module>,
    },
    /// A type declaration, possibly in a different module
    TypeDeclaration {
        decl: grammar::TypeDeclaration,
        module: Option<&'a Module>,
    },
    /// A function parameter in the current module
    FunctionParam {
        name: AST<grammar::PlainIdentifier>,
        param_index: usize,
        func_node: AST<Any>,
    },
}

impl ResolvedIdentifier<'_> {
    pub fn name(&self) -> &str {
        match self {
            ResolvedIdentifier::ConstDeclaration { decl, .. } => decl.identifier.slice().as_str(),
            ResolvedIdentifier::TypeDeclaration { decl, .. } => decl.identifier.slice().as_str(),
            ResolvedIdentifier::FunctionParam { name, .. } => name.slice().as_str(),
        }
    }
}

impl Type {
    /// Some types can be exactly equivalent in the set of values they describe,
    /// but structurally represented differently. For example, a union that
    /// contains one or more other unions doesn't change its meaning if those
    /// unions are flattened. Other examples include:
    /// - A union containing two of the exact same type
    /// - A union containing a more general type like `number` alongside a more
    ///   specific type like `42` (the more general type can subsume both of
    ///   them)
    /// - A union containing two object types where one is broader than the
    ///   other
    /// - Two unions that contain the same set of elements in a different
    ///   ordering
    ///
    /// This function makes a best-effort to simplify and normalize as many of
    /// these cases as possible, to make types more human-readable but also to
    /// create more situations where exactly-equivalent types have the exact
    /// same structural representation.
    ///
    /// It also, additionally, "interprets" certain types that are lazily
    /// resolved, to try and figure out their "real" type. Local identifiers
    /// are a major instance- this function determines what "real" type a given
    /// variable turns out to have.
    pub fn normalize(self, ctx: NormalizeContext<'_>) -> Self {
        use Type::*;

        match self {
            Union { variants } => {
                let flattened_and_unique = variants
                    .into_iter()
                    .flat_map(|v| {
                        let v = v.normalize(ctx); // recurse

                        // flatten any inner unions
                        match v {
                            Union {
                                variants: variants_inner,
                            } => variants_inner,
                            other => vec![other],
                        }
                    })
                    .collect::<HashSet<Type>>(); // collect into a HashSet to uniquify

                let mut v: Vec<Type> = flattened_and_unique.into_iter().collect();
                v.sort(); // finally, sort the types for consistency

                // Remove variants that are subsumed by other variants.
                // E.g. `true | boolean` collapses to `boolean` because
                // `true` fits into `boolean`.
                let fits_ctx = FitsContext {
                    modules: ctx.modules,
                };
                let keep: Vec<bool> = v
                    .iter()
                    .enumerate()
                    .map(|(i, candidate)| {
                        !v.iter().enumerate().any(|(j, other)| {
                            i != j
                                && candidate.clone().fits(other.clone(), fits_ctx)
                                && !other.clone().fits(candidate.clone(), fits_ctx)
                        })
                    })
                    .collect();
                let mut keep_iter = keep.iter();
                v.retain(|_| *keep_iter.next().unwrap());

                if v.len() == 1 {
                    v.into_iter().next().unwrap()
                } else if v.is_empty() {
                    Type::Never
                } else {
                    Union { variants: v }
                }
            }
            LocalIdentifier { identifier } => resolve_local_identifier(&identifier, ctx),
            NamedType { name, identifier } => resolve_named_type(&name, identifier.as_ref(), ctx),
            Invocation { function, args } => {
                let normalized_args: Vec<Type> =
                    args.into_iter().map(|a| a.normalize(ctx)).collect();
                let func_type = function.as_ref().clone().normalize(ctx);
                let func_type = resolve_generic(func_type, Some(&normalized_args), ctx);

                match func_type {
                    FuncType {
                        original_expression: Some(func_expr_node),
                        ..
                    } => {
                        // Re-infer the body's return type from the AST so we get
                        // the lazy type tree (with LocalIdentifier references to
                        // parameters still intact), then normalize it with
                        // parameter type overrides from the call-site arguments.
                        let infer_ctx = InferTypeContext {
                            modules: ctx.modules,
                            current_module: ctx.current_module,
                        };
                        let body_type = func_expr_node
                            .unpack()
                            .map(|func| match &func.return_type {
                                Some((_colon, ret_type)) => {
                                    ret_type.unpack().map(Type::from).unwrap_or(Poisoned)
                                }
                                None => match func.body.unpack() {
                                    Some(FunctionBody::Expression(expr)) => {
                                        expr.infer_type(infer_ctx)
                                    }
                                    Some(FunctionBody::Block(_)) => Type::Never,
                                    None => Poisoned,
                                },
                            })
                            .unwrap_or(Poisoned);

                        let overrides = ParamTypeOverrides {
                            func_node: func_expr_node,
                            arg_types: normalized_args,
                        };
                        let override_ctx = NormalizeContext {
                            param_type_overrides: Some(&overrides),
                            ..ctx
                        };
                        body_type.normalize(override_ctx)
                    }
                    FuncType { returns, .. } => returns.as_ref().clone().normalize(ctx),
                    _ => Unknown,
                }
            }
            PropertyAccess { subject, property } => {
                let subject_normalized = subject.as_ref().clone().normalize(ctx);
                match subject_normalized.known_properties() {
                    Some(fields) => fields.get(&property).cloned().unwrap_or(Poisoned),
                    None => PropertyAccess {
                        subject: Arc::new(subject_normalized),
                        property,
                    },
                }
            }
            BinaryOperation {
                operator,
                left,
                right,
            } => normalize_binary_operation(
                operator,
                left.as_ref().clone().normalize(ctx),
                right.as_ref().clone().normalize(ctx),
                ctx,
            ),
            UnaryOperation { operator, operand } => {
                normalize_unary_operation(operator, operand.as_ref().clone().normalize(ctx))
            }
            IfElse {
                condition,
                consequent,
                alternate,
            } => {
                if condition.as_ref() == &Type::TRUE {
                    consequent.as_ref().clone().normalize(ctx)
                } else if condition.as_ref() == &Type::FALSE {
                    alternate.as_ref().clone().normalize(ctx)
                } else {
                    Type::Union {
                        variants: vec![consequent.as_ref().clone(), alternate.as_ref().clone()],
                    }
                    .normalize(ctx)
                }
            }
            Tuple { elements } => Tuple {
                elements: elements.into_iter().map(|e| e.normalize(ctx)).collect(),
            },
            Array { element } => Array {
                element: Arc::new(element.as_ref().clone().normalize(ctx)),
            },
            Object { fields } => Object {
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, v.normalize(ctx)))
                    .collect(),
            },
            Interface { name, fields } => Interface {
                name,
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, v.normalize(ctx)))
                    .collect(),
            },
            FuncType {
                args,
                args_spread,
                returns,
                original_expression,
            } => FuncType {
                args: args.into_iter().map(|a| a.normalize(ctx)).collect(),
                args_spread: args_spread.map(|s| Arc::new(s.as_ref().clone().normalize(ctx))),
                returns: Arc::new(returns.as_ref().clone().normalize(ctx)),
                original_expression,
            },
            Generic {
                parameters,
                subject,
            } => {
                let identity_bindings = TypeBindings {
                    bindings: parameters
                        .iter()
                        .map(|p| {
                            (
                                p.clone(),
                                Type::NamedType {
                                    name: p.clone(),
                                    identifier: None,
                                },
                            )
                        })
                        .collect(),
                };
                let binding_ctx = NormalizeContext {
                    type_bindings: Some(&identity_bindings),
                    ..ctx
                };
                Generic {
                    parameters,
                    subject: Arc::new(subject.as_ref().clone().normalize(binding_ctx)),
                }
            }
            Parameterized { subject, arguments } => {
                let normalized_subject = subject.as_ref().clone().normalize(ctx);
                let normalized_arguments: Vec<Type> =
                    arguments.into_iter().map(|a| a.normalize(ctx)).collect();

                match normalized_subject {
                    Generic {
                        parameters,
                        subject: generic_body,
                    } => {
                        let bindings = TypeBindings {
                            bindings: parameters.into_iter().zip(normalized_arguments).collect(),
                        };
                        let binding_ctx = NormalizeContext {
                            type_bindings: Some(&bindings),
                            ..ctx
                        };
                        generic_body.as_ref().clone().normalize(binding_ctx)
                    }
                    other => Parameterized {
                        subject: Arc::new(other),
                        arguments: normalized_arguments,
                    },
                }
            }
            _ => self,
        }
    }

    /// Widens exact literal types to their general counterparts.
    /// For example, `ExactBoolean(true)` becomes `Boolean`, `ExactNumber(42)`
    /// becomes `Number`, `ExactString("foo")` becomes `String`.
    /// This is used when inferring the type of a variable declaration without
    /// an explicit type annotation.
    pub fn widen(self) -> Self {
        match self {
            Type::Boolean { value: Some(_) } => Type::ANY_BOOLEAN,
            Type::Number {
                min_value: Some(_), ..
            }
            | Type::Number {
                max_value: Some(_), ..
            } => Type::ANY_NUMBER,
            Type::String { value: Some(_) } => Type::ANY_STRING,
            other => other,
        }
    }
}

impl AST<Expression> {
    /// Given an expression at some specific point in the AST tree, look
    /// upwards to try and find what type it "should" have based on its context,
    /// if any.
    ///
    /// Examples:
    /// - The value of a const declaration, if the const has an annotated type,
    ///   is expected to be that annotated type
    /// - The value of an argument to a function invocation is expected to be
    ///   the type of that function's Nth parameter
    ///
    /// This will be used for things like inferring the parameter types of a
    /// function that doesn't have them annotated, but is passed into a context
    /// where they're known (lambdas)
    pub fn expected_type(&self) -> Option<Type> {
        let parent = self.parent()?;
        let norm_ctx = NormalizeContext {
            modules: None,
            current_module: None,
            param_type_overrides: None,
            type_bindings: None,
        };

        match parent.details()? {
            Any::Declaration(grammar::Declaration::ConstDeclaration(const_decl)) => const_decl
                .type_annotation
                .as_ref()
                .and_then(|(_colon, type_expr)| type_expr.unpack().map(Type::from)),
            Any::Expression(Expression::Invocation(inv)) => {
                let arg_index = inv.arguments.iter().position(|arg| arg.ptr_eq(self))?;

                let ctx = InferTypeContext {
                    modules: None,
                    current_module: None,
                };
                let func_type = inv.function.infer_type(ctx).normalize(norm_ctx);
                let func_type = resolve_generic(func_type, None, norm_ctx);

                match func_type {
                    Type::FuncType { args, .. } => args.into_iter().nth(arg_index),
                    _ => None,
                }
            }
            Any::Expression(Expression::PipeCallExpression(pipe)) => {
                // subject is arg0, explicit arguments start at index 1
                let arg_index = if pipe.subject.ptr_eq(self) {
                    Some(0)
                } else {
                    pipe.arguments
                        .iter()
                        .position(|arg| arg.ptr_eq(self))
                        .map(|i| i + 1)
                };

                arg_index.and_then(|idx| {
                    let func = pipe.function.as_ref()?;
                    let func_expr: AST<Expression> = func.clone().upcast();
                    let infer_ctx = InferTypeContext {
                        modules: norm_ctx.modules,
                        current_module: norm_ctx.current_module,
                    };
                    let func_type = func_expr.infer_type(infer_ctx).normalize(norm_ctx);
                    let func_type = resolve_generic(func_type, None, norm_ctx);

                    match func_type {
                        Type::FuncType { args, .. } => args.into_iter().nth(idx),
                        _ => None,
                    }
                })
            }
            Any::FunctionBody(FunctionBody::Expression(_)) => {
                let func_body_node = parent;
                let func_node = func_body_node.parent()?;

                match func_node.details()? {
                    Any::Expression(Expression::FunctionExpression(func)) => func
                        .return_type
                        .as_ref()
                        .and_then(|(_colon, type_expr)| type_expr.unpack().map(Type::from)),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

/// Walks up the AST scope chain from the given node and collects all
/// identifiers (const declarations, imports, function parameters) that are
/// visible at that point.
pub fn identifiers_in_scope<'a>(
    node: &AST<Any>,
    ctx: NormalizeContext<'a>,
) -> Vec<ResolvedIdentifier<'a>> {
    let mut results = Vec::new();
    let mut current = node.parent();

    while let Some(ancestor) = current {
        match ancestor.details() {
            Some(Any::Module(module)) => {
                // Collect module-level const declarations
                results.extend(
                    module
                        .declarations
                        .iter()
                        .filter_map(|decl| match decl.unpack() {
                            Some(Declaration::ConstDeclaration(const_decl)) => Some(const_decl),
                            _ => None,
                        })
                        .map(|const_decl| ResolvedIdentifier::ConstDeclaration {
                            decl: const_decl,
                            module: None,
                        }),
                );

                // Collect imported identifiers
                if let (Some(store), Some(current_module)) = (ctx.modules, ctx.current_module) {
                    results.extend(
                        module
                            .declarations
                            .iter()
                            .filter_map(|decl| match decl.unpack() {
                                Some(Declaration::ImportDeclaration(import_decl)) => {
                                    Some(import_decl)
                                }
                                _ => None,
                            })
                            .flat_map(|import_decl| {
                                let import_path = import_decl
                                    .path
                                    .unpack()
                                    .map(|lit| lit.contents.as_str().to_string());
                                import_decl
                                    .imports
                                    .iter()
                                    .filter_map(move |spec| {
                                        let original_name = spec.name.slice().as_str().to_string();
                                        let path = import_path.as_ref()?;
                                        resolve_imported_identifier(
                                            &original_name,
                                            path,
                                            store,
                                            current_module,
                                        )
                                    })
                                    .collect::<Vec<_>>()
                            }),
                    );
                }

                // Module is the top-level scope; stop walking
                break;
            }
            Some(Any::Expression(Expression::FunctionExpression(func))) => {
                results.extend(func.parameters.iter().enumerate().map(
                    |(param_index, (param_name, _type_ann))| ResolvedIdentifier::FunctionParam {
                        name: param_name.clone(),
                        param_index,
                        func_node: ancestor.clone(),
                    },
                ));
            }
            _ => {}
        }

        current = ancestor.parent();
    }

    results
}

/// Resolve a specific identifier name at the given AST node. Walks up the
/// scope chain and returns the first declaration matching `name`.
pub fn resolve_identifier<'a>(
    name: &str,
    node: &AST<Any>,
    ctx: NormalizeContext<'a>,
) -> Option<ResolvedIdentifier<'a>> {
    identifiers_in_scope(node, ctx)
        .into_iter()
        .find(|r| r.name() == name)
}

/// The built-in `js` global object type. This is an Interface that exposes
/// JavaScript built-ins to Bagel code.
pub fn js_global_type() -> Type {
    Type::Interface {
        name: "JsGlobals".to_string(),
        fields: std::collections::BTreeMap::from([(
            String::from("console"),
            Type::Interface {
                name: "Console".to_string(),
                fields: std::collections::BTreeMap::from([(
                    String::from("log"),
                    Type::FuncType {
                        args: vec![],
                        args_spread: Some(Arc::new(Type::Unknown)),
                        returns: Arc::new(Type::Never),
                        original_expression: None,
                    },
                )]),
            },
        )]),
    }
}

/// Thin wrapper around `resolve_identifier` that extracts the type from the
/// resolution result. Called by `normalize()` for `Type::LocalIdentifier`.
fn resolve_local_identifier(
    identifier: &AST<grammar::LocalIdentifier>,
    ctx: NormalizeContext<'_>,
) -> Type {
    let name = identifier.slice().as_str();

    // Special built-in: the `js` global
    if name == "js" {
        return js_global_type();
    }

    let node: AST<Any> = identifier.clone().upcast();
    match resolve_identifier(name, &node, ctx) {
        Some(ResolvedIdentifier::ConstDeclaration { decl, module }) => {
            let decl_ctx = match module {
                Some(m) => NormalizeContext {
                    current_module: Some(m),
                    ..ctx
                },
                None => ctx,
            };
            resolve_declaration_type(&decl, decl_ctx)
        }
        Some(ResolvedIdentifier::TypeDeclaration { .. }) => {
            // Type declarations resolve in the type namespace, not the value namespace
            Type::Poisoned
        }
        Some(ResolvedIdentifier::FunctionParam {
            name: _,
            param_index,
            func_node,
        }) => {
            // Check for call-site argument type overrides
            if let Some(overrides) = ctx.param_type_overrides {
                if overrides.func_node.ptr_eq(&func_node) {
                    if let Some(arg_type) = overrides.arg_types.get(param_index) {
                        return arg_type.clone();
                    }
                }
            }

            // Check if the parameter has a type annotation
            match func_node.details() {
                Some(Any::Expression(Expression::FunctionExpression(func))) => {
                    match &func.parameters[param_index].1 {
                        Some((_colon, type_expr)) => {
                            type_expr.unpack().map(Type::from).unwrap_or(Type::Poisoned)
                        }
                        None => {
                            // Try contextual typing: get expected type for the
                            // function expression and extract the parameter type
                            let func_expr_node: AST<Expression> = match &func_node {
                                AST::Valid(inner, _) => AST::<Expression>::new(inner.clone()),
                                AST::Malformed(slice, message) => {
                                    AST::Malformed(slice.clone(), message.clone())
                                }
                            };
                            func_expr_node
                                .expected_type()
                                .and_then(|t| match resolve_generic(t.normalize(ctx), None, ctx) {
                                    Type::FuncType { args, .. } => {
                                        args.into_iter().nth(param_index)
                                    }
                                    _ => None,
                                })
                                .unwrap_or(Type::Unknown)
                        }
                    }
                }
                _ => Type::Unknown,
            }
        }
        None => Type::Poisoned,
    }
}

/// Resolve an identifier imported from another module, returning the
/// declaration site in the imported module.
fn resolve_imported_identifier<'a>(
    name: &str,
    import_path: &str,
    store: &'a ModulesStore,
    current_module: &Module,
) -> Option<ResolvedIdentifier<'a>> {
    let imported_module = store.find_imported(current_module, import_path)?;

    // Find the exported const declaration with the matching name in the imported module
    let imported_module_data = imported_module.ast.unpack()?;
    imported_module_data
        .declarations
        .iter()
        .filter(|decl| decl.details().is_some())
        .filter_map(|decl| match decl.unpack().unwrap() {
            grammar::Declaration::ConstDeclaration(const_decl) => Some(const_decl),
            grammar::Declaration::TypeDeclaration(_)
            | grammar::Declaration::ImportDeclaration(_) => None,
        })
        .find(|const_decl| const_decl.identifier.slice().as_str() == name)
        .map(|const_decl| ResolvedIdentifier::ConstDeclaration {
            decl: const_decl,
            module: Some(imported_module),
        })
}

/// Thin wrapper around `resolve_type_identifier` that extracts the type from
/// the resolution result. Called by `normalize()` for `Type::NamedType`.
fn resolve_named_type(
    name: &str,
    identifier: Option<&AST<grammar::PlainIdentifier>>,
    ctx: NormalizeContext<'_>,
) -> Type {
    // Check type parameter bindings first
    if let Some(bindings) = ctx.type_bindings {
        if let Some((_, bound_type)) = bindings.bindings.iter().find(|(p, _)| p == name) {
            return bound_type.clone();
        }
    }

    // AST lookup requires the identifier node
    match identifier {
        Some(identifier) => {
            let node: AST<Any> = identifier.clone().upcast();
            match resolve_type_identifier(name, &node, ctx) {
                Some(ResolvedIdentifier::TypeDeclaration { decl, module }) => {
                    let decl_ctx = match module {
                        Some(m) => NormalizeContext {
                            current_module: Some(m),
                            ..ctx
                        },
                        None => ctx,
                    };
                    decl.value
                        .unpack()
                        .map(Type::from)
                        .unwrap_or(Type::Poisoned)
                        .normalize(decl_ctx)
                }
                _ => Type::Poisoned,
            }
        }
        None => Type::Poisoned,
    }
}

/// Resolve a type identifier name at the given AST node. Walks up the
/// scope chain and returns the first type declaration matching `name`.
pub fn resolve_type_identifier<'a>(
    name: &str,
    node: &AST<Any>,
    ctx: NormalizeContext<'a>,
) -> Option<ResolvedIdentifier<'a>> {
    type_identifiers_in_scope(node, ctx)
        .into_iter()
        .find(|r| r.name() == name)
}

/// Walks up the AST scope chain from the given node and collects all
/// type identifiers (type declarations and imported type declarations) that
/// are visible at that point.
pub fn type_identifiers_in_scope<'a>(
    node: &AST<Any>,
    ctx: NormalizeContext<'a>,
) -> Vec<ResolvedIdentifier<'a>> {
    let mut results = Vec::new();
    let mut current = node.parent();

    while let Some(ancestor) = current {
        match ancestor.details() {
            Some(Any::Module(module)) => {
                // Collect module-level type declarations
                results.extend(
                    module
                        .declarations
                        .iter()
                        .filter_map(|decl| match decl.unpack() {
                            Some(Declaration::TypeDeclaration(type_decl)) => Some(type_decl),
                            _ => None,
                        })
                        .map(|type_decl| ResolvedIdentifier::TypeDeclaration {
                            decl: type_decl,
                            module: None,
                        }),
                );

                // Collect imported type identifiers
                if let (Some(store), Some(current_module)) = (ctx.modules, ctx.current_module) {
                    results.extend(
                        module
                            .declarations
                            .iter()
                            .filter_map(|decl| match decl.unpack() {
                                Some(Declaration::ImportDeclaration(import_decl)) => {
                                    Some(import_decl)
                                }
                                _ => None,
                            })
                            .flat_map(|import_decl| {
                                let import_path = import_decl
                                    .path
                                    .unpack()
                                    .map(|lit| lit.contents.as_str().to_string());
                                import_decl
                                    .imports
                                    .iter()
                                    .filter_map(move |spec| {
                                        let original_name = spec.name.slice().as_str().to_string();
                                        let path = import_path.as_ref()?;
                                        resolve_imported_type_identifier(
                                            &original_name,
                                            path,
                                            store,
                                            current_module,
                                        )
                                    })
                                    .collect::<Vec<_>>()
                            }),
                    );
                }

                break;
            }
            _ => {}
        }

        current = ancestor.parent();
    }

    results
}

/// Resolve an identifier imported from another module as a type declaration.
fn resolve_imported_type_identifier<'a>(
    name: &str,
    import_path: &str,
    store: &'a ModulesStore,
    current_module: &Module,
) -> Option<ResolvedIdentifier<'a>> {
    let imported_module = store.find_imported(current_module, import_path)?;

    let imported_module_data = imported_module.ast.unpack()?;
    imported_module_data
        .declarations
        .iter()
        .filter(|decl| decl.details().is_some())
        .filter_map(|decl| match decl.unpack().unwrap() {
            grammar::Declaration::TypeDeclaration(type_decl) => Some(type_decl),
            _ => None,
        })
        .find(|type_decl| type_decl.identifier.slice().as_str() == name)
        .map(|type_decl| ResolvedIdentifier::TypeDeclaration {
            decl: type_decl,
            module: Some(imported_module),
        })
}

/// Resolve the type of a declaration from its type annotation (if present)
/// or by inferring from its value expression.
fn resolve_declaration_type(
    decl: &grammar::ConstDeclaration,
    norm_ctx: NormalizeContext<'_>,
) -> Type {
    let ctx = InferTypeContext {
        modules: norm_ctx.modules,
        current_module: norm_ctx.current_module,
    };
    decl.value.infer_type(ctx).normalize(norm_ctx)
}

/// Helper to create an exact String type from a computed string value.
fn exact_string(s: String) -> Type {
    Type::String {
        value: Some(Slice::new(Arc::new(s))),
    }
}

/// Helper: construct an exact number type (min == max == value).
fn exact_number(value: i64) -> Type {
    Type::Number {
        min_value: Some(value),
        max_value: Some(value),
    }
}

/// Helper: try to extract an exact i64 from a Number type where min == max.
fn as_exact_number(t: &Type) -> Option<i64> {
    match t {
        Type::Number {
            min_value: Some(min),
            max_value: Some(max),
        } if min == max => Some(*min),
        _ => None,
    }
}

/// Helper: try to extract an exact string value from a String type.
fn as_exact_string(t: &Type) -> Option<&Slice> {
    match t {
        Type::String { value: Some(v) } => Some(v),
        _ => None,
    }
}

/// Helper: try to extract an exact bool value from a Boolean type.
fn as_exact_bool(t: &Type) -> Option<bool> {
    match t {
        Type::Boolean { value: Some(v) } => Some(*v),
        _ => None,
    }
}

/// Normalize a binary operation type by computing exact results where possible.
fn normalize_binary_operation(
    operator: BinaryOperator,
    left: Type,
    right: Type,
    ctx: NormalizeContext<'_>,
) -> Type {
    use BinaryOperator::*;

    let fits_ctx = FitsContext {
        modules: ctx.modules,
    };

    if left == Type::Poisoned || right == Type::Poisoned {
        return Type::Poisoned;
    }

    // Distribute over unions: (A | B) + C  →  (A + C) | (B + C), etc.
    if let Type::Union { variants } = &left {
        return Type::Union {
            variants: variants
                .iter()
                .map(|v| normalize_binary_operation(operator, v.clone(), right.clone(), ctx))
                .collect(),
        }
        .normalize(ctx);
    }
    if let Type::Union { variants } = &right {
        return Type::Union {
            variants: variants
                .iter()
                .map(|v| normalize_binary_operation(operator, left.clone(), v.clone(), ctx))
                .collect(),
        }
        .normalize(ctx);
    }

    // Check operand compatibility; return Poisoned if either operand doesn't fit
    if let Some(allowed) = operator.allowed_operand_type() {
        if !left.clone().fits(allowed.clone(), fits_ctx) || !right.clone().fits(allowed, fits_ctx) {
            return Type::Poisoned;
        }
    }

    match operator {
        Add => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                l.checked_add(r)
                    .map(exact_number)
                    .unwrap_or(Type::ANY_NUMBER)
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_string(&right)) {
                exact_string(format!("{}{}", l.as_str(), r.as_str()))
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_number(&right)) {
                exact_string(format!("{}{}", l.as_str(), r))
            } else if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_string(&right)) {
                exact_string(format!("{}{}", l, r.as_str()))
            } else if matches!(&left, Type::String { .. }) || matches!(&right, Type::String { .. })
            {
                Type::ANY_STRING
            } else {
                Type::ANY_NUMBER
            }
        }
        Subtract => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                l.checked_sub(r)
                    .map(exact_number)
                    .unwrap_or(Type::ANY_NUMBER)
            } else {
                Type::ANY_NUMBER
            }
        }
        Multiply => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                l.checked_mul(r)
                    .map(exact_number)
                    .unwrap_or(Type::ANY_NUMBER)
            } else {
                Type::ANY_NUMBER
            }
        }
        Divide => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                if r != 0 {
                    l.checked_div(r)
                        .map(exact_number)
                        .unwrap_or(Type::ANY_NUMBER)
                } else {
                    Type::ANY_NUMBER
                }
            } else {
                Type::ANY_NUMBER
            }
        }

        Equal => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l == r),
                }
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_string(&right)) {
                Type::Boolean {
                    value: Some(l.as_str() == r.as_str()),
                }
            } else if let (Some(l), Some(r)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                Type::Boolean {
                    value: Some(l == r),
                }
            } else if left == Type::Nil && right == Type::Nil {
                Type::TRUE
            } else {
                Type::ANY_BOOLEAN
            }
        }
        NotEqual => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l != r),
                }
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_string(&right)) {
                Type::Boolean {
                    value: Some(l.as_str() != r.as_str()),
                }
            } else if let (Some(l), Some(r)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                Type::Boolean {
                    value: Some(l != r),
                }
            } else if left == Type::Nil && right == Type::Nil {
                Type::FALSE
            } else {
                Type::ANY_BOOLEAN
            }
        }
        LessThan => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean { value: Some(l < r) }
            } else {
                Type::ANY_BOOLEAN
            }
        }
        LessThanOrEqual => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l <= r),
                }
            } else {
                Type::ANY_BOOLEAN
            }
        }
        GreaterThan => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean { value: Some(l > r) }
            } else {
                Type::ANY_BOOLEAN
            }
        }
        GreaterThanOrEqual => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l >= r),
                }
            } else {
                Type::ANY_BOOLEAN
            }
        }

        And => {
            if let (Some(left), Some(right)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                (left && right).into()
            } else {
                Type::ANY_BOOLEAN
            }
        }
        Or => {
            if let (Some(left), Some(right)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                (left || right).into()
            } else {
                Type::ANY_BOOLEAN
            }
        }
        NullishCoalescing => {
            if left == Type::Nil {
                right
            } else if !Type::Nil.fits(left.clone(), fits_ctx) {
                left
            } else {
                Type::Union {
                    variants: vec![left, right],
                }
                .normalize(ctx)
            }
        }
    }
}

/// If the type is `Generic { parameters, subject }`, resolve it by inferring
/// type parameter bindings from the actual invocation arguments (if available),
/// then instantiating the generic. Non-generic types pass through unchanged.
///
/// When `args` is `Some`, the function structurally matches actual argument
/// types against the generic function's parameter types to infer bindings.
/// When `args` is `None`, all parameters default to `Unknown`.
fn resolve_generic(t: Type, args: Option<&[Type]>, ctx: NormalizeContext<'_>) -> Type {
    match t {
        Type::Generic {
            parameters,
            subject,
        } => {
            let mut bindings: Vec<(String, Vec<Type>)> =
                parameters.iter().map(|p| (p.clone(), vec![])).collect();

            if let (
                Some(actual_args),
                Type::FuncType {
                    args: param_types, ..
                },
            ) = (args, subject.as_ref())
            {
                param_types
                    .iter()
                    .zip(actual_args.iter())
                    .for_each(|(pattern, concrete)| {
                        infer_generic_bindings(pattern, concrete, &mut bindings);
                    });
            }

            let resolved_args = resolve_inferred_bindings(&parameters, &bindings);
            Type::Parameterized {
                subject: Arc::new(Type::Generic {
                    parameters,
                    subject,
                }),
                arguments: resolved_args,
            }
            .normalize(ctx)
        }
        other => other,
    }
}

/// Walk two types in parallel — a "pattern" type from the generic signature
/// and a "concrete" type from the actual argument — collecting bindings for
/// generic type parameters.
fn infer_generic_bindings(
    pattern: &Type,
    concrete: &Type,
    bindings: &mut Vec<(String, Vec<Type>)>,
) {
    match pattern {
        // Base case: a NamedType that matches a generic parameter name
        Type::NamedType {
            name,
            identifier: None,
        } => {
            if let Some(entry) = bindings.iter_mut().find(|(p, _)| p == name) {
                entry.1.push(concrete.clone());
            }
        }
        // Recurse into function types
        Type::FuncType {
            args: pattern_args,
            returns: pattern_returns,
            ..
        } => {
            if let Type::FuncType {
                args: concrete_args,
                returns: concrete_returns,
                ..
            } = concrete
            {
                pattern_args
                    .iter()
                    .zip(concrete_args.iter())
                    .for_each(|(p, c)| infer_generic_bindings(p, c, bindings));
                infer_generic_bindings(
                    pattern_returns.as_ref(),
                    concrete_returns.as_ref(),
                    bindings,
                );
            }
        }
        // Recurse into arrays
        Type::Array {
            element: pattern_elem,
        } => {
            if let Type::Array {
                element: concrete_elem,
            } = concrete
            {
                infer_generic_bindings(pattern_elem.as_ref(), concrete_elem.as_ref(), bindings);
            }
        }
        // Recurse into tuples pairwise
        Type::Tuple {
            elements: pattern_elems,
        } => {
            if let Type::Tuple {
                elements: concrete_elems,
            } = concrete
            {
                pattern_elems
                    .iter()
                    .zip(concrete_elems.iter())
                    .for_each(|(p, c)| infer_generic_bindings(p, c, bindings));
            }
        }
        // Recurse into object/interface shared fields
        Type::Object {
            fields: pattern_fields,
        }
        | Type::Interface {
            fields: pattern_fields,
            ..
        } => match concrete {
            Type::Object {
                fields: concrete_fields,
            }
            | Type::Interface {
                fields: concrete_fields,
                ..
            } => {
                pattern_fields.iter().for_each(|(key, p_type)| {
                    if let Some(c_type) = concrete_fields.get(key) {
                        infer_generic_bindings(p_type, c_type, bindings);
                    }
                });
            }
            _ => {}
        },
        // Recurse into each variant of a union
        Type::Union {
            variants: pattern_variants,
        } => {
            pattern_variants
                .iter()
                .for_each(|p| infer_generic_bindings(p, concrete, bindings));
        }
        _ => {}
    }
}

/// Convert collected bindings into a final argument list aligned with the
/// generic parameters. Single match → use directly, multiple → union,
/// unmatched → Unknown.
fn resolve_inferred_bindings(parameters: &[String], bindings: &[(String, Vec<Type>)]) -> Vec<Type> {
    parameters
        .iter()
        .map(|param| {
            bindings
                .iter()
                .find(|(name, _)| name == param)
                .map(|(_, types)| match types.len() {
                    0 => Type::Unknown,
                    1 => types[0].clone(),
                    _ => Type::Union {
                        variants: types.clone(),
                    },
                })
                .unwrap_or(Type::Unknown)
        })
        .collect()
}

/// Normalize a unary operation type by computing exact results where possible.
fn normalize_unary_operation(operator: UnaryOperator, operand: Type) -> Type {
    let fits_ctx = FitsContext { modules: None };
    let allowed = operator.allowed_operand_type();

    if !operand.clone().fits(allowed, fits_ctx) {
        return Type::Poisoned;
    }

    match operator {
        UnaryOperator::Not => match as_exact_bool(&operand) {
            Some(value) => (!value).into(),
            None => match operand {
                Type::Nil => Type::TRUE,
                _ => Type::ANY_BOOLEAN,
            },
        },
    }
}
