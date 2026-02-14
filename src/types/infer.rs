use std::sync::Arc;

use crate::{
    ast::{
        container::AST,
        grammar::{ElseClause, Expression, FunctionBody},
        modules::{Module, ModulesStore},
    },
    types::{NormalizeContext, Type},
};

#[derive(Clone, Copy, Debug)]
pub struct InferTypeContext<'a> {
    pub modules: Option<&'a ModulesStore>,
    pub current_module: Option<&'a Module>,
}

impl AST<Expression> {
    pub fn infer_type(&self, ctx: InferTypeContext<'_>) -> Type {
        use crate::ast::grammar::Expression::*;

        eprintln!("[DEBUG] infer_type({:?})", self);
        eprintln!("[DEBUG]   Slice: {:?}", self.slice());

        match self.unpack() {
            // Malformed expressions have unknown type
            None => Type::Poisoned,

            // Infer type from valid expressions
            Some(expr) => match expr {
                NilLiteral(_) => Type::Nil,

                BooleanLiteral(lit) => Type::Boolean {
                    value: Some(lit.value),
                },

                NumberLiteral(_) => {
                    // Parse the number from the slice to determine if it's an exact type
                    let text = self.slice().as_str();

                    if let Ok(value) = text.parse::<i64>() {
                        Type::Number {
                            min_value: Some(value),
                            max_value: Some(value),
                        }
                    } else {
                        Type::ANY_NUMBER
                    }
                }

                StringLiteral(str_lit) => Type::String {
                    value: Some(str_lit.contents.clone()),
                },

                LocalIdentifier(local_id) => Type::LocalIdentifier {
                    identifier: local_id,
                },

                BinaryOperation(bin_op) => Type::BinaryOperation {
                    operator: bin_op.operator.unpack().unwrap(),
                    left: Arc::new(bin_op.left.infer_type(ctx)),
                    right: Arc::new(bin_op.right.infer_type(ctx)),
                },

                UnaryOperation(unary_op) => Type::UnaryOperation {
                    operator: unary_op.operator.unpack().unwrap(),
                    operand: Arc::new(unary_op.operand.infer_type(ctx)),
                },

                Invocation(inv) => Type::Invocation {
                    function: Arc::new(inv.function.infer_type(ctx)),
                    args: inv.arguments.iter().map(|a| a.infer_type(ctx)).collect(),
                },

                FunctionExpression(func) => {
                    let norm_ctx = NormalizeContext {
                        modules: ctx.modules,
                        current_module: ctx.current_module,
                    };
                    let expected_args =
                        self.expected_type()
                            .and_then(|t| match t.normalize(norm_ctx) {
                                Type::FuncType { args, .. } => Some(args),
                                _ => None,
                            });

                    let args = func
                        .parameters
                        .iter()
                        .enumerate()
                        .map(|(i, (_name, type_ann))| {
                            type_ann
                                .as_ref()
                                .and_then(|(_, t)| t.unpack().map(Type::from))
                                .or_else(|| {
                                    expected_args.as_ref().and_then(|ea| ea.get(i).cloned())
                                })
                                .unwrap_or(Type::Unknown)
                        })
                        .collect();
                    let returns = Arc::new(match &func.return_type {
                        Some((_colon, ret_type)) => {
                            ret_type.unpack().map(Type::from).unwrap_or(Type::Poisoned)
                        }
                        None => match func.body.unpack() {
                            Some(FunctionBody::Expression(expr)) => expr.infer_type(ctx),
                            Some(FunctionBody::Block(_)) => Type::Never,
                            None => Type::Poisoned,
                        },
                    });

                    Type::FuncType {
                        args,
                        args_spread: None,
                        returns,
                    }
                }

                ArrayLiteral(arr) => {
                    // Array literals infer as tuple types with exact element types
                    if arr.elements.is_empty() {
                        // Empty array is a tuple with no elements
                        Type::Tuple { elements: vec![] }
                    } else {
                        // Get types of all elements
                        let element_types: Vec<Type> = arr
                            .elements
                            .iter()
                            .map(|elem| elem.infer_type(ctx))
                            .collect();

                        Type::Tuple {
                            elements: element_types,
                        }
                    }
                }

                ObjectLiteral(obj) => {
                    // Infer object type from fields
                    let mut fields = std::collections::BTreeMap::new();
                    for (key, _, value) in &obj.fields {
                        let field_name = key.slice().as_str().to_string();
                        let field_type = value.infer_type(ctx);
                        fields.insert(field_name, field_type);
                    }

                    Type::Object { fields }
                }

                IfElseExpression(if_else) => {
                    let condition_type = if_else.condition.infer_type(ctx);
                    let consequent_type = if_else.consequent.infer_type(ctx);

                    let alternate_type = match &if_else.else_clause {
                        Some(ElseClause::ElseBlock { expression, .. }) => {
                            expression.infer_type(ctx)
                        }
                        Some(ElseClause::ElseIf {
                            if_else: nested, ..
                        }) => {
                            let nested_as_expr: AST<Expression> = nested.clone().upcast();
                            nested_as_expr.infer_type(ctx)
                        }
                        None => Type::Nil,
                    };

                    Type::IfElse {
                        condition: Arc::new(condition_type),
                        consequent: Arc::new(consequent_type),
                        alternate: Arc::new(alternate_type),
                    }
                }

                PropertyAccessExpression(prop_access) => match prop_access.property {
                    AST::Valid(inner, _) => Type::PropertyAccess {
                        subject: Arc::new(prop_access.subject.infer_type(ctx)),
                        property: inner.slice.as_str().to_string(),
                    },
                    AST::Malformed(_, _) => Type::Poisoned,
                },

                ParenthesizedExpression(paren) => paren.expression.infer_type(ctx),
            },
        }
    }
}
