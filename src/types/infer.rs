use std::sync::Arc;

use crate::{
    ast::{
        container::AST,
        grammar::{BinaryOperator, Expression},
    },
    types::Type,
};

#[derive(Clone, Copy, Debug)]
pub struct InferTypeContext {
    // pub modules: &'a ModulesStore,
    // pub current_module: &'a ParsedModule,
    // pub expressions_encountered: &'a Vec<AST<Expression>>,
}

impl AST<Expression> {
    pub fn infer_type(&self, ctx: InferTypeContext) -> Type {
        use crate::ast::grammar::Expression::*;

        eprintln!("[DEBUG] infer_type({:?})", self);
        eprintln!("[DEBUG]   Slice: {:?}", self.slice());

        match self.details() {
            // Malformed expressions have unknown type
            None => Type::Unknown,

            // Infer type from valid expressions
            Some(_) => match self.unpack() {
                NilLiteral(_) => Type::Nil,

                BooleanLiteral(lit) => Type::ExactBoolean { value: lit.value },

                NumberLiteral(_) => {
                    // Parse the number from the slice to determine if it's an exact type
                    let text = self.slice().as_str();

                    if let Ok(value) = text.parse::<i64>() {
                        Type::ExactNumber { value }
                    } else {
                        Type::Number
                    }
                }

                StringLiteral(str_lit) => {
                    // Infer exact string type from contents
                    Type::ExactString {
                        value: str_lit.contents.clone(),
                    }
                }

                LocalIdentifier(_) => {
                    // Without a symbol table, we can't determine the type
                    // In the future, this would look up the identifier in the context
                    Type::Unknown
                }

                BinaryOperation(bin_op) => {
                    let left_type = bin_op.left.infer_type(ctx);
                    let right_type = bin_op.right.infer_type(ctx);

                    match bin_op.operator.unpack() {
                        BinaryOperator::Add
                        | BinaryOperator::Subtract
                        | BinaryOperator::Multiply
                        | BinaryOperator::Divide => {
                            // For now, assume numeric operations return Number
                            // In the future, we could be more precise with exact numbers
                            match (left_type, right_type) {
                                (
                                    Type::ExactNumber { value: l },
                                    Type::ExactNumber { value: r },
                                ) => {
                                    // Perform constant folding for exact numbers
                                    let result = match bin_op.operator.unpack() {
                                        BinaryOperator::Add => l.checked_add(r),
                                        BinaryOperator::Subtract => l.checked_sub(r),
                                        BinaryOperator::Multiply => l.checked_mul(r),
                                        BinaryOperator::Divide => {
                                            if r != 0 {
                                                l.checked_div(r)
                                            } else {
                                                None
                                            }
                                        }
                                    };
                                    result
                                        .map(|value| Type::ExactNumber { value })
                                        .unwrap_or(Type::Number)
                                }
                                _ => Type::Number,
                            }
                        }
                    }
                }

                Invocation(inv) => {
                    // Infer the function type
                    let func_type = inv.function.infer_type(ctx);

                    // If it's a function type, return its return type
                    match func_type {
                        Type::FuncType { returns, .. } => (*returns).clone(),
                        _ => Type::Unknown,
                    }
                }

                FunctionExpression(func) => {
                    // For now, we don't have enough context to infer parameter types
                    // We'd need a symbol table to track what types the parameters are used as
                    let args = vec![Type::Unknown; func.parameters.len()];
                    let returns = Arc::new(func.body.infer_type(ctx));

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

                    Type::Object {
                        fields,
                        is_open: false,
                    }
                }
            },
        }
    }
}
