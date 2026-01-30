use crate::types::Type;

#[derive(Debug, Clone, Copy)]
pub struct FitsContext {}

impl Type {
    /// Convenience wrapper for seeing if a type fit is allowed
    pub fn fits(self, destination: Type, ctx: FitsContext) -> bool {
        self.fit_issues(destination, ctx).is_empty()
    }

    /// Determines whether a value type (self) "fits" into a destination type
    /// (destination). If it doesn't fit, one or more errors will be returned
    /// in a Vec. Most type incompatibilities will only return one message,
    /// but a stack of errors can be returned to communicate, eg, which
    /// property within an object is incompatible with the object type
    /// (even recursively).
    ///
    /// Similar to TypeScript, each type represents a set of possible values,
    /// and the value type fits into the destination if the destination's set
    /// of values includes *at least* all the ones in the value's set of
    /// values.
    pub fn fit_issues(self, destination: Type, _ctx: FitsContext) -> Vec<String> {
        let value = self.normalize();
        let destination = destination.normalize();

        // Base case; types are identical
        if value == destination {
            return vec![];
        }

        // Any accepts everything
        if matches!(destination, Type::Any) {
            return vec![];
        }

        // Nothing fits into Never (it's the bottom type)
        if matches!(destination, Type::Never) {
            return vec![format!(
                "Type '{}' is not assignable to type 'never'.",
                value
            )];
        }

        // Never fits into everything (it's the bottom type)
        if matches!(value, Type::Never) {
            return vec![];
        }

        // Unknown only fits into Any or Unknown (already handled above)
        if matches!(value, Type::Unknown) {
            return vec![format!(
                "Type 'unknown' is not assignable to type '{}'.",
                destination
            )];
        }

        match (&value, &destination) {
            // Exact boolean fits into general boolean
            (Type::Boolean { value: Some(_) }, Type::Boolean { value: None }) => vec![],

            // Exact/bounded number fits into general number or a wider range
            (
                Type::Number { min_value: val_min, max_value: val_max },
                Type::Number { min_value: dest_min, max_value: dest_max },
            ) => {
                let min_ok = match (val_min, dest_min) {
                    (_, None) => true, // destination has no lower bound
                    (Some(v), Some(d)) => v >= d,
                    (None, Some(_)) => false, // value unbounded but dest has bound
                };
                let max_ok = match (val_max, dest_max) {
                    (_, None) => true, // destination has no upper bound
                    (Some(v), Some(d)) => v <= d,
                    (None, Some(_)) => false,
                };
                if min_ok && max_ok {
                    vec![]
                } else {
                    vec![format!(
                        "Type '{}' is not assignable to type '{}'.",
                        value, destination
                    )]
                }
            }

            // Exact string fits into general string
            (Type::String { value: Some(_) }, Type::String { value: None }) => vec![],

            // Array type compatibility: element types must be compatible
            (Type::Array { element: val_elem }, Type::Array { element: dest_elem }) => {
                let elem_issues = val_elem
                    .as_ref()
                    .clone()
                    .fit_issues(dest_elem.as_ref().clone(), _ctx);
                if elem_issues.is_empty() {
                    vec![]
                } else {
                    vec![format!(
                        "Type '{}[]' is not assignable to type '{}[]'.",
                        val_elem, dest_elem
                    )]
                }
            }

            // Tuple type compatibility: must have same length and each element must fit
            (
                Type::Tuple {
                    elements: val_elems,
                },
                Type::Tuple {
                    elements: dest_elems,
                },
            ) => {
                if val_elems.len() != dest_elems.len() {
                    return vec![format!(
                        "Tuple of length {} is not assignable to tuple of length {}.",
                        val_elems.len(),
                        dest_elems.len()
                    )];
                }

                for (i, (val_elem, dest_elem)) in
                    val_elems.iter().zip(dest_elems.iter()).enumerate()
                {
                    let elem_issues = val_elem.clone().fit_issues(dest_elem.clone(), _ctx);
                    if !elem_issues.is_empty() {
                        return vec![format!(
                            "Element at index {} is not compatible: {}",
                            i, elem_issues[0]
                        )];
                    }
                }
                vec![]
            }

            // Object type compatibility: value must have at least all fields of destination
            // Destination fields must be compatible with value fields
            (
                Type::Object {
                    fields: val_fields,
                    is_open: val_open,
                },
                Type::Object {
                    fields: dest_fields,
                    is_open: dest_open,
                },
            ) => {
                // Check that all required destination fields exist in value and are compatible
                for (key, dest_type) in dest_fields {
                    if let Some(val_type) = val_fields.get(key) {
                        let field_issues = val_type.clone().fit_issues(dest_type.clone(), _ctx);
                        if !field_issues.is_empty() {
                            return vec![format!(
                                "Property '{}' is not compatible: {}",
                                key, field_issues[0]
                            )];
                        }
                    } else {
                        return vec![format!(
                            "Property '{}' is missing in type '{}'.",
                            key, value
                        )];
                    }
                }

                // If destination is closed (not jopen), value cannot have extra fields
                if !dest_open {
                    for key in val_fields.keys() {
                        if !dest_fields.contains_key(key) {
                            return vec![format!(
                                "Object literal may only specify known properties, and '{}' does not exist in type '{}'.",
                                key, destination
                            )];
                        }
                    }
                }

                vec![]
            }

            // Function type compatibility
            (
                Type::FuncType {
                    args: val_args,
                    args_spread: val_spread,
                    returns: val_returns,
                },
                Type::FuncType {
                    args: dest_args,
                    args_spread: dest_spread,
                    returns: dest_returns,
                },
            ) => {
                // For functions, parameters are contravariant and return type is covariant
                // This means: destination parameters must fit into value parameters
                // And: value return type must fit into destination return type

                // Check parameter count compatibility
                if val_args.len() < dest_args.len() && val_spread.is_none() {
                    return vec![format!(
                        "Function with {} parameters is not assignable to function with {} parameters.",
                        val_args.len(),
                        dest_args.len()
                    )];
                }

                // Check parameter types (contravariant)
                for (i, (val_param, dest_param)) in
                    val_args.iter().zip(dest_args.iter()).enumerate()
                {
                    let param_issues = dest_param.clone().fit_issues(val_param.clone(), _ctx);
                    if !param_issues.is_empty() {
                        return vec![format!(
                            "Parameter {} is not compatible: {}",
                            i, param_issues[0]
                        )];
                    }
                }

                // Check return type (covariant)
                let return_issues = val_returns
                    .as_ref()
                    .clone()
                    .fit_issues(dest_returns.as_ref().clone(), _ctx);
                if !return_issues.is_empty() {
                    return vec![format!(
                        "Return type is not compatible: {}",
                        return_issues[0]
                    )];
                }

                vec![]
            }

            // Union type as destination: value must fit into at least one variant
            (
                _,
                Type::Union {
                    variants: dest_variants,
                },
            ) => {
                for variant in dest_variants {
                    let issues = value.clone().fit_issues(variant.clone(), _ctx);
                    if issues.is_empty() {
                        return vec![]; // Fits into at least one variant
                    }
                }
                vec![format!(
                    "Type '{}' is not assignable to type '{}'.",
                    value, destination
                )]
            }

            // Union type as value: all variants must fit into destination
            (
                Type::Union {
                    variants: val_variants,
                },
                _,
            ) => {
                for variant in val_variants {
                    let issues = variant.clone().fit_issues(destination.clone(), _ctx);
                    if !issues.is_empty() {
                        return vec![format!(
                            "Type '{}' is not assignable to type '{}'. Not all union members are compatible.",
                            value, destination
                        )];
                    }
                }
                vec![]
            }

            // Default case: types are incompatible
            _ => vec![format!(
                "Type '{}' is not assignable to type '{}'.",
                value, destination
            )],
        }
    }
}
