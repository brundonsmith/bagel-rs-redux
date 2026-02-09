use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::collections::HashSet;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Result, Token, braced};

/// A node in the hiererchy type tree - either a leaf or a sub-hiererchy with children
struct HierarchyNode {
    name: Ident,
    children: Vec<HierarchyNode>,
}

impl Parse for HierarchyNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        let children = if input.peek(syn::token::Brace) {
            let content;
            braced!(content in input);
            let mut children = Vec::new();
            while !content.is_empty() {
                children.push(content.parse()?);
                if content.peek(Token![,]) {
                    content.parse::<Token![,]>()?;
                }
            }
            children
        } else {
            Vec::new()
        };
        Ok(HierarchyNode { name, children })
    }
}

impl HierarchyNode {
    fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    /// Collect all descendant leaf names (not including self)
    fn collect_descendant_leaves(&self, leaves: &mut Vec<Ident>) {
        for child in &self.children {
            if child.is_leaf() {
                leaves.push(child.name.clone());
            } else {
                child.collect_descendant_leaves(leaves);
            }
        }
    }

    /// Generate code for this hiererchy and all nested hiererchys
    fn generate(&self, seen_impls: &mut HashSet<(String, String)>) -> TokenStream2 {
        if self.is_leaf() {
            return TokenStream2::new();
        }

        let name = &self.name;
        let variant_names: Vec<_> = self.children.iter().map(|c| &c.name).collect();

        // Generate enum
        let enum_def = quote! {
            #[derive(Clone, PartialEq, Eq, Hash)]
            pub enum #name {
                #(#variant_names(#variant_names)),*
            }

            impl std::fmt::Debug for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #(#name::#variant_names(inner) => std::fmt::Debug::fmt(inner, f)),*
                    }
                }
            }
        };

        // Generate From/TryFrom for direct children
        let mut impls = Vec::new();
        for child in &self.children {
            let child_name = &child.name;
            let key = (child_name.to_string(), name.to_string());

            if seen_impls.insert(key) {
                impls.push(quote! {
                    impl From<#child_name> for #name {
                        fn from(variant: #child_name) -> Self {
                            #name::#child_name(variant)
                        }
                    }

                    impl TryFrom<#name> for #child_name {
                        type Error = ();

                        fn try_from(un: #name) -> Result<Self, Self::Error> {
                            match un {
                                #name::#child_name(variant) => Ok(variant),
                                _ => Err(())
                            }
                        }
                    }
                });
            }

            // Generate transitive impls for all descendants through this child
            if !child.is_leaf() {
                let mut descendant_leaves = Vec::new();
                child.collect_descendant_leaves(&mut descendant_leaves);

                for leaf in descendant_leaves {
                    let key = (leaf.to_string(), name.to_string());
                    if seen_impls.insert(key) {
                        impls.push(quote! {
                            impl From<#leaf> for #name {
                                fn from(variant: #leaf) -> Self {
                                    #name::#child_name(<#child_name>::from(variant))
                                }
                            }

                            impl TryFrom<#name> for #leaf {
                                type Error = ();

                                fn try_from(un: #name) -> Result<Self, Self::Error> {
                                    match un {
                                        #name::#child_name(variant) => variant.try_into().map_err(|_| ()),
                                        _ => Err(())
                                    }
                                }
                            }
                        });
                    }
                }
            }
        }

        // Recursively generate nested unions
        let nested: Vec<_> = self
            .children
            .iter()
            .map(|c| c.generate(seen_impls))
            .collect();

        quote! {
            #enum_def
            #(#impls)*
            #(#nested)*
        }
    }
}

/// Generates enum hierarchies with automatic `From` and `TryFrom` implementations.
///
/// As documented in GRAMMAR.md, the `type_hierarchy!` macro generates enum hierarchies
/// that wrap AST nodes at different levels of abstraction. For example:
///
/// - The top-level type (e.g., `Any`) is an enum that can hold any node in the tree
/// - Mid-level types (e.g., `Expression`) are enums that can hold any expression node
/// - Leaf types (e.g., `NumberLiteral`) are concrete structs defined elsewhere
///
/// # Generated Code
///
/// The macro generates:
/// - **Enum definitions** for each non-leaf level with variants for each child
/// - **`From` implementations** for upcasting (e.g., `NumberLiteral` -> `Expression` -> `Any`)
/// - **`TryFrom` implementations** for downcasting (e.g., `Any` -> `Expression` -> `NumberLiteral`)
///
/// This allows type-safe casting throughout the codebase via `upcast()` and `try_downcast()`.
///
/// # Example
///
/// ```ignore
/// type_hierarchy! {
///     Any {
///         Expression {
///             NumberLiteral,
///             BooleanLiteral,
///             BinaryOperation,
///         },
///         Declaration,
///     }
/// }
/// ```
///
/// Generates:
/// - `enum Any { Expression(Expression), Declaration(Declaration) }`
/// - `enum Expression { NumberLiteral(NumberLiteral), BooleanLiteral(BooleanLiteral), ... }`
/// - `From<NumberLiteral> for Expression`
/// - `From<Expression> for Any`
/// - `From<NumberLiteral> for Any` (transitive)
/// - And corresponding `TryFrom` implementations for downcasting
#[proc_macro]
pub fn type_hierarchy(input: TokenStream) -> TokenStream {
    let node = syn::parse_macro_input!(input as HierarchyNode);
    let mut seen_impls = HashSet::new();
    node.generate(&mut seen_impls).into()
}
