use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::collections::{HashMap, HashSet};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Result, Token, braced};

/// A node in the hierarchy type tree - either a leaf or a sub-hierarchy with children
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

    /// Generate code for this hierarchy and all nested hierarchies.
    fn generate(&self, seen_from_impls: &mut HashSet<(String, String)>) -> TokenStream2 {
        if self.is_leaf() {
            return TokenStream2::new();
        }

        let name = &self.name;
        let variant_names: Vec<_> = self.children.iter().map(|c| &c.name).collect();

        // Generate enum definition
        let enum_def = quote! {
            #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

        // Build reachability map: for each type reachable from this node,
        // collect all the direct child branches through which it can be reached.
        // (branch_ident, is_direct_child)
        let mut reachable: HashMap<String, Vec<(Ident, bool)>> = HashMap::new();
        for child in &self.children {
            reachable
                .entry(child.name.to_string())
                .or_default()
                .push((child.name.clone(), true));

            if !child.is_leaf() {
                let mut leaves = Vec::new();
                child.collect_descendant_leaves(&mut leaves);
                for leaf in leaves {
                    reachable
                        .entry(leaf.to_string())
                        .or_default()
                        .push((child.name.clone(), false));
                }
            }
        }

        // Find shared leaves: leaves that appear under multiple branches
        let shared_leaves: HashSet<String> = reachable
            .iter()
            .filter(|(_, branches)| branches.len() > 1)
            .map(|(name, _)| name.clone())
            .collect();

        // For each non-leaf child, determine which sibling branches contain
        // shared leaves that also exist under this child. We need cross-branch
        // TryFrom arms for these.
        //
        // cross_branch_arms[child_name] = vec of TokenStream arms to add to
        // TryFrom<Self> for ChildName
        let mut cross_branch_try_from: HashMap<String, Vec<TokenStream2>> = HashMap::new();
        for child in &self.children {
            if child.is_leaf() {
                continue;
            }

            let child_name = &child.name;
            let mut my_leaves = Vec::new();
            child.collect_descendant_leaves(&mut my_leaves);
            let my_leaf_set: HashSet<String> = my_leaves.iter().map(|l| l.to_string()).collect();

            // For each shared leaf under this child, find sibling branches
            // that also contain it
            for leaf_name in my_leaf_set.iter().filter(|l| shared_leaves.contains(*l)) {
                let leaf_ident: Ident =
                    syn::parse_str(leaf_name).expect("valid ident");

                if let Some(branches) = reachable.get(leaf_name) {
                    for (sibling_branch, sibling_is_direct) in branches {
                        if sibling_branch.to_string() == child_name.to_string() {
                            continue; // skip self
                        }

                        let arms = cross_branch_try_from
                            .entry(child_name.to_string())
                            .or_default();

                        if *sibling_is_direct {
                            // The leaf IS the sibling branch (leaf is direct child)
                            arms.push(quote! {
                                #name::#sibling_branch(variant) => {
                                    Ok(#child_name::from(variant))
                                }
                            });
                        } else {
                            // The leaf is nested under the sibling branch
                            arms.push(quote! {
                                #name::#sibling_branch(variant) => {
                                    let leaf: #leaf_ident = variant.try_into().map_err(|_| ())?;
                                    Ok(#child_name::from(leaf))
                                }
                            });
                        }
                    }
                }
            }
        }

        let mut impls = Vec::new();

        for (target_name, branches) in &reachable {
            let target_type: Ident =
                syn::parse_str(target_name).expect("valid identifier");

            // --- From impl (first path wins, Rust only allows one) ---
            let from_key = (target_name.clone(), name.to_string());
            if seen_from_impls.insert(from_key) {
                let (ref first_branch, first_is_direct) = branches[0];
                if first_is_direct {
                    impls.push(quote! {
                        impl From<#target_type> for #name {
                            fn from(variant: #target_type) -> Self {
                                #name::#first_branch(variant)
                            }
                        }
                    });
                } else {
                    impls.push(quote! {
                        impl From<#target_type> for #name {
                            fn from(variant: #target_type) -> Self {
                                #name::#first_branch(<#first_branch>::from(variant))
                            }
                        }
                    });
                }
            }

            // --- TryFrom impl ---
            // Primary match arms from the reachability map
            let mut match_arms: Vec<TokenStream2> = branches
                .iter()
                .map(|(branch, is_direct)| {
                    if *is_direct {
                        quote! {
                            #name::#branch(variant) => Ok(variant)
                        }
                    } else {
                        quote! {
                            #name::#branch(variant) => variant.try_into().map_err(|_| ())
                        }
                    }
                })
                .collect();

            // Add cross-branch arms if this is a non-leaf child with shared leaves
            if let Some(extra_arms) = cross_branch_try_from.get(target_name) {
                match_arms.extend(extra_arms.iter().cloned());
            }

            impls.push(quote! {
                impl TryFrom<#name> for #target_type {
                    type Error = ();

                    fn try_from(un: #name) -> Result<Self, Self::Error> {
                        match un {
                            #(#match_arms,)*
                            _ => Err(())
                        }
                    }
                }
            });
        }

        // Recursively generate nested unions
        let nested: Vec<_> = self
            .children
            .iter()
            .map(|c| c.generate(seen_from_impls))
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
/// A leaf type may appear under multiple branches (e.g., `ConstDeclaration` under both
/// `Declaration` and `Statement`). In this case:
/// - `From` uses the first path encountered (Rust only allows one `From` impl per pair)
/// - `TryFrom` generates match arms for **all** paths, so downcasting succeeds regardless
///   of which branch the value was constructed through. This includes cross-branch
///   recovery for intermediate enum types (e.g., `TryFrom<Any> for Statement` can extract
///   a `ConstDeclaration` stored under `Any::Declaration`).
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
    let mut seen_from_impls = HashSet::new();
    node.generate(&mut seen_from_impls).into()
}
