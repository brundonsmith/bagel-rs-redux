use serde::Deserialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
pub struct Config {
    pub rules: Rules,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Rules {
    /// Whether and at what count to enforce indentation
    indentation: IndentationConfig,

    /// Whether and at what width to enforce maximum line width
    line_width: LineWidthConfig,

    /// Require that top-level constant names in modules are in lower camel case
    lower_camel_case_module_constants: RuleSeverityOrOff,

    /// Require that top-level function names in modules are in lower camel case
    lower_camel_case_module_functions: RuleSeverityOrOff,

    /// Require that top-level type names in modules are in upper camel case
    upper_camel_case_module_types: RuleSeverityOrOff,

    /// Require that local variables and constants are in lower camel case
    lower_camel_case_module_local_variables: RuleSeverityOrOff,

    /// Require that all imports are at the top of the file, and sorted:
    /// - The first section is HTTP imports, followed by a blank line, followed
    ///   by another section for relative imports
    /// - Within each section, imports are sorted alphabetically by path
    /// - Within each group of imported items, items are sorted alphabetically
    ///   by name
    sorted_imports: RuleSeverityOrOff,

    /// Require that all imports (items and entire statements) be used
    /// somewhere in the module
    no_unused_imports: RuleSeverityOrOff,

    /// If a pair of parenthesis will have no effect on behavior (due to
    /// precedence) or otherwise, they shouldn't be there
    no_unnecessary_parenthesis: RuleSeverityOrOff,

    /// If a function is inferred to be pure, require the explicit `pure`
    /// keyword to be on it
    explicit_pure_functions: RuleSeverityOrOff,

    /// If a function body isn't a single expression, but could be, require
    /// that it must be
    use_expressions_where_possible: RuleSeverityOrOff,

    /// Require that every non-exported local declaration be referenced from
    /// somewhere.
    ///
    /// Can be set to "module", which only applies to
    /// non-exported declarations, or can be set to "project", which assumes
    /// an export that isn't used in the current project isn't used at all
    /// (use "module" for libraries).
    no_dead_code: NoDeadCodeConfig,

    /// Require that every mutable variable (declared with `let`) is mutated
    /// somewhere. Auto-fix for this rule will replace those cases of `let`
    /// with `const`.
    ///
    /// Can be set to "module", which only applies to
    /// non-exported declarations, or can be set to "project", which assumes
    /// an export that isn't used in the current project isn't used at all
    /// (use "module" for libraries).
    no_unnecessary_mutable_variables: RuleSeverityOrOff,

    /// Require that the nil coalescing operator's left operand be
    /// possibly-nil (otherwise the operator does nothing).
    no_unnecessary_nil_coalescing: RuleSeverityOrOff,

    /// Require that all conditionals have the possibility to be either true
    /// or false (not guaranteed to be just one or the other, based on type
    /// information)
    no_unnecessary_conditional: RuleSeverityOrOff,

    /// Standardize module-level function declarations to either be all plain
    /// const declarations, or all use the dedicated function syntax.
    ///
    /// Const declarations that have a type definition which can't be
    /// represented in the dedicated function syntax (eg, a named function
    /// type) will be ignored by this rule.
    function_syntax: FunctionSyntaxConfig,
}

impl Default for Rules {
    fn default() -> Self {
        use FunctionSyntax::*;
        use Locality::*;
        use RuleSeverityOrOff::*;

        Self {
            indentation: IndentationConfig {
                severity: Autofix,
                spaces: 4,
            },
            line_width: LineWidthConfig {
                severity: Autofix,
                width: 80,
            },
            lower_camel_case_module_constants: Autofix,
            lower_camel_case_module_functions: Autofix,
            upper_camel_case_module_types: Autofix,
            lower_camel_case_module_local_variables: Autofix,
            sorted_imports: Autofix,
            no_unused_imports: Autofix,
            no_unnecessary_parenthesis: Autofix,
            explicit_pure_functions: Autofix,
            use_expressions_where_possible: Autofix,
            no_dead_code: NoDeadCodeConfig {
                severity: Warn,
                locality: Project,
            },
            no_unnecessary_mutable_variables: Autofix,
            no_unnecessary_nil_coalescing: Autofix,
            no_unnecessary_conditional: Error,
            function_syntax: FunctionSyntaxConfig {
                severity: Autofix,
                function_syntax: Function,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct IndentationConfig {
    severity: RuleSeverityOrOff,
    spaces: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct LineWidthConfig {
    severity: RuleSeverityOrOff,
    width: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct NoDeadCodeConfig {
    severity: RuleSeverityOrOff,
    locality: Locality,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum Locality {
    Module,
    Project,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct FunctionSyntaxConfig {
    severity: RuleSeverityOrOff,
    function_syntax: FunctionSyntax,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum FunctionSyntax {
    /// Plain const syntax
    Const,

    /// Named function syntax
    Function,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum RuleSeverity {
    Warn,
    Error,
    Autofix,
}

impl TryFrom<RuleSeverityOrOff> for RuleSeverity {
    type Error = &'static str;

    fn try_from(value: RuleSeverityOrOff) -> Result<Self, &'static str> {
        match value {
            RuleSeverityOrOff::Off => Err("Can't represent Off as a RuleSeverity"),
            RuleSeverityOrOff::Warn => Ok(Self::Warn),
            RuleSeverityOrOff::Error => Ok(Self::Error),
            RuleSeverityOrOff::Autofix => Ok(Self::Autofix),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum RuleSeverityOrOff {
    Off,
    Warn,
    Error,
    Autofix,
}

impl From<RuleSeverity> for RuleSeverityOrOff {
    fn from(value: RuleSeverity) -> Self {
        match value {
            RuleSeverity::Warn => Self::Warn,
            RuleSeverity::Error => Self::Error,
            RuleSeverity::Autofix => Self::Autofix,
        }
    }
}
