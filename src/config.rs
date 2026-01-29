use serde::Deserialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Config {
    pub rules: Rules,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Rules {
    sorted_imports: RuleSeverityOrOff,
    no_redundant_parenthesis: RuleSeverity,
}

impl Default for Rules {
    fn default() -> Self {
        Self {
            sorted_imports: RuleSeverityOrOff::Autofix,
            no_redundant_parenthesis: RuleSeverity::Autofix,
        }
    }
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
