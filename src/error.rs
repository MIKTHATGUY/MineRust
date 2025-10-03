use thiserror::Error;

/// Main error type for the DataRust compiler
#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Invalid fast type: {0}")]
    InvalidFastType(String),

    #[error("Unsupported operation: {0}")]
    UnsupportedOperation(String),

    #[error("Variable '{0}' not found")]
    VariableNotFound(String),

    #[error("Function '{0}' not found")]
    FunctionNotFound(String),

    #[error("Use after move: Variable '{0}' was moved")]
    UseAfterMove(String),

    #[error("IO error: {0}")]
    IoError(String),

    #[error("Codegen error: {0}")]
    CodegenError(String),
}

/// Warning type for non-fatal issues
#[derive(Debug, Clone)]
pub enum CompileWarning {
    LossyCast {
        from: String,
        to: String,
        line: usize,
    },
    FastFloatApproximation {
        var: String,
        line: usize,
    },
    MixedStorage {
        line: usize,
    },
    UnusedVariable {
        name: String,
        line: usize,
    },
    SlowParsing {
        message: String,
        line: usize,
    },
    NegativeToUnsigned {
        from: String,
        to: String,
        line: usize,
    },
    ConsiderFast {
        var: String,
        line: usize,
    },
}

impl CompileWarning {
    pub fn message(&self) -> String {
        match self {
            CompileWarning::LossyCast { from, to, line } => {
                format!(
                    "Line {}: Warning: Potential truncation in cast from {} to {}",
                    line, from, to
                )
            }
            CompileWarning::FastFloatApproximation { var, line } => {
                format!("Line {}: Warning: Fast float '{}' uses approximation; precision may be lost", line, var)
            }
            CompileWarning::MixedStorage { line } => {
                format!(
                    "Line {}: Warning: Mixing fast and NBT storage may slow execution",
                    line
                )
            }
            CompileWarning::UnusedVariable { name, line } => {
                format!("Line {}: Warning: Unused variable '{}'", line, name)
            }
            CompileWarning::SlowParsing { message, line } => {
                format!("Line {}: Warning: {}", line, message)
            }
            CompileWarning::NegativeToUnsigned { from, to, line } => {
                format!(
                    "Line {}: Warning: Casting negative {} to {} - value may be invalid",
                    line, from, to
                )
            }
            CompileWarning::ConsiderFast { var, line } => {
                format!(
                    "Line {}: Consider using 'fast {}' for better performance in loops",
                    line, var
                )
            }
        }
    }
}
