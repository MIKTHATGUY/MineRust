/// Type checker module for DataRust
use crate::ast::*;
use crate::error::{CompileError, CompileWarning};
use std::collections::HashMap;

pub struct TypeChecker {
    /// Symbol table for variables
    variables: HashMap<String, Type>,
    /// Symbol table for functions
    functions: HashMap<String, (Vec<Type>, Option<Type>)>,
    /// Warnings collected during type checking
    warnings: Vec<CompileWarning>,
    /// Whether to treat warnings as errors
    warn_as_error: bool,
}

pub fn check(program: Program, _source: &str, warn_as_error: bool) -> Result<Program, CompileError> {
    let mut checker = TypeChecker {
        variables: HashMap::new(),
        functions: HashMap::new(),
        warnings: Vec::new(),
        warn_as_error,
    };

    checker.check_program(program)
}

impl TypeChecker {
    fn check_program(&mut self, mut program: Program) -> Result<Program, CompileError> {
        // First pass: collect function signatures
        for item in &program.items {
            if let Item::Function(func) = item {
                let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
                self.functions.insert(
                    func.name.clone(),
                    (param_types, func.return_type.clone())
                );
            }
        }

        // Second pass: type check each item
        for item in &mut program.items {
            match item {
                Item::Function(func) => self.check_function(func)?,
                Item::Struct(_) => {} // Structs don't need runtime checks
                _ => {}
            }
        }

        // Report warnings
        for warning in &self.warnings {
            eprintln!("{}", warning.message());
            if self.warn_as_error {
                return Err(CompileError::TypeError(warning.message()));
            }
        }

        Ok(program)
    }

    fn check_function(&mut self, func: &mut Function) -> Result<(), CompileError> {
        // Add parameters to scope
        for param in &func.params {
            // Check if fast storage is compatible
            if let Type::Fast(inner_ty, _) = &param.ty {
                if !inner_ty.is_fast_compatible() {
                    return Err(CompileError::InvalidFastType(format!(
                        "fast {:?} is not supported; scoreboards are 32-bit and can't handle this type",
                        inner_ty
                    )));
                }
                
                // Warn about fast floats
                if inner_ty.is_float() {
                    self.warnings.push(CompileWarning::FastFloatApproximation {
                        var: param.name.clone(),
                        line: 0, // TODO: track line numbers
                    });
                }
            }
            
            self.variables.insert(param.name.clone(), param.ty.clone());
        }

        // Type check function body
        let body_ty = self.check_expr(&mut func.body)?;

        // Check return type matches
        if let Some(ref return_ty) = func.return_type {
            if !self.types_compatible(&body_ty, return_ty) {
                return Err(CompileError::TypeError(format!(
                    "Function '{}' return type mismatch: expected {:?}, got {:?}",
                    func.name, return_ty, body_ty
                )));
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &mut Expr) -> Result<Type, CompileError> {
        let ty = match &mut expr.kind {
            ExprKind::IntLit(_) => Type::I32,
            ExprKind::FloatLit(_) => Type::F64,
            ExprKind::BoolLit(_) => Type::Bool,
            ExprKind::StringLit(_) => Type::Str,
            ExprKind::CharLit(_) => Type::Char,

            ExprKind::Var(name) => {
                self.variables.get(name)
                    .cloned()
                    .ok_or_else(|| CompileError::VariableNotFound(name.clone()))?
            }

            ExprKind::BinaryOp { op, left, right } => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;
                self.check_binary_op(*op, &left_ty, &right_ty)?
            }

            ExprKind::UnaryOp { op, expr: inner } => {
                let inner_ty = self.check_expr(inner)?;
                self.check_unary_op(*op, &inner_ty)?
            }

            ExprKind::Let { name, ty, value } => {
                let value_ty = self.check_expr(value)?;
                
                let var_ty = if let Some(ty) = ty {
                    // Check if fast storage is compatible
                    if let Type::Fast(inner_ty, _) = ty {
                        if !inner_ty.is_fast_compatible() {
                            return Err(CompileError::InvalidFastType(format!(
                                "fast {:?} is not supported; scoreboards are 32-bit",
                                inner_ty
                            )));
                        }
                        
                        // Check for unsupported types
                        match **inner_ty {
                            Type::I64 | Type::U64 => {
                                return Err(CompileError::InvalidFastType(
                                    "fast i64/u64 not supported; scoreboards are 32-bit".to_string()
                                ));
                            }
                            Type::Str | Type::Char => {
                                return Err(CompileError::InvalidFastType(
                                    "fast str/char not supported; use NBT storage".to_string()
                                ));
                            }
                            _ => {}
                        }
                        
                        // Warn about fast floats
                        if inner_ty.is_float() {
                            self.warnings.push(CompileWarning::FastFloatApproximation {
                                var: name.clone(),
                                line: 0,
                            });
                        }
                    }
                    
                    // Check compatibility
                    if !self.types_compatible(&value_ty, ty) {
                        // Warn if lossy cast
                        if self.is_lossy_cast(&value_ty, ty) {
                            self.warnings.push(CompileWarning::LossyCast {
                                from: format!("{:?}", value_ty),
                                to: format!("{:?}", ty),
                                line: 0,
                            });
                        } else {
                            return Err(CompileError::TypeError(format!(
                                "Type mismatch in let binding: expected {:?}, got {:?}",
                                ty, value_ty
                            )));
                        }
                    }
                    
                    ty.clone()
                } else {
                    value_ty
                };
                
                self.variables.insert(name.clone(), var_ty.clone());
                var_ty
            }

            ExprKind::Assign { target, value } => {
                let value_ty = self.check_expr(value)?;
                let var_ty = self.variables.get(target)
                    .ok_or_else(|| CompileError::VariableNotFound(target.clone()))?;
                
                if !self.types_compatible(&value_ty, var_ty) {
                    return Err(CompileError::TypeError(format!(
                        "Type mismatch in assignment: expected {:?}, got {:?}",
                        var_ty, value_ty
                    )));
                }
                
                value_ty
            }

            ExprKind::Call { name, args } => {
                let (param_types, return_ty) = self.functions.get(name)
                    .ok_or_else(|| CompileError::FunctionNotFound(name.clone()))?
                    .clone();
                
                if args.len() != param_types.len() {
                    return Err(CompileError::TypeError(format!(
                        "Function '{}' expects {} arguments, got {}",
                        name, param_types.len(), args.len()
                    )));
                }
                
                for (arg, expected_ty) in args.iter_mut().zip(param_types.iter()) {
                    let arg_ty = self.check_expr(arg)?;
                    if !self.types_compatible(&arg_ty, expected_ty) {
                        return Err(CompileError::TypeError(format!(
                            "Argument type mismatch: expected {:?}, got {:?}",
                            expected_ty, arg_ty
                        )));
                    }
                }
                
                return_ty.unwrap_or(Type::I32)
            }

            ExprKind::Cast { expr: inner, target_ty } => {
                let inner_ty = self.check_expr(inner)?;
                
                // Check if cast is valid
                if !self.can_cast(&inner_ty, target_ty) {
                    return Err(CompileError::TypeError(format!(
                        "Invalid cast from {:?} to {:?}",
                        inner_ty, target_ty
                    )));
                }
                
                // Warn if lossy
                if self.is_lossy_cast(&inner_ty, target_ty) {
                    self.warnings.push(CompileWarning::LossyCast {
                        from: format!("{:?}", inner_ty),
                        to: format!("{:?}", target_ty),
                        line: 0,
                    });
                }
                
                target_ty.clone()
            }

            ExprKind::Block(exprs) => {
                let mut last_ty = Type::I32; // Default unit type
                for expr in exprs {
                    last_ty = self.check_expr(expr)?;
                }
                last_ty
            }

            ExprKind::If { condition, then_branch, else_branch } => {
                let cond_ty = self.check_expr(condition)?;
                if cond_ty != Type::Bool {
                    return Err(CompileError::TypeError(
                        "If condition must be bool".to_string()
                    ));
                }
                
                let then_ty = self.check_expr(then_branch)?;
                
                if let Some(else_expr) = else_branch {
                    let else_ty = self.check_expr(else_expr)?;
                    if !self.types_compatible(&then_ty, &else_ty) {
                        return Err(CompileError::TypeError(format!(
                            "If branches have incompatible types: {:?} vs {:?}",
                            then_ty, else_ty
                        )));
                    }
                }
                
                then_ty
            }

            _ => {
                return Err(CompileError::TypeError(
                    "Unsupported expression type".to_string()
                ));
            }
        };

        expr.ty = Some(ty.clone());
        Ok(ty)
    }

    fn check_binary_op(&mut self, op: BinOp, left_ty: &Type, right_ty: &Type) -> Result<Type, CompileError> {
        // Check if both types are numeric
        if !left_ty.is_numeric() || !right_ty.is_numeric() {
            return Err(CompileError::TypeError(format!(
                "Binary operation {:?} requires numeric types, got {:?} and {:?}",
                op, left_ty, right_ty
            )));
        }

        // Check for mixed storage (fast + NBT)
        let left_is_fast = matches!(left_ty, Type::Fast(_, _));
        let right_is_fast = matches!(right_ty, Type::Fast(_, _));
        if left_is_fast != right_is_fast {
            self.warnings.push(CompileWarning::MixedStorage { line: 0 });
        }

        // Promote to wider type
        let result_ty = self.promote_types(left_ty, right_ty);
        Ok(result_ty)
    }

    fn check_unary_op(&self, op: UnOp, ty: &Type) -> Result<Type, CompileError> {
        match op {
            UnOp::Neg => {
                if !ty.is_numeric() {
                    return Err(CompileError::TypeError(
                        "Negation requires numeric type".to_string()
                    ));
                }
                Ok(ty.clone())
            }
            UnOp::Not => {
                if *ty != Type::Bool {
                    return Err(CompileError::TypeError(
                        "Logical NOT requires bool type".to_string()
                    ));
                }
                Ok(Type::Bool)
            }
        }
    }

    fn types_compatible(&self, from: &Type, to: &Type) -> bool {
        if from == to {
            return true;
        }

        // Check if cast is possible
        self.can_cast(from, to)
    }

    fn can_cast(&self, from: &Type, to: &Type) -> bool {
        // Numeric conversions
        if from.is_numeric() && to.is_numeric() {
            return true;
        }

        // Fast <-> non-fast conversions
        if let Type::Fast(inner, _) = from {
            return self.can_cast(inner, to);
        }
        if let Type::Fast(inner, _) = to {
            return self.can_cast(from, inner);
        }

        false
    }

    fn is_lossy_cast(&self, from: &Type, to: &Type) -> bool {
        // Float to int is lossy
        if from.is_float() && to.is_integer() {
            return true;
        }

        // Larger int to smaller int is potentially lossy
        // (simplified check)
        if from.is_integer() && to.is_integer() {
            return true; // Conservative: assume all int conversions might lose data
        }

        false
    }

    fn promote_types(&self, left: &Type, right: &Type) -> Type {
        // Promote to float if either is float
        if left.is_float() || right.is_float() {
            return Type::F64;
        }

        // Otherwise, promote to larger int type (simplified: always i32)
        Type::I32
    }
}
