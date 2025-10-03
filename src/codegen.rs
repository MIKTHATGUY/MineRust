/// Code generation module - converts AST to Minecraft commands
use crate::ast::*;
use crate::error::CompileError;
use std::collections::HashMap;

pub struct CodeGen {
    /// Generated functions (name -> commands)
    functions: HashMap<String, Vec<String>>,
    /// Namespace for the datapack
    namespace: String,
    /// Temporary scoreboard counter
    temp_counter: usize,
    /// Current function's unique ID (for return addresses)
    current_function_id: u32,
    /// Global counter for unique function IDs
    function_id_counter: u32,
}

pub fn generate(program: &Program) -> Result<HashMap<String, Vec<String>>, CompileError> {
    let mut codegen = CodeGen {
        functions: HashMap::new(),
        namespace: "mypack".to_string(),
        temp_counter: 0,
        current_function_id: 0,
        function_id_counter: 0,
    };

    codegen.generate_program(program)?;
    Ok(codegen.functions)
}

impl CodeGen {
    fn generate_program(&mut self, program: &Program) -> Result<(), CompileError> {
        // Generate load function for initialization
        let load_commands = vec![
            "# DataRust initialization".to_string(),
            format!("scoreboard objectives add {}_obj dummy", self.namespace),
            format!("scoreboard objectives add {}_temp dummy", self.namespace),
            format!("data merge storage {}:vars {{}}", self.namespace),
            "# Constants for overflow wrapping".to_string(),
            format!("scoreboard players set #const256 {}_temp 256", self.namespace),
            format!("scoreboard players set #const65536 {}_temp 65536", self.namespace),
        ];
        self.functions.insert("load".to_string(), load_commands);

        // Generate each function
        for item in &program.items {
            if let Item::Function(func) = item {
                self.generate_function(func)?;
            }
        }

        Ok(())
    }

    fn generate_function(&mut self, func: &Function) -> Result<(), CompileError> {
        // Assign unique ID to this function for isolated return address
        self.current_function_id = self.function_id_counter;
        self.function_id_counter += 1;
        
        // Reset temp counter for each function to ensure isolation
        self.temp_counter = 0;
        
        let mut commands = vec![
            format!("# Function: {} (ID: {})", func.name, self.current_function_id),
        ];

        // Generate function body
        let result_var = self.generate_expr(&func.body, &mut commands)?;

        // Store return value if needed
        if func.return_type.is_some() {
            commands.push(format!(
                "# Store return value from {}",
                result_var
            ));
            commands.push(self.store_return_value(&result_var));
        }

        self.functions.insert(func.name.clone(), commands);
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr, commands: &mut Vec<String>) -> Result<String, CompileError> {
        match &expr.kind {
            ExprKind::IntLit(val, explicit_ty) => {
                // Store literal in a temporary scoreboard
                let temp = self.get_temp();
                commands.push(format!(
                    "scoreboard players set {} {}_temp {}",
                    temp, self.namespace, val
                ));
                
                // Apply overflow wrapping for small types if needed
                if let Some(ty) = explicit_ty {
                    self.apply_overflow_wrapping(&temp, ty, commands);
                }
                
                Ok(temp)
            }

            ExprKind::FloatLit(val, explicit_ty) => {
                // Check if this should be stored in fast storage (scaled int)
                if let Some(ty_info) = &expr.ty {
                    if let Type::Fast(_inner, Some(scale)) = ty_info {
                        // Fast float: store as scaled integer in scoreboard
                        let scaled_val = (val * (*scale as f64)) as i64;
                        let temp = self.get_temp();
                        commands.push(format!(
                            "scoreboard players set {} {}_temp {}",
                            temp, self.namespace, scaled_val
                        ));
                        return Ok(temp);
                    }
                }
                
                // Regular NBT storage for floats
                let temp = format!("temp_{}", self.temp_counter);
                self.temp_counter += 1;
                
                // Determine NBT type suffix
                let suffix = match explicit_ty {
                    Some(Type::F32) => "f",
                    _ => "d", // f64 or default
                };
                
                commands.push(format!(
                    "data modify storage {}:vars {} set value {}{}",
                    self.namespace, temp, val, suffix
                ));
                Ok(temp)
            }

            ExprKind::BoolLit(val) => {
                let temp = self.get_temp();
                commands.push(format!(
                    "scoreboard players set {} {}_temp {}",
                    temp, self.namespace, if *val { 1 } else { 0 }
                ));
                Ok(temp)
            }

            ExprKind::Var(name) => {
                // Variable reference - need to load it into a temp if it's in NBT
                // For now, we'll check if it looks like a fast variable (starts with #)
                // Otherwise, load from NBT storage into a temp scoreboard
                if name.starts_with('#') {
                    // Already a scoreboard variable
                    Ok(name.clone())
                } else {
                    // Load from NBT into a temporary scoreboard
                    let temp = self.get_temp();
                    commands.push(format!(
                        "execute store result score {} {}_temp run data get storage {}:vars {}",
                        temp, self.namespace, self.namespace, name
                    ));
                    Ok(temp)
                }
            }

            ExprKind::BinaryOp { op, left, right } => {
                self.generate_binary_op(*op, left, right, commands)
            }

            ExprKind::Let { name, ty, value } => {
                let value_var = self.generate_expr(value, commands)?;
                
                // Determine storage type
                if let Some(Type::Fast(_, _)) = ty {
                    // Fast storage: use scoreboard
                    let var_name = format!("#{}_{}",  self.namespace, name);
                    commands.push(format!(
                        "scoreboard players operation {} {}_obj = {} {}_temp",
                        var_name, self.namespace, value_var, self.namespace
                    ));
                    Ok(var_name)
                } else {
                    // NBT storage - need to copy from scoreboard to NBT using execute store
                    commands.push(format!(
                        "execute store result storage {}:vars {} int 1 run scoreboard players get {} {}_temp",
                        self.namespace, name, value_var, self.namespace
                    ));
                    Ok(name.clone())
                }
            }

            ExprKind::Block(exprs) => {
                let mut last_var = String::new();
                for expr in exprs {
                    last_var = self.generate_expr(expr, commands)?;
                }
                Ok(last_var)
            }

            ExprKind::Call { name, args } => {
                // Generate code for arguments
                for arg in args {
                    self.generate_expr(arg, commands)?;
                }
                
                // Call function
                commands.push(format!(
                    "function {}:{}",
                    self.namespace, name
                ));
                
                // Return value is in function-specific return variable
                let return_var = self.get_return_var(name);
                
                // Copy to a local temp to avoid conflicts if this result is used
                let result_temp = self.get_temp();
                commands.push(format!(
                    "scoreboard players operation {} {}_temp = {} {}_obj",
                    result_temp, self.namespace, return_var, self.namespace
                ));
                
                Ok(result_temp)
            }

            ExprKind::If { condition, then_branch, else_branch } => {
                let cond_var = self.generate_expr(condition, commands)?;
                let result_temp = self.get_temp();
                
                // Generate then branch
                let mut then_commands = Vec::new();
                let then_var = self.generate_expr(then_branch, &mut then_commands)?;
                then_commands.push(format!(
                    "scoreboard players operation {} {}_temp = {} {}_temp",
                    result_temp, self.namespace, then_var, self.namespace
                ));
                
                // Generate execute if
                commands.push(format!(
                    "execute if score {} {}_temp matches 1 run function {}:if_then_{}",
                    cond_var, self.namespace, self.namespace, self.temp_counter
                ));
                self.functions.insert(
                    format!("if_then_{}", self.temp_counter),
                    then_commands
                );
                self.temp_counter += 1;
                
                // Generate else branch if present
                if let Some(else_expr) = else_branch {
                    let mut else_commands = Vec::new();
                    let else_var = self.generate_expr(else_expr, &mut else_commands)?;
                    else_commands.push(format!(
                        "scoreboard players operation {} {}_temp = {} {}_temp",
                        result_temp, self.namespace, else_var, self.namespace
                    ));
                    
                    commands.push(format!(
                        "execute unless score {} {}_temp matches 1 run function {}:if_else_{}",
                        cond_var, self.namespace, self.namespace, self.temp_counter
                    ));
                    self.functions.insert(
                        format!("if_else_{}", self.temp_counter),
                        else_commands
                    );
                    self.temp_counter += 1;
                }
                
                Ok(result_temp)
            }

            _ => {
                Err(CompileError::CodegenError(
                    format!("Unsupported expression: {:?}", expr.kind)
                ))
            }
        }
    }

    fn generate_binary_op(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
        commands: &mut Vec<String>
    ) -> Result<String, CompileError> {
        let left_var = self.generate_expr(left, commands)?;
        let right_var = self.generate_expr(right, commands)?;
        
        let result = self.get_temp();
        
        // Copy left to result
        commands.push(format!(
            "scoreboard players operation {} {}_temp = {} {}_temp",
            result, self.namespace, left_var, self.namespace
        ));
        
        // Perform operation
        let op_symbol = match op {
            BinOp::Add => "+=",
            BinOp::Sub => "-=",
            BinOp::Mul => "*=",
            BinOp::Div => "/=",
            BinOp::Mod => "%=",
            _ => {
                return Err(CompileError::CodegenError(
                    format!("Unsupported binary operation: {:?}", op)
                ));
            }
        };
        
        commands.push(format!(
            "scoreboard players operation {} {}_temp {} {} {}_temp",
            result, self.namespace, op_symbol, right_var, self.namespace
        ));
        
        // Apply overflow wrapping if the result type is a small integer
        if let Some(ty) = &left.ty {
            self.apply_overflow_wrapping(&result, ty, commands);
        }
        
        Ok(result)
    }

    fn apply_overflow_wrapping(&self, var: &str, ty: &Type, commands: &mut Vec<String>) {
        // Unwrap Fast types to get the inner type
        let base_ty = match ty {
            Type::Fast(inner, _) => inner.as_ref(),
            _ => ty,
        };
        
        match base_ty {
            Type::I8 => {
                // i8: (-128..127) wrap with: +128, %256, -128
                commands.push(format!(
                    "scoreboard players add {} {}_temp 128",
                    var, self.namespace
                ));
                commands.push(format!(
                    "scoreboard players operation {} {}_temp %= #const256 {}_temp",
                    var, self.namespace, self.namespace
                ));
                commands.push(format!(
                    "scoreboard players remove {} {}_temp 128",
                    var, self.namespace
                ));
            }
            Type::U8 => {
                // u8: (0..255) wrap with: %256
                commands.push(format!(
                    "scoreboard players operation {} {}_temp %= #const256 {}_temp",
                    var, self.namespace, self.namespace
                ));
            }
            Type::I16 => {
                // i16: (-32768..32767) wrap with: +32768, %65536, -32768
                commands.push(format!(
                    "scoreboard players add {} {}_temp 32768",
                    var, self.namespace
                ));
                commands.push(format!(
                    "scoreboard players operation {} {}_temp %= #const65536 {}_temp",
                    var, self.namespace, self.namespace
                ));
                commands.push(format!(
                    "scoreboard players remove {} {}_temp 32768",
                    var, self.namespace
                ));
            }
            Type::U16 => {
                // u16: (0..65535) wrap with: %65536
                commands.push(format!(
                    "scoreboard players operation {} {}_temp %= #const65536 {}_temp",
                    var, self.namespace, self.namespace
                ));
            }
            // i32, u32, i64, u64 don't need wrapping in scoreboards (i32 is native, others use NBT)
            _ => {}
        }
    }

    fn get_temp(&mut self) -> String {
        // Create unique temp variable with function ID prefix to avoid conflicts
        let temp = format!("#f{}_temp{}", self.current_function_id, self.temp_counter);
        self.temp_counter += 1;
        temp
    }

    fn store_return_value(&self, var: &str) -> String {
        // Use function-specific return address to avoid conflicts
        format!(
            "scoreboard players operation #{}_return_{} {}_obj = {} {}_temp",
            self.namespace, self.current_function_id, self.namespace, var, self.namespace
        )
    }
    
    fn get_return_var(&self, function_name: &str) -> String {
        // When calling a function, reference its unique return address
        // For now, we'll use a hash of the function name for consistency
        let function_id = self.hash_function_name(function_name);
        format!("#{}_return_{}", self.namespace, function_id)
    }
    
    fn hash_function_name(&self, name: &str) -> u32 {
        // Simple hash function to generate pseudo-random IDs from function names
        let mut hash: u32 = 5381;
        for c in name.bytes() {
            hash = ((hash << 5).wrapping_add(hash)).wrapping_add(c as u32);
        }
        hash % 100000 // Keep it reasonably sized
    }
}
