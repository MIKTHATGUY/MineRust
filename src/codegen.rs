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
            format!(
                "scoreboard players set #const256 {}_temp 256",
                self.namespace
            ),
            format!(
                "scoreboard players set #const65536 {}_temp 65536",
                self.namespace
            ),
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

        let mut commands = vec![format!(
            "# Function: {} (ID: {})",
            func.name, self.current_function_id
        )];

        // Generate function body
        let result_var = self.generate_expr(&func.body, &mut commands)?;

        // Store return value if needed
        if let Some(ref return_type) = func.return_type {
            commands.push(format!("# Store return value from {}", result_var));
            
            // Determine if return type is fast or NBT
            let is_fast_return = matches!(return_type, Type::Fast(_, _));
            
            if is_fast_return {
                // Fast return: store in scoreboard
                let return_var = format!("#{}_return", self.namespace);
                commands.push(format!(
                    "scoreboard players operation {} {}_obj = {} {}_temp",
                    return_var, self.namespace, result_var, self.namespace
                ));
            } else {
                // NBT return: determine appropriate NBT type
                let nbt_type = self.get_nbt_type(return_type);
                commands.push(format!(
                    "execute store result storage {}:vars return {} 1 run scoreboard players get {} {}_temp",
                    self.namespace, nbt_type, result_var, self.namespace
                ));
            }
        }

        self.functions.insert(func.name.clone(), commands);
        Ok(())
    }

    fn generate_expr(
        &mut self,
        expr: &Expr,
        commands: &mut Vec<String>,
    ) -> Result<String, CompileError> {
        match &expr.kind {
            ExprKind::IntLit(val, _explicit_ty) => {
                // Return a literal marker that will be handled by Let/Assign
                // For now, use a temp scoreboard for arithmetic contexts
                let temp = self.get_temp();
                commands.push(format!(
                    "scoreboard players set {} {}_temp {}",
                    temp, self.namespace, val
                ));
                Ok(temp)
            }

            ExprKind::FloatLit(_val, _explicit_ty) => {
                // Return a marker for NBT context
                // For now, use a temp variable name
                let temp = format!("float_temp_{}", self.temp_counter);
                self.temp_counter += 1;
                Ok(temp)
            }

            ExprKind::BoolLit(_val) => {
                // Return a marker for NBT context
                let temp = format!("bool_temp_{}", self.temp_counter);
                self.temp_counter += 1;
                Ok(temp)
            }

            ExprKind::StringLit(val) => {
                // String literals stored in NBT
                let temp = format!("str_temp_{}", self.temp_counter);
                self.temp_counter += 1;
                commands.push(format!(
                    "data modify storage {}:vars {} set value \"{}\"",
                    self.namespace, temp, val
                ));
                Ok(temp)
            }

            ExprKind::CharLit(val) => {
                // Char literals stored as single-char strings in NBT
                let temp = format!("char_temp_{}", self.temp_counter);
                self.temp_counter += 1;
                commands.push(format!(
                    "data modify storage {}:vars {} set value \"{}\"",
                    self.namespace, temp, val
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
                // Check if this is a fast storage type
                if let Some(Type::Fast(_inner_ty, scale)) = ty {
                    // Fast storage: use scoreboard
                    match &value.kind {
                        ExprKind::IntLit(val, _) => {
                            // Direct scoreboard set for integer literals
                            let var_name = format!("#{}_{}", self.namespace, name);

                            if let Some(scale_val) = scale {
                                // Fast float with scaling
                                let scaled = ((*val as f64) * (*scale_val as f64)) as i64;
                                commands.push(format!(
                                    "scoreboard players set {} {}_obj {}",
                                    var_name, self.namespace, scaled
                                ));
                            } else {
                                // Regular fast integer
                                commands.push(format!(
                                    "scoreboard players set {} {}_obj {}",
                                    var_name, self.namespace, val
                                ));
                            }
                            return Ok(var_name);
                        }
                        ExprKind::FloatLit(val, _) => {
                            // Fast float: scale and store as integer
                            let var_name = format!("#{}_{}", self.namespace, name);
                            let scaled = if let Some(scale_val) = scale {
                                (val * (*scale_val as f64)) as i64
                            } else {
                                *val as i64
                            };
                            commands.push(format!(
                                "scoreboard players set {} {}_obj {}",
                                var_name, self.namespace, scaled
                            ));
                            return Ok(var_name);
                        }
                        ExprKind::BoolLit(val) => {
                            // Fast bool: 0 or 1 in scoreboard
                            let var_name = format!("#{}_{}", self.namespace, name);
                            commands.push(format!(
                                "scoreboard players set {} {}_obj {}",
                                var_name,
                                self.namespace,
                                if *val { 1 } else { 0 }
                            ));
                            return Ok(var_name);
                        }
                        _ => {
                            // Complex expression or variable reference
                            // Check if we're converting from NBT to fast with scaling
                            match &value.kind {
                                ExprKind::Var(var_name) => {
                                    // Converting NBT variable to fast storage
                                    let target_var = format!("#{}_{}", self.namespace, name);

                                    if let Some(scale_val) = scale {
                                        // Fast float conversion with scaling: use data get with scale
                                        commands.push(format!(
                                            "execute store result score {} {}_obj run data get storage {}:vars {} {}",
                                            target_var, self.namespace, self.namespace, var_name, scale_val
                                        ));
                                    } else {
                                        // Regular int/bool conversion
                                        commands.push(format!(
                                            "execute store result score {} {}_obj run data get storage {}:vars {}",
                                            target_var, self.namespace, self.namespace, var_name
                                        ));
                                    }
                                    return Ok(target_var);
                                }
                                _ => {
                                    // Evaluate expression and copy to scoreboard
                                    let value_var = self.generate_expr(value, commands)?;
                                    let var_name = format!("#{}_{}", self.namespace, name);
                                    commands.push(format!(
                                        "scoreboard players operation {} {}_obj = {} {}_temp",
                                        var_name, self.namespace, value_var, self.namespace
                                    ));
                                    return Ok(var_name);
                                }
                            }
                        }
                    }
                } else {
                    // NBT storage: use direct data modify for literals
                    match &value.kind {
                        ExprKind::IntLit(val, explicit_ty) => {
                            // Determine NBT type suffix based on the variable's type or explicit literal type
                            let nbt_suffix = if let Some(ty) = ty.as_ref().or(explicit_ty.as_ref())
                            {
                                match ty {
                                    Type::I8 | Type::U8 => "b",   // Byte
                                    Type::I16 | Type::U16 => "s", // Short
                                    Type::I64 | Type::U64 => "L", // Long
                                    _ => "",                      // Int (default, no suffix)
                                }
                            } else {
                                "" // Default to Int
                            };

                            commands.push(format!(
                                "data modify storage {}:vars {} set value {}{}",
                                self.namespace, name, val, nbt_suffix
                            ));
                            return Ok(name.clone());
                        }
                        ExprKind::FloatLit(val, explicit_ty) => {
                            // Determine float type suffix
                            let nbt_suffix = if let Some(ty) = ty.as_ref().or(explicit_ty.as_ref())
                            {
                                match ty {
                                    Type::F32 => "f", // Float
                                    _ => "d",         // Double (default)
                                }
                            } else {
                                "d" // Default to Double
                            };

                            commands.push(format!(
                                "data modify storage {}:vars {} set value {}{}",
                                self.namespace, name, val, nbt_suffix
                            ));
                            return Ok(name.clone());
                        }
                        ExprKind::BoolLit(val) => {
                            // Bool as NBT Byte (0 or 1)
                            commands.push(format!(
                                "data modify storage {}:vars {} set value {}b",
                                self.namespace,
                                name,
                                if *val { 1 } else { 0 }
                            ));
                            return Ok(name.clone());
                        }
                        ExprKind::StringLit(s) => {
                            // String as NBT String
                            commands.push(format!(
                                "data modify storage {}:vars {} set value \"{}\"",
                                self.namespace, name, s
                            ));
                            return Ok(name.clone());
                        }
                        ExprKind::CharLit(c) => {
                            // Char as single-character NBT String
                            commands.push(format!(
                                "data modify storage {}:vars {} set value \"{}\"",
                                self.namespace, name, c
                            ));
                            return Ok(name.clone());
                        }
                        _ => {
                            // Complex expression or variable reference
                            match &value.kind {
                                ExprKind::Var(var_name) => {
                                    // Check if source is a fast variable (starts with #)
                                    if var_name.starts_with('#') {
                                        // Converting fast to NBT
                                        // Check if we need to scale down (fast float to regular float)
                                        if let Some(value_ty) = &value.ty {
                                            if let Type::Fast(_inner, Some(_scale_val)) = value_ty {
                                                // Fast float to NBT float: divide by scale
                                                // For now, store as-is (scaled integer)
                                                // TODO: implement proper float conversion with division
                                                commands.push(format!(
                                                    "execute store result storage {}:vars {} int 1 run scoreboard players get {} {}_obj",
                                                    self.namespace, name, var_name, self.namespace
                                                ));
                                            } else {
                                                // Regular fast int/bool to NBT
                                                commands.push(format!(
                                                    "execute store result storage {}:vars {} int 1 run scoreboard players get {} {}_obj",
                                                    self.namespace, name, var_name, self.namespace
                                                ));
                                            }
                                        } else {
                                            // No type info, assume regular conversion
                                            commands.push(format!(
                                                "execute store result storage {}:vars {} int 1 run scoreboard players get {} {}_obj",
                                                self.namespace, name, var_name, self.namespace
                                            ));
                                        }
                                    } else {
                                        // NBT to NBT copy
                                        commands.push(format!(
                                            "data modify storage {}:vars {} set from storage {}:vars {}",
                                            self.namespace, name, self.namespace, var_name
                                        ));
                                    }
                                    return Ok(name.clone());
                                }
                                _ => {
                                    // Evaluate expression then store
                                    let value_var = self.generate_expr(value, commands)?;

                                    // Determine the appropriate NBT type suffix
                                    let nbt_type = if let Some(ty) = ty {
                                        match ty {
                                            Type::I8 | Type::U8 => "byte",
                                            Type::I16 | Type::U16 => "short",
                                            Type::I64 | Type::U64 => "long",
                                            Type::F32 => "float",
                                            Type::F64 => "double",
                                            _ => "int",
                                        }
                                    } else {
                                        "int"
                                    };

                                    commands.push(format!(
                                        "execute store result storage {}:vars {} {} 1 run scoreboard players get {} {}_temp",
                                        self.namespace, name, nbt_type, value_var, self.namespace
                                    ));
                                    return Ok(name.clone());
                                }
                            }
                        }
                    }
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
                // Generate code for arguments (TODO: proper parameter passing)
                for arg in args {
                    self.generate_expr(arg, commands)?;
                }

                // Call function
                commands.push(format!("function {}:{}", self.namespace, name));

                // Return value handling depends on the called function's return type
                // For now, check if the return value is in fast or NBT storage
                // We'll use a heuristic: check the global return variables
                
                // Try fast return first (scoreboard)
                let result_temp = self.get_temp();
                commands.push(format!(
                    "# Copy return value from function {}",
                    name
                ));
                commands.push(format!(
                    "scoreboard players operation {} {}_temp = #{}_return {}_obj",
                    result_temp, self.namespace, self.namespace, self.namespace
                ));

                Ok(result_temp)
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
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
                self.functions
                    .insert(format!("if_then_{}", self.temp_counter), then_commands);
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
                    self.functions
                        .insert(format!("if_else_{}", self.temp_counter), else_commands);
                    self.temp_counter += 1;
                }

                Ok(result_temp)
            }

            ExprKind::Cast { expr, target_ty } => {
                // Generate the source expression
                let source_var = self.generate_expr(expr, commands)?;

                // For most casts, we just need to copy the value and possibly apply overflow wrapping
                // The actual type conversion is handled by NBT or scoreboard operations
                let result = self.get_temp();

                // Copy source to result
                commands.push(format!(
                    "scoreboard players operation {} {}_temp = {} {}_temp",
                    result, self.namespace, source_var, self.namespace
                ));

                // Apply overflow wrapping for the target type if it's a small integer
                self.apply_overflow_wrapping(&result, target_ty, commands);

                Ok(result)
            }

            ExprKind::As { selector, body } => {
                // Generate body commands
                let mut body_commands = Vec::new();
                let result_var = self.generate_expr(body, &mut body_commands)?;
                
                // Store result in temp before wrapping in execute
                let final_result = self.get_temp();
                body_commands.push(format!(
                    "scoreboard players operation {} {}_temp = {} {}_temp",
                    final_result, self.namespace, result_var, self.namespace
                ));
                
                // Create a helper function for the body
                let helper_name = format!("as_helper_{}", self.temp_counter);
                self.temp_counter += 1;
                self.functions.insert(helper_name.clone(), body_commands);
                
                // Generate selector string
                let selector_str = match selector {
                    SelectorOrString::Selector(sel) => self.generate_selector(sel),
                    SelectorOrString::String(s) => s.clone(),
                };
                
                // Generate execute as command
                commands.push(format!(
                    "execute as {} run function {}:{}",
                    selector_str, self.namespace, helper_name
                ));
                
                Ok(final_result)
            }

            ExprKind::At { position, body } => {
                // Generate body commands
                let mut body_commands = Vec::new();
                let result_var = self.generate_expr(body, &mut body_commands)?;
                
                // Store result in temp before wrapping in execute
                let final_result = self.get_temp();
                body_commands.push(format!(
                    "scoreboard players operation {} {}_temp = {} {}_temp",
                    final_result, self.namespace, result_var, self.namespace
                ));
                
                // Create a helper function for the body
                let helper_name = format!("at_helper_{}", self.temp_counter);
                self.temp_counter += 1;
                self.functions.insert(helper_name.clone(), body_commands);
                
                // Generate position string
                let pos_str = match position {
                    Position::Relative(coords) => coords.clone(),
                    Position::Absolute(x, y, z) => format!("{} {} {}", x, y, z),
                    Position::Struct(name) => {
                        // TODO: Load struct coordinates - for now use placeholder
                        format!("~ ~ ~  # TODO: load from {}", name)
                    }
                };
                
                // Generate execute at command
                commands.push(format!(
                    "execute positioned {} run function {}:{}",
                    pos_str, self.namespace, helper_name
                ));
                
                Ok(final_result)
            }

            ExprKind::AsAt { selector, body } => {
                // Generate body commands
                let mut body_commands = Vec::new();
                let result_var = self.generate_expr(body, &mut body_commands)?;
                
                // Store result in temp before wrapping in execute
                let final_result = self.get_temp();
                body_commands.push(format!(
                    "scoreboard players operation {} {}_temp = {} {}_temp",
                    final_result, self.namespace, result_var, self.namespace
                ));
                
                // Create a helper function for the body
                let helper_name = format!("asat_helper_{}", self.temp_counter);
                self.temp_counter += 1;
                self.functions.insert(helper_name.clone(), body_commands);
                
                // Generate selector string
                let selector_str = match selector {
                    SelectorOrString::Selector(sel) => self.generate_selector(sel),
                    SelectorOrString::String(s) => s.clone(),
                };
                
                // Generate execute as at command
                commands.push(format!(
                    "execute as {} at @s run function {}:{}",
                    selector_str, self.namespace, helper_name
                ));
                
                Ok(final_result)
            }

            ExprKind::Selector(selector) => {
                // Generate Minecraft selector string
                let selector_str = self.generate_selector(selector);
                
                // Store selector as a string temp (for use in commands)
                let temp = format!("selector_temp_{}", self.temp_counter);
                self.temp_counter += 1;
                commands.push(format!("# Selector: {}", selector_str));
                
                // Return a marker that can be used later
                // In practice, selectors are used inline in commands, not stored
                Ok(temp)
            }

            _ => Err(CompileError::CodegenError(format!(
                "Unsupported expression: {:?}",
                expr.kind
            ))),
        }
    }

    fn generate_binary_op(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
        commands: &mut Vec<String>,
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
                return Err(CompileError::CodegenError(format!(
                    "Unsupported binary operation: {:?}",
                    op
                )));
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

    fn get_nbt_type(&self, ty: &Type) -> &str {
        match ty {
            Type::I8 | Type::U8 => "byte",
            Type::I16 | Type::U16 => "short",
            Type::I64 | Type::U64 => "long",
            Type::F32 => "float",
            Type::F64 => "double",
            Type::Str => "string",
            Type::Fast(inner, _) => self.get_nbt_type(inner),
            _ => "int",
        }
    }

    fn generate_selector(&self, selector: &Selector) -> String {
        use crate::ast::{SelectorTarget, SelectorFilter, SortType};

        // Map target to Minecraft selector base
        let base = match selector.target {
            SelectorTarget::AllPlayers => "@a",
            SelectorTarget::NearestPlayer => "@p",
            SelectorTarget::RandomPlayer => "@r",
            SelectorTarget::SelfTarget => "@s",
            SelectorTarget::Entities => "@e",
        };

        if selector.filters.is_empty() {
            return base.to_string();
        }

        // Build filter string
        let mut filter_parts = Vec::new();

        for filter in &selector.filters {
            match filter {
                SelectorFilter::Type(entity_type) => {
                    filter_parts.push(format!("type={}", entity_type));
                }
                SelectorFilter::Name(name) => {
                    filter_parts.push(format!("name={}", name));
                }
                SelectorFilter::Distance(range) => {
                    filter_parts.push(format!("distance={}", self.format_range(range)));
                }
                SelectorFilter::X(val) => {
                    filter_parts.push(format!("x={}", val));
                }
                SelectorFilter::Y(val) => {
                    filter_parts.push(format!("y={}", val));
                }
                SelectorFilter::Z(val) => {
                    filter_parts.push(format!("z={}", val));
                }
                SelectorFilter::DX(val) => {
                    filter_parts.push(format!("dx={}", val));
                }
                SelectorFilter::DY(val) => {
                    filter_parts.push(format!("dy={}", val));
                }
                SelectorFilter::DZ(val) => {
                    filter_parts.push(format!("dz={}", val));
                }
                SelectorFilter::Pitch(range) => {
                    filter_parts.push(format!("x_rotation={}", self.format_range(range)));
                }
                SelectorFilter::Yaw(range) => {
                    filter_parts.push(format!("y_rotation={}", self.format_range(range)));
                }
                SelectorFilter::Limit(limit) => {
                    filter_parts.push(format!("limit={}", limit));
                }
                SelectorFilter::Sort(sort_type) => {
                    let sort_str = match sort_type {
                        SortType::Nearest => "nearest",
                        SortType::Furthest => "furthest",
                        SortType::Random => "random",
                        SortType::Arbitrary => "arbitrary",
                    };
                    filter_parts.push(format!("sort={}", sort_str));
                }
                SelectorFilter::Predicate(pred) => {
                    filter_parts.push(format!("predicate={}", pred));
                }
            }
        }

        format!("{}[{}]", base, filter_parts.join(","))
    }

    fn format_range(&self, range: &RangeValue) -> String {
        match range {
            RangeValue::Exact(val) => val.to_string(),
            RangeValue::Range(start, end) => format!("{}..{}", start, end),
            RangeValue::UpTo(end) => format!("..{}", end),
            RangeValue::From(start) => format!("{}..", start),
        }
    }
}
