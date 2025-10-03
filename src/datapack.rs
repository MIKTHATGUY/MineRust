/// Datapack output module - writes files using std::fs and serde_json
use crate::error::CompileError;
use serde_json::json;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub fn write_datapack(
    output_path: &Path,
    functions: HashMap<String, Vec<String>>
) -> Result<(), CompileError> {
    let namespace = "mypack";

    // Create directory structure
    let data_path = output_path.join("data").join(namespace);
    let functions_path = data_path.join("functions");
    
    fs::create_dir_all(&functions_path)
        .map_err(|e| CompileError::IoError(format!("Failed to create functions directory: {}", e)))?;

    // Write pack.mcmeta
    let pack_meta = json!({
        "pack": {
            "pack_format": 48,
            "description": "DataRust compiled datapack"
        }
    });
    
    let meta_path = output_path.join("pack.mcmeta");
    fs::write(&meta_path, serde_json::to_string_pretty(&pack_meta).unwrap())
        .map_err(|e| CompileError::IoError(format!("Failed to write pack.mcmeta: {}", e)))?;

    // Write each function
    for (name, commands) in functions {
        let function_path = functions_path.join(format!("{}.mcfunction", name));
        let content = commands.join("\n") + "\n";
        
        fs::write(&function_path, content)
            .map_err(|e| CompileError::IoError(format!("Failed to write function {}: {}", name, e)))?;
    }

    // Create load.json to auto-run load function
    let tags_path = data_path.join("tags").join("functions");
    fs::create_dir_all(&tags_path)
        .map_err(|e| CompileError::IoError(format!("Failed to create tags directory: {}", e)))?;
    
    let load_tag = json!({
        "values": [
            format!("{}:load", namespace)
        ]
    });
    
    let load_tag_path = tags_path.join("load.json");
    fs::write(&load_tag_path, serde_json::to_string_pretty(&load_tag).unwrap())
        .map_err(|e| CompileError::IoError(format!("Failed to write load tag: {}", e)))?;

    Ok(())
}
