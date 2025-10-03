# Type Conversion Test File - README

## File: test_type_conversions.dr

This comprehensive test file covers **all possible type conversions** and assignments between DataRust primitive types.

## What's Included

### ✅ Test Categories (12 sections)

1. **Same Type Assignments** - Direct variable assignments of identical types
2. **Signed Integer Conversions** - All i8/i16/i32/i64 cast combinations
3. **Unsigned Integer Conversions** - All u8/u16/u32/u64 cast combinations  
4. **Float Conversions** - f32/f64 to/from integers
5. **Boolean Conversions** - bool to integers and vice versa
6. **Fast Storage Conversions** - fast types and NBT/scoreboard interactions
7. **Mixed Arithmetic** - Operations between different types
8. **Boundary Values** - Max/min values and overflow wrapping tests
9. **Negative Number Conversions** - Negative to unsigned casts (warnings expected)
10. **Chained Conversions** - Multiple sequential casts
11. **Type Inference** - Testing implicit type deduction
12. **Function Parameters** - Type conversions through function calls

### Coverage Matrix

| From/To | i8 | i16 | i32 | i64 | u8 | u16 | u32 | u64 | f32 | f64 | bool |
|---------|----|----|-----|-----|----|-----|-----|-----|-----|-----|------|
| **i8**  | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **i16** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **i32** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **i64** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **u8**  | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **u16** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **u32** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **u64** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **f32** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **f64** | ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | ✓   | ✓   | ✓    |
| **bool**| ✓  | ✓  | ✓   | ✓   | ✓  | ✓   | ✓   | ✓   | -   | -   | ✓    |

**Total conversion tests: 100+ unique cast operations**

## Current Status

### ✅ Implemented
- [x] Parser support for `as` keyword
- [x] Type checking with cast validation
- [x] Warning generation for lossy casts
- [x] Comment parsing (line and block comments)

### ⏳ In Progress
- [ ] Code generation for cast operations in codegen.rs

### Expected Warnings

The test file will generate warnings for:
- **Truncation**: Larger → smaller type (e.g., `i32 as i8`)
- **Float → Int**: Loss of decimal precision
- **Negative → Unsigned**: Invalid range conversions
- **Fast float**: Approximation warnings for scaled storage

## How to Use

### Test the entire file (once codegen is complete):
```bash
cargo run -- --input test_type_conversions.dr --output test_conv_output --debug
```

### Test simplified version:
```bash
cargo run -- --input simple_cast_test.dr --output test_output --debug
```

### Expected Output
- Successful parsing of all 12 test functions
- ~30-50 warnings for lossy/potentially unsafe casts
- Generated Minecraft datapack in `test_conv_output/`

## What Each Test Proves

1. **test_same_type_assignments**: Copy semantics work
2. **test_signed_integer_conversions**: Sign preservation in widening/narrowing
3. **test_unsigned_integer_conversions**: Unsigned range handling
4. **test_float_conversions**: Precision loss detection
5. **test_bool_conversions**: 0/1 integer representation
6. **test_fast_conversions**: Scoreboard ↔ NBT transfers
7. **test_mixed_arithmetic**: Type coercion in expressions
8. **test_boundary_conversions**: Overflow wrapping behavior
9. **test_negative_conversions**: Warning system for invalid conversions
10. **test_chained_conversions**: Multiple cast sequence correctness
11. **test_type_inference**: Default type rules (i32, f64)
12. **test_function_conversions**: Parameter/return type casting

## Example Snippets

### Basic Cast
```rust
let a: i32 = 100
let b: i8 = a as i8  // Warning: Potential truncation
```

### Chained Cast
```rust
let start: i64 = 1000i64
let to_i32: i32 = start as i32
let to_i16: i16 = to_i32 as i16
let to_i8: i8 = to_i16 as i8
```

### Fast Storage
```rust
let fast_val: fast i32 = 50
let norm_val: i32 = 50
let combined: i32 = fast_val as i32 + norm_val
```

## Next Steps

To complete type conversion support:
1. Implement `ExprKind::Cast` in `codegen.rs`
2. Handle NBT type conversions (e.g., byte → int)
3. Add scoreboard transfer for fast storage casts
4. Test overflow wrapping for boundary cases

## File Stats

- **Lines**: ~450
- **Functions**: 12 test functions + main
- **Variables**: 200+ declarations
- **Casts**: 100+ type conversions
- **Comments**: Detailed section headers

---

**Status**: Parser ✅ | Type Checker ✅ | Codegen ⏳
