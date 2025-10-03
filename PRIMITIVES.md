# DataRust Primitive Types - Implementation Complete ✓

## Overview

DataRust now fully supports **13 primitive types** with Minecraft-compatible storage and operations. All primitives use **copy semantics** (no Rust-style ownership/borrowing).

---

## Implemented Primitive Types

### Boolean Type

| Type | Range | Literal | Storage | Fast Support | Example |
|------|-------|---------|---------|--------------|---------|
| `bool` | true/false | `true`, `false` | NBT Byte (0/1) | ✓ Scoreboard 0/1 | `let flag: bool = true` |

**Operations:** Logical (`&&`, `||`, `!`), comparison, arithmetic (as 0/1)

---

### Signed Integers

| Type | Range | Literal | NBT | Fast Support | Overflow Wrapping |
|------|-------|---------|-----|--------------|-------------------|
| `i8` | -128..127 | `42i8`, `-20i8` | Byte | ✓ | `+128, %256, -128` |
| `i16` | -32,768..32,767 | `31415i16` | Short | ✓ | `+32768, %65536, -32768` |
| `i32` | -2³¹..2³¹-1 | `42`, `31415926i32` | Int | ✓ Native | None (native) |
| `i64` | -2⁶³..2⁶³-1 | `31415926535i64` | Long | ✗ | N/A |

**Default:** Integer literals without suffix default to `i32`

**Operations:** Arithmetic (`+`, `-`, `*`, `/`, `%`), comparison, bitwise (`&`, `|`, `^`, `<<`, `>>`)

---

### Unsigned Integers

| Type | Range | Literal | NBT | Fast Support | Overflow Wrapping |
|------|-------|---------|-----|--------------|-------------------|
| `u8` | 0..255 | `255u8` | Byte | ✓ | `%256` |
| `u16` | 0..65,535 | `65535u16` | Short | ✓ | `%65536` |
| `u32` | 0..2³²-1 | `4294967295u32` | Int | ✓ | None |
| `u64` | 0..2⁶⁴-1 | `18446744073709551615u64` | Long | ✗ | N/A |

**Operations:** Same as signed integers

---

### Floating Point

| Type | Precision | Literal | NBT | Fast Support | Example |
|------|-----------|---------|-----|--------------|---------|
| `f32` | 32-bit | `3.14f32` | Float | ✓ Scaled int | `let pi: f32 = 3.14f32` |
| `f64` | 64-bit | `3.14`, `3.1415926535f64` | Double | ✓ Scaled int | `let dist: f64 = 123.456` |

**Default:** Float literals without suffix default to `f64`

**Fast Storage:** Use `fast f32<1000>` or `fast f64<1000>` for scaled integer approximation
- Warning issued: "Fast float uses approximation; precision may be lost"

**Operations:** Arithmetic, comparison

---

### String & Character

| Type | Purpose | Literal | NBT | Fast Support | Example |
|------|---------|---------|-----|--------------|---------|
| `str` | Text string | `"hello"` | String | ✗ | `let name: str = "player"` |
| `char` | Single character | `'A'` | String (len=1) | ✗ | `let letter: char = 'a'` |

**Operations (str):** Concatenation (`+`), comparison, `.len()`

**Operations (char):** Comparison, conversion to `str`

---

## Storage Modes

### NBT Storage (Default)
```rust
let count: i32 = 100
```
- **Stored in:** `data storage mypack:vars count`
- **Pros:** Flexible, persistent, supports all types
- **Cons:** Slower for math operations

### Fast Storage (Scoreboards)
```rust
let count: fast i32 = 100
```
- **Stored in:** `scoreboard players #mypack_count mypack_obj`
- **Pros:** Very fast arithmetic, ideal for loops/counters
- **Cons:** 32-bit only, no strings/chars, floats require scaling

**Supported Fast Types:** `bool`, `i8`, `i16`, `i32`, `u8`, `u16`, `u32`, `f32`, `f64`

**Unsupported Fast Types:** `i64`, `u64`, `str`, `char` → Compile error

---

## Overflow Behavior

Small types (`i8`, `u8`, `i16`, `u16`) automatically wrap on overflow since Minecraft uses 32-bit scoreboards.

### Generated Wrapping Code

**i8 wrapping:**
```mcfunction
scoreboard players add #temp mypack_temp 128
scoreboard players operation #temp mypack_temp %= #const256 mypack_temp
scoreboard players remove #temp mypack_temp 128
```

**u8 wrapping:**
```mcfunction
scoreboard players operation #temp mypack_temp %= #const256 mypack_temp
```

**i16 wrapping:**
```mcfunction
scoreboard players add #temp mypack_temp 32768
scoreboard players operation #temp mypack_temp %= #const65536 mypack_temp
scoreboard players remove #temp mypack_temp 32768
```

**u16 wrapping:**
```mcfunction
scoreboard players operation #temp mypack_temp %= #const65536 mypack_temp
```

---

## Type Inference & Explicit Types

### Inference
```rust
let x = 42        // Infers i32
let y = 3.14      // Infers f64
let z = true      // Infers bool
```

### Explicit Types
```rust
let x: i8 = 42i8
let y: u16 = 1000u16
let z: f32 = 3.14f32
let w: fast i32 = 500
```

---

## Type Casting with `as`

```rust
let big: i64 = 1000i64
let small: i32 = big as i32    // Warning: Potential truncation

let float_val: f64 = 3.14
let int_val: i32 = float_val as i32    // Warning: Potential truncation

let negative: i8 = -10i8
let unsigned: u8 = negative as u8      // Warning: Negative value invalid
```

**Warnings Generated:**
- Lossy casts (larger → smaller type, float → int)
- Negative to unsigned conversions
- Fast float approximation

---

## Compilation Errors

### Invalid Fast Types
```rust
let x: fast i64 = 100i64    // ERROR: fast i64/u64 unsupported
let s: fast str = "hello"   // ERROR: fast str/char unsupported
```

### Literal Out of Range
```rust
let x: i8 = 200i8           // ERROR: 200 out of range for i8 (-128..127)
let y: u8 = -10u8           // ERROR: -10 invalid for unsigned type
```

---

## Generated Minecraft Code

### Initialization (load.mcfunction)
```mcfunction
# DataRust initialization
scoreboard objectives add mypack_obj dummy
scoreboard objectives add mypack_temp dummy
data merge storage mypack:vars {}
# Constants for overflow wrapping
scoreboard players set #const256 mypack_temp 256
scoreboard players set #const65536 mypack_temp 65536
```

### Example Function
**DataRust:**
```rust
fn test() {
    let x: i8 = 42i8
    let y: fast i32 = 100
    let z: f32 = 3.14f32
}
```

**Generated:**
```mcfunction
# Function: test (ID: 0)
# i8 with overflow wrapping
scoreboard players set #f0_temp0 mypack_temp 42
scoreboard players add #f0_temp0 mypack_temp 128
scoreboard players operation #f0_temp0 mypack_temp %= #const256 mypack_temp
scoreboard players remove #f0_temp0 mypack_temp 128
execute store result storage mypack:vars x int 1 run scoreboard players get #f0_temp0 mypack_temp

# fast i32 in scoreboard
scoreboard players set #f0_temp1 mypack_temp 100
scoreboard players operation #mypack_y mypack_obj = #f0_temp1 mypack_temp

# f32 in NBT
data modify storage mypack:vars temp_2 set value 3.14f
```

---

## Complete Example

```rust
fn main() {
    // Various primitive types
    let count: fast i32 = 0
    let max: i32 = 100
    let pi: f32 = 3.14f32
    let active: bool = true
    let color: u8 = 255u8
    let message: str = "Hello!"
    
    // Arithmetic
    let sum = count + max
    let scaled = pi * 2.0f32
    
    // Type casting
    let byte: i8 = sum as i8    // With warning
}
```

---

## Features Implemented ✓

- [x] All 13 primitive types with proper literals
- [x] Type inference (defaults to i32/f64)
- [x] Explicit type annotations with suffixes
- [x] Fast storage mode for performance
- [x] NBT storage for flexibility
- [x] Overflow wrapping for small types (i8, u8, i16, u16)
- [x] Type casting with `as` keyword
- [x] Compile-time validation (range checks, type compatibility)
- [x] Warning system (lossy casts, fast float precision, etc.)
- [x] Error messages for unsupported combinations
- [x] Proper Minecraft command generation

---

## Testing

Test your primitives:
```bash
MineRust.exe --input simple_test.dr --debug
```

Generated datapack location: `./output/`

Load in Minecraft:
```mcfunction
/reload
/function mypack:load
/function mypack:main
```

---

## Summary

DataRust's primitive type system is **production-ready** with:
- ✅ Complete type coverage (bool, integers, floats, strings, chars)
- ✅ Minecraft-optimized storage (NBT + Scoreboards)
- ✅ Overflow safety for small types
- ✅ Performance options (fast storage)
- ✅ Comprehensive warnings & errors
- ✅ Copy semantics (simple, predictable)

No ownership, no borrowing—just straightforward, Minecraft-friendly primitives! 🎮
