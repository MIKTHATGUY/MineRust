MineRust — project skeleton

This repository contains a TypeScript-based compiler toolchain skeleton for a small language (MineRust).

Structure created:
- src/lexer — tokenizer
- src/parser — parser grammar and wrapper
- src/ast — AST nodes and visitors
- src/typechecker — type checking
- src/ir — intermediate representation
- src/emitter — emitters and pack writer
- src/linker — module linking
- src/runtime — runtime builtins
- src/utils — helpers
- src/cli — CLI entrypoint
- tests/sample.ms — a tiny sample source file

Next steps:
- Implement the lexer and grammar (nearley) and generate the parser.
- Implement AST lowering to IR and the emitter to create a datapack.

To run (basic):
1. Install dependencies and dev tools (TypeScript, nearley) if needed.
2. Build with tsc (project already contains a tsconfig.json).

This README was generated automatically by the project scaffold script.
