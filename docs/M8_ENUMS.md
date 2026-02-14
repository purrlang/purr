# M8: Enums & Switch - Implementation Report

## Overview
M8 introduces enum types and basic enum variant support to the Purr compiler. This milestone adds:
- Enum definitions with named variants  
- Enum type system integration
- Enum variant references and creation
- C codegen for enum types (mapped to `int` with variant constants)

## Implementation Summary

### 1. Lexer (lexer.ml)
- Updated pipe `|` operator to allow single pipe for enum variants (not just `||`)
- Added `Enum` and `Switch` keywords
- Pipe now parses in two contexts: `||` for OR, single `|` for enum variant separation

**Key Changes:**
```ocaml
| Some '|' ->
    (* Now allows single pipe for enums *)
    if peek next is Some '|' then Or
    else Pipe
```

### 2. Parser (parser.ml)
- Added `parse_enum` function for parsing enum definitions
- Enum syntax: `enum EnumName { Variant1 | Variant2 | Variant3 }`
- Updated `parseStructEnumAndActors` to handle enum definitions
- AST now includes enum list in program

**Enum Definition Grammar:**
```
enum_def ::= "enum" IDENT "{" variant_list "}"
variant_list ::= IDENT ("|" IDENT)*
```

### 3. AST (ast.ml) 
- Added `Enum of string` variant to `ty` type
- Added `EnumVariant` expression: `EnumVariant { enum_name: string; variant_name: string; span: Span.t }`
- Added `enum_def` type with variant list
- Extended `program` type with `enums: enum_def list`

**Type System:**
```ocaml
type ty = ... | Enum of string
type enum_def = { name: string; variants: enum_variant list; span: Span.t }
type enum_variant = { name: string; span: Span.t }
```

### 4. Semantic Analysis (sema.ml)
- Added `enum_def` tracking in context: `enums: (string, enum_def) Hashtbl.t`
- Builds enum table during `checkProgram` from all enum definitions
- Updated `checkExprType` to validate enum variants
- **Key: Identifiers can resolve to enum variants** - if an identifier matches a variant name and the expected type is an Enum, it's accepted as a variant reference
- Passes enum table to all checking functions

**Type Checking Logic:**
```ocaml
| Ast.Ident (name, span) ->
    (match expected_ty with
     | Ast.Enum enum_name ->
         (* Check if name is a variant of this enum *)
     | _ -> (* Regular variable check *))
```

### 5. IR (ir.ml)
- Added `EnumVal of string * string` to `value` type (enum_name, variant_name)
- Updated `program_ir` type to include `enums: Ast.enum_def list`
- IR generation checks identifiers against all enum variants to determine if they should become `EnumVal`
- Enum variants create temporary assignment instructions

**IR Value:**
```ocaml
| EnumVal of string * string  (* "Color" * "Red" *)
```

### 6. Codegen (codegen_c.ml)
- Updated `typeToC` to map `Enum` types to `int`
- Enum definitions generate C `#define` constants: `#define Color_Red 0`
- Each variant gets a numeric value (0, 1, 2, ...)
- `EnumVal` codegen produces variant constant reference: `Color_Red`
- Enum values can be printed as integers

**Generated C:**
```c
/* enum Color */
#define Color_Red 0
#define Color_Green 1
#define Color_Blue 2
```

## Example Program (examples/enums.pu)
```purr
enum Color {
    Red | Green | Blue
}

fn get_color() Color {
    return Red;
}

actor Main {
    fn main() {
        var color: Color = Red;
        print("Color assigned");
    }

    on start() {
        main();
    }
}
```

## Type System Additions

### Enum Type Syntax
- `Color` - enum type
- `Red`, `Green`, `Blue` - enum variants (identifiers that match variant names)
- Variants are values of their containing enum type

### Type Checking Rules
1. Variable `var x: Color = Red` - Red must match an enum variant
2. Function return `fn get_color() Color { return Red; }` - Red variant valid
3. Parameter `fn check(c: Color)` - accepts Color-typed enum values

## Limitations (MVP)

### Not Implemented
- **Switch/Pattern Matching** - No switch expressions yet (planned for M8 phase 2)
- **Associated Values** - Enum variants cannot have data fields yet
- **Exhaustiveness Checking** - No match validation
- **Enum Methods** - No enum-specific functions

### Current Behavior  
- Enums are represented as integers in C (0, 1, 2, ...)
- No type safety at runtime (C level doesn't preserve enum type)
- Variant values are just numeric constants
- No destructuring or pattern matching

## Tokens Added
- `Enum` keyword
- `Switch` keyword (reserved for future use)
- `Pipe` operator (single `|`)

## Files Modified
1. `token.ml` - Added Enum, Switch, Pipe tokens
2. `lexer.ml` - Lexing for new tokens, pipe handling
3. `ast.ml` - Enum type, variant expressions, enum definitions
4. `parser.ml` - Enum parsing, parseStructEnumAndActors
5. `sema.ml` - Enum table, type checking, identifier resolution
6. `ir.ml` - EnumVal, enum definitions in program_ir, variant detection
7. `codegen_c.ml` - C code generation for enums

## Validation Approach
1. Enum definitions are validated during parsing (correct syntax)
2. Variant references are type-checked (must match expected enum type)
3. Variable declarations enforce enum type consistency
4. Function signatures with enum types are validated
5. IR generation creates proper enum constant references

## Next Steps (M8 Phase 2)
1. Add `switch` expression for pattern matching
2. Add associated values to enum variants
3. Implement match exhaustiveness checking
4. Add enum method support (static functions on enums)
5. Add option/result types as standard enums

## Code Stats
- **token.ml**: +3 variants (Enum, Switch, Pipe)
- **lexer.ml**: ~5 lines modified (pipe handling)
- **ast.ml**: +3 type additions, 2 record types
- **parser.ml**: +40 lines (parse_enum function)
- **sema.ml**: +40 lines (enum checking)
- **ir.ml**: +15 lines (EnumVal handling)
- **codegen_c.ml**: +15 lines (enum code generation)

## Testing
Ready for gauntlet testing. Example programs:
- `examples/enums.pu` - Basic enum definition and usage
- Gauntlet programs can use enums for state representation

Status: **READY FOR GAUNTLET PHASE 2**
