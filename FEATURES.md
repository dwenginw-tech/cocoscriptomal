# CocoScript v0.5.0 - Major Feature Update

## What's New

### 1. Module System
- Circular dependency detection
- Proper module resolution with search paths
- Better error messages for missing modules

### 2. Garbage Collection
- Mark-and-sweep GC replaces bump allocator
- Automatic memory management
- Configurable GC threshold
- Type-aware object traversal

### 3. Better Error Messages
- Colored terminal output
- Source location tracking (file:line:column)
- Helpful hints for common mistakes
- Proper error types (Syntax, Type, Runtime, Compile)

### 4. Complete Array Utilities
- `map(array, func)` - full closure support
- `filter(array, predicate)` - working implementation
- `sort(array)` - in-place quicksort

### 5. Optimization Pass
- Constant folding for arithmetic
- Dead code elimination
- String concatenation at compile time
- Boolean expression simplification

## Architecture

```
src/
├── lang/          # Lexer, parser, AST
├── backend/       # Code generation, GC, array helpers
├── debug/         # Error handling
├── modules/       # Module resolution
├── optimize/      # Optimization passes
└── driver/        # Compiler driver
```

## Usage

```bash
cocoscript myfile.coco
./myfile
```

## Example

```lua
func double(x)
    return x * 2
end

func main()
    local nums = {1, 2, 3, 4, 5}
    local doubled = map(nums, double)
    sort(doubled)
end
```
