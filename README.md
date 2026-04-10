# CocoScript

A native compiled programming language with C/Lua hybrid syntax. Built in OCaml, compiles to x86-64 assembly via NASM.

Supports Windows and Linux with platform-specific optimizations.

## Example

```lua
#include "io"

func factorial(n)
    if n <= 1 then
        return 1
    end
    return n * factorial(n - 1)
end

func main()
    print("5! =")
    print(factorial(5))
    halt()
end
```

## Features

- Cross-platform: Windows and Linux support
- Integers, floats, booleans, strings, nil
- Arrays with index read/write
- String concatenation with `..`
- Variables with `local`
- Functions and recursion
- Classes with methods and fields
- Closures and lambdas
- `if`/`elseif`/`else`/`end` blocks
- `while`/`do`/`end` loops
- `for` and `foreach` loops
- Operators: `+ - * / % == != < > <= >= and or not`
- Builtins: `print`, `input`, `exec`, `halt`, `exit`, `len`, `tostring`, `toint`, `tofloat`
- Math library: `sqrt`, `floor`, `ceil`, `pow`, `sin`, `cos`, `tan`, `fmod`
- String library: `substr`, `char_at`, `char_code`, `from_char_code`, `index_of`, `split`, `trim`, `replace`, `upper`, `lower`, `starts_with`, `ends_with`
- File I/O: `read_file`, `write_file`, `append_file`, `file_exists`
- Array utilities: `push`, `pop`, `map`, `filter`, `sort`
- Module system with `#include` (import syntax coming soon)

## Installation

### Linux (Recommended)

Quick install with cocovm:

```bash
curl -o- https://raw.githubusercontent.com/dwenginw-tech/cocoscriptomal/main/install.sh | bash
```

Or manually:

```bash
git clone https://github.com/dwenginw-tech/cocoscriptomal
cd cocoscriptomal
chmod +x install.sh
./install.sh
```

Then restart your terminal or run `source ~/.bashrc`

### Windows

Download the installer from the [releases page](https://github.com/dwenginw-tech/cocoscriptomal/releases).

The installer includes everything needed: compiler, NASM, GCC, and standard library.

## Usage

### Linux

```bash
cocoscript myfile.coco
./myfile
```

Or use the helper script:

```bash
./run.sh myfile.coco
```

### Windows

```cmd
cocoscript myfile.coco
myfile.exe
```

Or use the helper script:

```cmd
run.bat myfile.coco
```

## Managing CocoScript (Linux)

```bash
cocovm update     
cocovm uninstall 
cocovm version   
```

## Build from source

Requires OCaml 5.4.1, opam, dune, NASM, and GCC (MinGW on Windows).

```bash
opam exec -- dune build
```

Compile a file:

```bash
opam exec -- dune exec cocoscript myfile.coco
```

## End users

End users only need the CocoScript binary, NASM, and GCC. No OCaml required.

- Linux: Dependencies are typically already installed or easily available via package manager
- Windows: Bundled installer includes everything

## License

MIT — the compiler is open source. Programs you write in CocoScript can be any license you want.

## New Features Guide

### File I/O

CocoScript now supports file operations:

```lua
write_file("output.txt", "Hello, World!")

local content = read_file("input.txt")
if content then
    print(content)
end
append_file("log.txt", "New log entry\n")

if file_exists("config.txt") == 1 then
    print("Config found")
end
```

### String Utilities

Enhanced string manipulation:

```lua
local text = "  Hello World  "

local clean = trim(text)  

local upper = upper("hello") 
local lower = lower("WORLD") 

if starts_with("hello world", "hello") == 1 then
    print("Match!")
end

if ends_with("test.txt", ".txt") == 1 then
    print("Text file")
end

local parts = split("a,b,c", ",")
```

### Array Operations

Work with arrays more easily:

```lua
local numbers = {1, 2, 3, 4, 5}

push(numbers, 6)

local last = pop(numbers)

-- Array utilities (coming soon: full implementations)
-- local doubled = map(numbers, func(x) return x * 2 end)
-- local evens = filter(numbers, func(x) return x % 2 == 0 end)
-- sort(numbers)
```

## CocoScript v0.5.0 Implementation Summary

### High-Impact Features Implemented

#### 1. Module System (`src/modules/`)
**Files:**
- `module_resolver.ml` - Module loading, dependency resolution, circular dependency detection

**Features:**
- Search path resolution (current dir, ./lib, exe/lib, src/lib)
- Circular dependency detection with helpful error messages
- Recursive dependency loading
- Builtin module support (io, math, string)

**Integration:**
- Updated `compiler.ml` to use `Module_resolver.resolve_imports`
- Checks circular deps before compilation

#### 2. Garbage Collection (`src/backend/gc_marksweep.ml`)
**Replaces:** Simple bump allocator in `gc.ml`

**Features:**
- Mark-and-sweep algorithm
- Object headers with type tags (int, string, array, object, closure)
- Automatic collection when threshold reached
- Heap compaction during sweep
- Root set management (max 1024 roots)

**Functions:**
- `_gc_init` - Initialize 8MB heap
- `_gc_mark` - Recursive marking phase
- `_gc_sweep` - Sweep and compact
- `_gc_collect` - Full collection cycle
- `_gc_alloc` - Allocate with automatic GC

**Memory Layout:**
```
Header (8 bytes):
[0-3]: size
[4]: mark bit
[5]: type tag
[6-7]: reserved
```

#### 3. Better Error Messages
**Enhanced Files:**
- `parser.ml` - Added filename, line, column tracking
- `token.ml` - Already had `to_string` function

**Changes:**
- Parser state now tracks `filename`, `line`, `column`
- `expect` function uses `Error.syntax_error` with location
- `eat` function updates column counter
- All parse errors show exact location

**Example Output:**
```
Syntax Error: Unexpected token: }
  --> myfile.coco:15:8
  Hint: Expected end
```

#### 4. Complete Array Utilities (`src/backend/codegen.ml`, `array_helpers.ml`)
**Implemented:**
- `map(array, func)` - Calls closure for each element, returns new array
- `filter(array, predicate)` - Returns new array with matching elements  
- `sort(array)` - In-place quicksort via `_coco_quicksort` helper

**array_helpers.ml:**
- `emit_quicksort` - Full quicksort implementation in x86-64 assembly
- Handles both Linux (System V) and Windows (x64) calling conventions
- Recursive partitioning with proper register preservation

#### 5. Optimization Pass (`src/optimize/`)
**Files:**
- `optimizer.ml` - AST-level optimizations
- `dune` - Build configuration

**Optimizations:**
- Constant folding (arithmetic, boolean, string concat)
- Dead code elimination (if true/false branches)
- Expression simplification
- Recursive optimization of nested structures

**Examples:**
```
2 + 3 * 4 → 14
"hello" .. " world" → "hello world"
if true then ... → (optimized away)
```

### Module Organization

```
src/
├── lang/          # Frontend (lexer, parser, AST, tokens)
│   ├── lexer.ml
│   ├── parser.ml  (enhanced with error tracking)
│   ├── ast.ml
│   ├── token.ml
│   └── class.ml
├── backend/       # Code generation
│   ├── codegen.ml (enhanced with full map/filter/sort)
│   ├── gc.ml (old bump allocator)
│   ├── gc_marksweep.ml (new GC)
│   ├── array_helpers.ml (quicksort)
│   └── dune
├── debug/         # Error handling
│   ├── error.ml (already existed, now integrated)
│   └── dune
├── modules/       # Module system (NEW)
│   ├── module_resolver.ml
│   └── dune
├── optimize/      # Optimizations (NEW)
│   ├── optimizer.ml
│   └── dune
└── driver/        # Compiler driver
    ├── compiler.ml (updated to use modules + optimizer)
    ├── main.ml
    └── dune (updated dependencies)
```

### Build Configuration Updates

**dune files updated:**
- `src/driver/dune` - Added `modules` and `optimize` libraries
- `src/backend/dune` - Created with `lang` and `debug` dependencies
- `src/modules/dune` - Created with `lang` and `debug` dependencies
- `src/optimize/dune` - Created with `lang` dependency

### Testing

**New test file:**
- `examples/test_features.coco` - Tests map, filter, sort with closures

### Breaking Changes

None - all changes are backwards compatible. Old code still works.

### Performance Improvements

1. GC reduces memory usage (no more unbounded growth)
2. Constant folding eliminates runtime computation
3. Optimized array operations with proper closure calling
