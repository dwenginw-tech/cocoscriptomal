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
