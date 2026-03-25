# CocoScript

A native compiled programming language with C/Lua hybrid syntax. Built in OCaml, compiles to x86-64 assembly via NASM on Windows.

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

- Integers, floats, booleans, strings, nil
- Arrays with index read/write
- String concatenation with `..`
- Variables with `local`
- Functions and recursion
- `if`/`elseif`/`else`/`end` blocks
- `while`/`do`/`end` loops
- `for` loops
- Operators: `+ - * / % == != < > <= >= and or not`
- Builtins: `print`, `input`, `exec`, `halt`, `exit`

## Build from source

Requires OCaml 5.4.1, opam, dune, NASM, and GCC (MinGW).

```
opam exec -- dune build
```

## Compile a .coco file

```
opam exec -- dune exec cocoscript myfile.coco
myfile.exe
```

## End users

End users only need `cocoscript.exe`, NASM, and GCC. No OCaml required. Bundled releases will include everything in one zip.

## License

GPL v3 — the compiler is open source. Programs you write in CocoScript can be any license you want.
