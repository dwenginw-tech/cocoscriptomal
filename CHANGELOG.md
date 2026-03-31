# CocoScript Changelog

## Version 0.2.0 - Standard Library Expansion

### New Features

#### File I/O Operations
- `read_file(path)` - Read entire file contents as string
- `write_file(path, content)` - Write string to file (overwrites)
- `append_file(path, content)` - Append string to file
- `file_exists(path)` - Check if file exists (returns 1 or 0)

#### String Utilities
- `trim(str)` - Remove leading and trailing whitespace
- `upper(str)` - Convert string to uppercase
- `lower(str)` - Convert string to lowercase
- `starts_with(str, prefix)` - Check if string starts with prefix
- `ends_with(str, suffix)` - Check if string ends with suffix
- `split(str, delim)` - Split string by delimiter (basic implementation)
- `replace(str, old, new)` - Replace substring (placeholder)

#### Array Utilities
- `map(array, func)` - Map function over array (placeholder)
- `filter(array, func)` - Filter array by predicate (placeholder)
- `sort(array)` - Sort array (placeholder)

### Improvements

- Enhanced type inference to recognize new builtin functions
- Better error handling for file operations
- Cross-platform file I/O support (Windows and Linux)

### Module System (In Progress)

- Added `import`, `from`, and `as` keywords to lexer
- Tokens ready for future module system implementation
- Current `#include` system still functional

### Examples

New example files demonstrating features:
- `examples/strings.coco` - String utility demonstrations
- `examples/fileio.coco` - File I/O operations
- `examples/stdlib_demo.coco` - Comprehensive standard library demo

### Notes

- Array utility functions (map, filter, sort) have placeholder implementations
- Full implementations require closure calling support
- `replace()` function currently returns original string unchanged

## Version 0.1.0 - Initial Release

- Basic language features
- Math and string libraries
- Classes and closures
- Cross-platform compilation
