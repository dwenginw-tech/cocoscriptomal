# CocoScript Standard Library Reference

## File I/O

### read_file(path)
Reads entire file contents as a string.

```lua
local content = read_file("data.txt")
if content then
    print(content)
else
    print("File not found")
end
```

**Returns**: String on success, nil on error

### write_file(path, content)
Writes string to file (overwrites existing content).

```lua
local result = write_file("output.txt", "Hello, World!")
if result == 1 then
    print("Success")
end
```

**Returns**: 1 on success, 0 on error

### append_file(path, content)
Appends string to end of file.

```lua
append_file("log.txt", "New entry\n")
```

**Returns**: 1 on success, 0 on error

### file_exists(path)
Checks if file exists.

```lua
if file_exists("config.txt") == 1 then
    print("Config found")
end
```

**Returns**: 1 if exists, 0 otherwise

## String Utilities

### trim(str)
Removes leading and trailing whitespace (spaces, tabs, newlines).

```lua
local clean = trim("  hello  ")  -- "hello"
```

### upper(str)
Converts string to uppercase.

```lua
local loud = upper("hello")  -- "HELLO"
```

### lower(str)
Converts string to lowercase.

```lua
local quiet = lower("WORLD")  -- "world"
```

### starts_with(str, prefix)
Checks if string starts with prefix.

```lua
if starts_with("hello world", "hello") == 1 then
    print("Match!")
end
```

**Returns**: 1 if true, 0 if false

### ends_with(str, suffix)
Checks if string ends with suffix.

```lua
if ends_with("test.txt", ".txt") == 1 then
    print("Text file")
end
```

**Returns**: 1 if true, 0 if false

### split(str, delimiter)
Splits string by delimiter (basic implementation).

```lua
local parts = split("a,b,c", ",")
```

**Note**: Currently returns placeholder array

### replace(str, old, new)
Replaces substring (placeholder).

```lua
local result = replace("hello world", "world", "there")
```

**Note**: Currently returns original string unchanged

## Array Operations

### push(array, value)
Adds element to end of array.

```lua
local nums = {1, 2, 3}
push(nums, 4)  -- nums is now {1, 2, 3, 4}
```

### pop(array)
Removes and returns last element.

```lua
local nums = {1, 2, 3}
local last = pop(nums)  -- last = 3, nums = {1, 2}
```

### map(array, function)
Maps function over array (placeholder).

```lua
-- Coming soon
-- local doubled = map(numbers, func(x) return x * 2 end)
```

### filter(array, function)
Filters array by predicate (placeholder).

```lua
-- Coming soon
-- local evens = filter(numbers, func(x) return x % 2 == 0 end)
```

### sort(array)
Sorts array in place (placeholder).

```lua
-- Coming soon
-- sort(numbers)
```

## Existing Builtins

### I/O
- `print(value, ...)` - Print values to stdout
- `input()` - Read line from stdin

### Type Conversion
- `tostring(value)` - Convert to string
- `toint(value)` - Convert to integer
- `tofloat(value)` - Convert to float

### String Operations
- `len(str)` - Get string length
- `substr(str, start, length)` - Extract substring
- `char_at(str, index)` - Get character at index
- `char_code(str)` - Get ASCII code of first character
- `from_char_code(code)` - Create string from ASCII code
- `index_of(haystack, needle)` - Find substring position

### Math Functions
- `sqrt(x)` - Square root
- `floor(x)` - Round down
- `ceil(x)` - Round up
- `pow(base, exp)` - Exponentiation
- `sin(x)`, `cos(x)`, `tan(x)` - Trigonometric functions
- `fmod(x, y)` - Floating-point modulo

### System
- `exec(command)` - Execute shell command
- `exit(code)` - Exit program with code
- `halt()` - Pause and wait for keypress

## Usage Examples

### File Processing
```lua
func main()
    -- Read input
    local data = read_file("input.txt")
    if not data then
        print("Error reading file")
        exit(1)
    end
    
    -- Process
    local processed = upper(trim(data))
    
    -- Write output
    if write_file("output.txt", processed) == 0 then
        print("Error writing file")
        exit(1)
    end
    
    print("Done!")
end
```

### String Manipulation
```lua
func validate_filename(name)
    -- Check extension
    if ends_with(name, ".txt") == 0 and 
       ends_with(name, ".coco") == 0 then
        return 0
    end
    
    -- Check prefix
    if starts_with(name, ".") == 1 then
        return 0  -- Hidden file
    end
    
    return 1
end
```

### Array Building
```lua
func collect_numbers()
    local nums = {}
    local i = 1
    while i <= 10 do
        push(nums, i * i)
        i = i + 1
    end
    return nums
end
```

## Notes

- File paths are relative to current working directory
- String functions allocate new memory (no in-place modification)
- Array utilities (map, filter, sort) have placeholder implementations
- Boolean values are represented as integers (1 = true, 0 = false)
- File operations return error codes for checking success/failure
