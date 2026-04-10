# TODO - Integration Tasks

## Critical (Required for v0.5 to work)

1. **Switch allocator in codegen.ml**
   - Replace all `_coco_alloc` calls with `_gc_alloc`
   - Add type tags when allocating (string=1, array=2, object=3, closure=4)
   - Initialize object headers properly

2. **Add GC initialization**
   - Call `_gc_init` at program start (before main)
   - Emit `_gc_init` call in generated assembly

3. **Root registration**
   - Register stack variables as GC roots
   - Unregister when leaving scope
   - Track closure environments

4. **Link new modules**
   - Ensure `gc_marksweep.ml` is compiled
   - Ensure `array_helpers.ml` is compiled
   - Update `codegen.ml` to call `Gc_marksweep.emit_gc_runtime`

5. **Parser integration**
   - Pass filename to `Parser.parse`
   - Update all `failwith` calls to use `Error.*_error`

## Nice to Have

- GC stress testing
- Memory leak detection
- Performance benchmarks
- More optimization passes
- Better type inference

## Known Issues

- GC not yet integrated into codegen
- Parser still has some `failwith` calls
- No GC root tracking yet
- Type tags not set during allocation
