## On AST Memory Usage

The tagged unions are wasting really a lot of memory when one or two of the variants are large and not frequent, 
it makes things worse when you directly put one abstract base class into other (Expr in Statement, TypeRef in Statement, etc.).
Before this commit, the Expr type is using nearly 100 bytes, the Statement type is using amazingly 240 bytes.
Some fixes are tried in this commit, by putting things into Boxes, but this operation makes the runtime cost more close
to standard runtime dispatch based on vtables and more deref operations in other OOP languages

The standard solution is to put object into pools (one for each type) and using indexes to reference them, 
in this perticular case of ast, you can convert index to u32 like source::Position and source::IsId because their count will not be more than that

BUT, I'd like to use a more aggresive and unsafe solution, use a "unified object pool", where
- an object pool is shared within a tree
- objects are "serialized" into byte streams into the pool, vectors are "inlined" into the structure, 
  works like C++ `struct S { int other_data; int length; int data[0] }`, this should reduce 
  a lot of memory waste caused by standard vectors 3*size_t layout
- considering ast objects does not change after construction, 
  objects are transmuted in place for reading, the "only-len" vector storage strategy is also based on this 

This is complex refactor and not very practical (it's very unsafe) and need a lot of fine tune to make it work correctly, 
and mast may also need this, so it is sheduled to work after I have basic name resolving structure implemented,
where a lot of ast information will be read and processed and I can considered how actually the "reading" operation is processed and design the reading interface

Additional note:
- ast types may need PhantomData<&'ast ()> to indicate their actual
  lifetime where `'ast` is lifetime of the pool `&'ast Vec<u64>`
- objects should be aligned as u64 because nearly every node has one or
  several Spans and they need to align as u64
- every object type may need its own `XXXIndex` type wrapping u32 to make type stronger, in that case, also considering
  the pretty implementation is actually very mechanical, I'd try to declare them in DSL and generate them by python
