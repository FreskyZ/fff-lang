## On AST/MAST Memory Usage

The tagged unions are wasting really a lot of memory when some of the variants are large, it makes 
things worse when you directly put one tagged union into another (Expr in Statement, TypeRef in 
Statement, etc.). Currently (when this document is written), the Expr type is using nearly 100 bytes,
the Statement type is using amazingly 240 bytes, and --print ast-mem shows that more than 80% percent
of memory space is wasted for tagged union padding, see [mem.txt](../../tests/ast/mem.txt).

Box many things helps little, but this operation makes the runtime cost closer to 
common runtime dispatch stategy based on vtables and more deref operations in other OOP languages.

The standard solution is to use arena, or [region based memory management] strategy, 
it has a more rust style definition in rust, called "a group of collections with same lifetime",
you allocate only several times large chunks of memory space, and use simple incrementing 
pointer for new allocations, when these allocations lifetime goes to end, no drop for each object,
all allocations in these chunks are thrown (or drop, or deallocate) at the same time.

More specificaly, in this project, ast and mast,
- an arena manager is shared between all ast nodes and mast nodes
- objects are constructed "in place" inside the arena, *but*, rust in place construction is unstable
  and even removed from standard library, so it is actually implemented by allocate same size place
  from arena, transmute it into mutable reference, and copy constructor parameters into, after that,
  a typed index is returned for other objects to reference this object, using integeral index should
  be easier to copy and saves more space (index is u32, reference is size_t)
- every reading operation needs to access arena to access to real object, include read field, formatting
  and even comparing, this may make unit test kind of complex but I think it will finally be ok
- for field types
  - "terminal" fields include IsId, Span, IdSpan, Keyword and Separator
  - optional terminal fields are 0, because all of them (except Span, investigate later) does not have 0
  - directly owned node, include bare object or boxed object, they will all become object index
  - optional directly owned node fields are 0, because object index cannot be 0
  - list of nodes are stored as slice (data + len fat pointer), it is too complex to inline variable
    length list inside object, especially when there is multiple lists (impl block)

About arena manager implementation detail
- all values inside arena is u32 aligned,
  - IsId is u32
  - Keyword and Separator is u8, regard them as u32 should be easier
  - Span contains 2 Positions, or 2 u32s, although it fits in an u64, it is u32 aligned
  - object index, and future def index in mast will also use u32 as underlying type
- chunk size is 4k bytes
  - which is not waste for small programs, e.g. 30 lines of code including 2 or 3 functions
    will use like 2k or 3k bytes of memory),
  - and 4k chunk will happily fits in a memory page, 
    or happily aligned inside a large memory page, which is expected to be kind of cache friendly.

The auto derived Debug and PartialEq for unit test is very not usable after that, there should be manual
implementation for Debug which respect tree structure but only prints bare integral values for ids, and
manual PartialEq implementations which follow object indexes and only compares terminal fields.
Considering this issue, plus some other boring-to-write-in-hand issues, I will make another auto generation
script for ast types for Debug, PartialEq, Node, MemoryProfiler and maybe FormatVisitor implementations.

[region based memory management]: (https://en.wikipedia.org/wiki/Region-based_memory_management)
