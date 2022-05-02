///! A region based memory management strategy implementation
///!
///! see [memory-management.md](docs/internal/memory-management.md) about motivation
///! it is different from [typed-arena] that this arena allows different types of object
///! it is different from [bumpalo] that this arena uses handles (index/slice) instead of reference
///!
///! [typed-arena](https://crates.io/crates/typed_arena)
///! [bumpalo](https://crates.io/crates/bumpalo)

// this is currently the reason this project needs nightly,
// there is std::alloc::alloc method, but they said they will deprecate when Allocator trait stablized
use std::alloc::{Global, Allocator, Layout};
use std::cell::RefCell;
use std::fmt;
use std::marker::PhantomData;
use std::mem::{size_of, align_of};
use std::num::{NonZeroU32, NonZeroUsize};
use std::ptr::NonNull;

#[cfg(test)]
mod tests;

// Some approaches / detailed designs that tried / considered and abort
// 1. typed arena,
//    because there is many different types inside an ast
// 2. reference based,
//    because index saves memory than 8 byte reference
// 3. derive `Clone`/`Copy` for `Index` regardless of underlying `T`, and pass `Index` as value in `get{_mut}` methods
//    it violates reference alias rule that you can claim 2 mutable reference of same object,
//    which may make hard-to-debug runtime errors and incorrect optimizations (rustc thinks 2 mutable reference do not alias),
//    so currently `get{_iter}{_mut}` methods binds the returned reference to index/slice, which can restrict returned reference
//    to index/slice's lifetime, which is very correct for index/slice's "owned handle" semantic
// 4. use `ops::Index` for `Arena::get{_iter}{_mut}`
//    the returned reference binds to self not the index so cannot use
// 5. bit operation the tag of a tagged union (enum, e.g. Expr) into the index, which is normally small and not using top bits
//    1. similar to index/slice, the tagged index would want to be used like `&TagIndex => &Enum`,
//       but there is no place to let the reference (the pointer) points to, for index/arena, their return value
//       points to actual data in arena (in one chunk), but tagged index is a compressed tag + index, not residual in arena,
//       you definitely cannot simply use some `TagIndex::as_ref()` or `Arena::get_tagged()` to get the reference
//    2. there was a try that create a `TagIndexRepr` to actually hold the values and cast itself to a reference to enum, 
//       which normally looks like `index.as_repr().as_ref()` and is even longer for mutable version `index.as_repr_mut().as_mut()`,
//       TagIndexRepr itself is not a reference and rely on rustc (maybe nll) to store this temp value to caller's stack,
//       you cannot assign the returned value to local variable because the reference's lifetime is restricted to the
//       method chain expression (like `stdin().lock()`), so the method chain is spreaded everywhere where tagged index is involved,
//       and I'm not sure whether this temp value promotion will always happen, so this approach is abandoned
//    3. another issue of `TagIndexRepr` is that relies on tagged union's layout, which is not guaranteed if you forget
//       the `#[repr(C)]` or `#[repr(u8)]` on enum definition, it will cause very strange runtime errors if rust decide to 
//       use u32 as tag value but you assume it's u8 in repr, segment fault at least 
// 6. inline slice in struct, e.g. `struct ArrayExpr { span: Span, items: [Expr] }`
//    1. it is hard to construct an instance of this type, even with the help of `place` or `emplace` methods,
//       because you cannot easily assign a variable to `[Expr]` even holds a variable with value `[Expr]` (not `&[Expr]`)
//    2. it does not support multiple slices in one struct, e.g. `struct FnDef { span: Span, parameters: [FnParameter], wheres: [Where] }`,
//       you simply don't know where `wheres` is, which for normal structs, the offset is known by rustc and
//       constant in generated code, add something like node.parameters(), node.wheres() is too complex and runtime cost
// 7. inline data in slice
//    it is actually ok to put same type objects directly in where slice's data points to, but caller's usage makes this incorrect,
//    currently syntax `parse_*` methods returns index for normal (not enum) nodes, which means they are already constructed from local
//    variables and allocated in arena before return, that's also important for the `place` or `emplace` design or else you have to
//    move (memcpy) the whole structure and makes `place` or `emplace` method meaningless, BUT this design also means you can allocate
//    many other objects between 2 logically continued item (fields for next item), so the original `build_slice` impementation, which
//    consumes a `Vec<Index>` and only records head index and len, which assumes the `Vec<Index>` item is continuous, is very incorrect,
//    so the original `Slice<T>` usage have to change to `Slice<Index<T>>`, whose iterator yields `Index<T>`, making this a double reference,
//    logically a Vec<&T>, but necessary, on the other hand, the original `Slice<TagIndex<U>>`, after 5, can be changed to `Slice<U>`, where
//    all enum values, whose size is always 2 u32s, can inline in slice's data pointer, and returning direct value in e.g. `parse_expr`
//    methods makes the signature a lot easier and test methods kind of easier
// 8. fixed size chunk
//    no, fixed size chunk is easy to implement and hard to make mistake, but `rustc_arena`'s
//    increasing chunk size design seems very great and interesting, so I change, too, with a very long partially auto generated test

// general naming convension
// T: type parameter for a normal node in tree
// E: type parameter for an enum in tree
// 'a: lifetime parameter for the arena
// 'b: lifetime parameter for the handle

/// A handle to an object in arena
///
/// # No Derive
///
/// - this is not `derive(Copy, Clone)`
///   because they allow 2 mutable reference of same object to exist, which violates reference alias rule
/// - this is not `derive(Debug, Display, Hash, ...)`
///   because the underlying numeric value is meaningless, data structures involving indexes should always use this with arena
pub struct Index<'a, T> {
    v: NonZeroU32,
    // this should not outlive arena
    p1: PhantomData<&'a Arena>,
    // this is an owned handle of T
    p2: PhantomData<T>,
}

/// A handle to a fixed length sequence of objects in arena
/// 
/// According to actual experience in syntax parser,
/// normal nodes should use `Slice<Index<T>>` while enum nodes can use e.g. `Slice<Expr>`
/// 
/// see [`Index`] about no derives
pub struct Slice<'a, T> {
    // use len + head instead of begin + end to allow fast .len()
    len: u32,
    // index value points to first element
    data: NonZeroU32,
    // slice should be restricted to arena's lifetime
    p1: PhantomData<&'a Arena>,
    // slice is an owned handle of sequence of T
    p2: PhantomData<[T]>,
}

impl<'a, T> Slice<'a, T> {

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

const fn get_chunk_size(chunk_index: usize) -> usize {
    const SIZES: [usize; 11] = [
        1 << 12, 1 << 12, 1 << 13, 1 << 14, 1 << 15, 1 << 16, 1 << 17, 1 << 18, 1 << 19, 1 << 20, 1 << 21,
    ];
    if chunk_index < SIZES.len() { SIZES[chunk_index] } else { 1 << 21 }
}

const fn get_chunk_layout(size: usize) -> Layout {
    // SAFETY: align is const, size comes from get_chunk_size
    unsafe {
        Layout::from_size_align_unchecked(size, 1 << 12)
    }
}

const fn get_chunk_base_index(chunk_index: usize) -> usize {
    const BASES: [usize; 11] = [
        0, 1 << 12, 1 << 13, 1 << 14, 1 << 15, 1 << 16, 1 << 17, 1 << 18, 1 << 19, 1 << 20, 1 << 21,
    ];
    if chunk_index < BASES.len() { BASES[chunk_index] } else { (chunk_index - 9) << 21 }
}

// get chunk index and offset inside chunk
const fn get_chunk_index_and_offset(index: u32) -> (usize, usize) {
    // >> 12
    // 0x00000 => 0 (4k) clz >= 20
    // 0x00001 => 1 (4k) clz = 19
    // 0x00002 ..= 0x00003 => 2 (8k) clz = 18
    // 0x00004 ..= 0x00007 => 3 (16k) clz = 17
    // 0x00008 ..= 0x0000F => 4 (32k) clz = 16
    // 0x00010 ..= 0x0001F => 5 (64k) clz = 15
    // 0x00020 ..= 0x0003F => 6 (128K) clz = 14
    // 0x00040 ..= 0x0007F => 7 (256K) clz = 13
    // 0x00080 ..= 0x000FF => 8 (512K) clz = 12
    // 0x00100 ..= 0x001FF => 9 (1024K) clz = 11, dummy mask 0x00000 => 0
    // 0x00200 ..= 0x003FF => 10 (2048K) clz = 10, dummy mask 0x00000 => 0
    // 0x00400 ..= 0x005FF => 11 (2048K) clz = 9, mask 0x00_0b0010_0x00 => 0
    // 0x00600 ..= 0x007FF => 12 (2048K) clz = 9, mask 0x00_0b0010_0x00 => 1
    // 0x00800 ..= 0x009FF => 13, clz = 8, mask 0x00_0b0110_0x00 => 00
    // 0x00A00 ..= 0x00BFF => 14, clz = 8, mask 0x00_0b0110_0x00 => 01
    // 0x00C00 ..= 0x00DFF => 15, clz = 8, mask 0x00_0b0110_0x00 => 10
    // 0x00E00 ..= 0x00FFF => 16, clz = 8, mask 0x00_0b0110_0x00 => 11
    // 0x01000 ..= 0x01FFF => 17 ..= 24, clz = 7, mask 0x00_0b1110_0x00 => 000 ..= 111
    // 0x02000 ..= 0x03FFF => 25 ..= 40, clz = 6, mask 0x0_0b0001_0b1110_0x00 => 0000 ..= 1111
    // 0x04000 => 41 ..= 72
    // 0x08000 => 73 ..= 136
    // 0x10000 => 137 ..= 264
    // 0x20000 => 265 ..= 520
    // 0x40000 => 521 ..= 1032 mask 0011_1111_1110_FF
    // 0x80000 => 1033 ..= 2056 mask 0111_1111_1110_FF
    //
    // input i
    // output LEN_TABLE[clz(i)] + (i & MASK_TABLE[clz(i)]) // note that this mask table is not after >>12
    const LEN_TABLE: [usize; 33] = [1033, 521, 265, 137, 73, 41,
        25, 17, 13, 11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    // create mask table with expected count of 1s
    macro_rules! mt { [$($i:expr),+] => ([$((1 << $i) - 1,)+]) }
    const ADD_TABLE: [u32; 33] = mt![10, 9, 8, 7, 6, 5,
        4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    const OFFSET_TABLE: [u32; 33] = mt![21, 21, 21, 21, 21, 21,
        21, 21, 21, 21, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12];
    
    let z = index.leading_zeros() as usize;
    (LEN_TABLE[z] + ((index >> 21) & ADD_TABLE[z]) as usize, (index & OFFSET_TABLE[z]) as usize)
}

struct Chunk {
    next: NonNull<u8>,
    head: NonNull<u8>,
}

impl Chunk {

    fn new(size: usize) -> Chunk {
        let head = match Global.allocate(get_chunk_layout(size)) {
            Ok(head) => {
                // cast: allocate returns NonNull<[u8]>, 
                // that does not allow `offset` like methods which rely on T: Sized
                let head = head.cast();
                // ptr::offset_from seems indicating this should be guaranteed by current implementation,
                // but I did not found it in implementaion or document, so assert here (this is not debug assert)
                // // this will not happen in major linux or windows platforms, because this high virtual address is used by os
                assert!(head.addr().get() < usize::MAX - size, "invalid alloc result");
                head
            },
            // AllocError does not provide any information and actually nothing more can do here
            Err(_) => panic!("oom"),
        };
        Chunk{ head, next: head }
    }

    // return none for not enough space
    fn allocate(&mut self, chunk_index: usize, layout: Layout) -> Option<(NonNull<u8>, NonZeroU32)> {
        // validate size
        let chunk_size = get_chunk_size(chunk_index);
        assert!(layout.size() <= chunk_size, "too large object");
        // min align 4
        let align = if layout.align() < 4 { 4 } else { layout.align() };
        let align_mask = align - 1;

        // SAFETY:
        // // from doc // these lines are really long
        // // 1. Both starting and other pointer must be either in bounds or one byte past the end of the same allocated object
        // // 2. Both pointers must derive from a pointer to the same object
        // // 3. The distance between the pointers, in bytes, must be an exact multiple of the size of T
        // // 4. The distance between the pointers, in bytes, cannot overflow an isize
        // // 5. The distance being in bounds cannot rely on wrapping arround the address space
        // 3. T is u8
        // others. self.head and self.next all derive from the same allocated object self.head,
        //         the object size is CHUNK_SIZE which is correctly less than isize for 16/32/64bit platforms,
        //         self.next is initialized same as self.head,
        //         self.head does not change, self.next is only changed by simple add,
        //         self.next does not overflow this object is guaranteed by the following if,
        //         self.next does not overflow max value of usize is guranteed by assert in Self::new
        let offset = unsafe {
            // as usize: previous comment explain that self.next is only same or large than self.head
            self.next.as_ptr().offset_from(self.head.as_ptr()) as usize
        };
        let align_padding = if offset & align_mask == 0 { 0 } else { align - (offset & align_mask) };

        if offset + align_padding + layout.size() <= chunk_size {
            // SAFETY: previous comment explain that result is still inside self.head and not null
            let real = unsafe { NonNull::new_unchecked(self.next.as_ptr().add(align_padding)) };
            // SAFETY: previous comment explain that result does not wrapping and is not null
            self.next = self.next.map_addr(|a| unsafe {
                NonZeroUsize::new_unchecked(a.get() + align_padding + layout.size())
            });
            // byte index start from 0, from beginning of first
            // chunk regarding all chunks as one block of continuous memory,
            // SAFETY: the first u32 in first chunk is not used, making this non null
            let index = unsafe {
                NonZeroU32::new_unchecked((get_chunk_base_index(chunk_index) + offset + align_padding) as u32)
            };
            Some((real, index))
        } else {
            None
        }
    }
}

pub struct Arena {
    // Vec: 
    //   complex operation like puting chunk structure into
    //   another chunk or some linked list simply repeat a vector's work
    // RefCell:
    //   a memory allocator cannot be mutable (allocate related method cannot mutable borrow self),
    //   or else the first allocated object constantly mutable borrows self and the program cannot do anyhing
    // RefCell make this struct not Sync, it may be better to store this inside TLS if multithreaded
    chunks: RefCell<Vec<Chunk>>,
}

impl Drop for Arena {

    fn drop(&mut self) {
        for (chunk_index, chunk) in self.chunks.borrow().iter().enumerate() {
            // SAFETY: chunk.head is allocated as get_chunk_layout(get_chunk_size) and does not change
            unsafe { Global.deallocate(chunk.head, get_chunk_layout(get_chunk_size(chunk_index))); }
        }
    }
}

impl Arena {

    pub fn new() -> Self {

        // include first chunk in new, because you never want an empty arena
        let mut first_chunk = Chunk::new(get_chunk_size(0));
        // index 0 is not used, SAFETY: usize add 4 is safe, no overlapping explained
        first_chunk.next = first_chunk.next.map_addr(|a| unsafe { NonZeroUsize::new_unchecked(a.get() + 4) });

        Self{ chunks: RefCell::new(vec![first_chunk]) }
    }

    fn allocate(&self, layout: Layout) -> (NonNull<u8>, NonZeroU32) {
        let mut chunks = self.chunks.borrow_mut();
        let chunk_index = chunks.len() - 1;
        assert!(layout.size() <= get_chunk_size(chunk_index + 1), "too large object");

        // SAFETY: self.chunks is not empty
        let last_chunk = unsafe { chunks.last_mut().unwrap_unchecked() };
        if let Some(addr) = last_chunk.allocate(chunk_index, layout) {
            return addr;
        }

        // you cannot put this in match { None } or .or_else,
        // because borrow mut of self.chunks may not be dropped and that's runtime error
        chunks.push(Chunk::new(get_chunk_size(chunk_index + 1)));
        // SAFETY: self.chunks is not empty
        let last_chunk = unsafe { chunks.last_mut().unwrap_unchecked() };
        // SAFETY: layout size is less than CHUNK_SIZE and must success for a new chunk
        unsafe { last_chunk.allocate(chunk_index + 1, layout).unwrap_unchecked() }
    }

    /// Allocate an object in place,
    /// note that this is actually unsafe because the passed reference
    /// in `init` is not initialized, `init` must set all fields with valid init value and do not read any field
    /// 
    /// Note that returned index is restricted to arena lifetime
    /// 
    /// ```compile_fail
    /// # use fflib::common::arena::Arena;
    /// let index = { let arena = Arena::new(); arena.emplace(|_: &mut i32| {}) }; // borrowed value does not live long enough
    /// ```
    pub fn emplace<'a, T: 'a, F: FnOnce(&'a mut T)>(&'a self, init: F) -> Index<'a, T> where T: Sized {
        let (real, index) = self.allocate(Layout::new::<T>());
        // SAFETY:
        // // from doc
        // // 1. The pointer must be properly aligned.
        // // 2. It must be "dereferenceable" in the sense defined in the module documentation.
        // //    the memory range of the given size starting at the pointer must all be within the bounds of a single allocated object. 
        // //    Note that in Rust, every (stack-allocated) variable is considered a separate allocated object
        // // 3. The pointer must point to an initialized instance of T.
        // // 4. You must enforce Rust's aliasing rules, since the returned lifetime 'a is arbitrarily chosen 
        // //    and does not necessarily reflect the actual lifetime of the data. In particular, for the duration 
        // //    of this lifetime, the memory the pointer points to must not get accessed (read or written) through any other pointer.
        // 1. correct implementation of Chunk::allocate gurantees that
        // 2. the memory range of the given starting at the pointer is within the single allocated object Chunk::head
        // 3. no, this is not initialized, the id explainers normally will panic for invalid values,
        //    *but* marking this function unsafe is not useful because only auto generated constructors will call this,
        //    they will guarantee that all fields are required in constructor method and initialized the in place object
        // 4. the result lifetime is same as in &self, which correctly dies together with &self,
        //    see Self::get{_mut} and Slice::iter{_mut} for more of following alias rule for borrow allocated object
        unsafe {
            init(real.cast::<T>().as_mut());
        }
        Index{ v: index, p1: PhantomData, p2: PhantomData }
    }

    // item should be index<T> for normal node and direct enum value for enum, see Some Approaches.8
    pub fn build_slice<'a, T: 'a, I: IntoIterator<Item = T>>(&'a self, into_iter: I) -> Slice<'a, T> {

        let mut len = 0;
        // maybe invalid but aligned index, similar to ptr::dangling
        // SAFETY: align will not be 0
        let mut head = unsafe { NonZeroU32::new_unchecked(align_of::<T>() as u32) };

        for item in into_iter {
            let index = self.emplace(|n| { *n = item; });
            if len == 0 {
                head = index.v;
            }
            len += 1;
        }
        Slice{ len, data: head, p1: PhantomData, p2: PhantomData }
    }
}

// --------------------
// read related methods
// --------------------

impl Arena {

    // map index to real address
    fn map_index(&self, index: u32) -> NonNull<u8> {
        let chunks = self.chunks.borrow();
        let (chunk_index, offset) = get_chunk_index_and_offset(index);
        let chunk = chunks.get(chunk_index).expect(&format!("invalid chunk index {chunk_index}"));

        // SAFETY
        // // from docs
        // // 1. Both the starting and resulting pointer must be either in bounds or one
        // //    byte past the end of the same [allocated object].
        // // 2. The computed offset, **in bytes**, cannot overflow an `isize`.
        // // 3. The offset being in bounds cannot rely on "wrapping around" the address
        // //    space. That is, the infinite-precision sum must fit in a `usize`.
        // 1. index & MASK_OFFSET is less than CHUNK_SIZE and make self.head and self.head + offset in side chunk.head object
        // 2. index & MASK_OFFSET is less than CHUNK_SIZE and less than isize for 16/32/64bit platforms
        // 3. chunk.head object does not overflow
        let ptr = unsafe { chunk.head.as_ptr().add(offset) };

        debug_assert!(ptr < chunk.next.as_ptr(), "invalid offset");
        // SAFETY: ptr derives from chunk.head so is non null
        unsafe { NonNull::new_unchecked(ptr) }
    }

    /// Returns a reference to an object that the index holds
    ///
    /// Note that returned reference lifetime is bound to index's lifetime:
    /// 
    /// ```compile_fail
    /// # use fflib::common::arena::Arena;
    /// let arena = Arena::new();
    /// let mut object = None;
    /// {
    ///     let index = arena.emplace(|n: &mut i32| { *n = 1; });
    ///     object = Some(arena.get(&index)); // borrowed value does not live long enough
    /// }
    /// assert_eq!(object.unwrap(), &1);
    /// ```
    pub fn get<'a, 'b: 'a, T>(&'a self, index: &'b Index<'a, T>) -> &'b T {
        let ptr = self.map_index(index.v.get());
        // SAFETY: the safety requirements are really long, but I have written really longer safety explanations, according to them this is safe
        unsafe { ptr.cast::<T>().as_ref() }
    }

    /// Returns a mutable reference to an object that the index holds,
    /// caller need to owned the index or own a mutable reference to the index,
    /// see [`Arena::get`] about lifetime binding
    pub fn get_mut<'a, 'b: 'a, T>(&'a self, index: &'b mut Index<'a, T>) -> &'b mut T {
        let ptr = self.map_index(index.v.get());
        // SAFETY: same as before
        unsafe { ptr.cast::<T>().as_mut() }
    }

    /// Returns a iterable over reference to objects that this slice holds,
    /// see [`Arena::get`] about lifetime binding
    pub fn get_iter<'a, 'b: 'a, T>(&'a self, slice: &'b Slice<'a, T>) -> impl Iterator<Item = &'b T> {
        SliceIter::<T, true>{ arena: self, current: slice.data.get(), remaining: slice.len, phantom: PhantomData }
    }

    /// Returns a iterable over mutable reference to objects that this slice holds,
    /// caller need to owned the slice or own a mutable reference to the slice,
    /// see [`Arena::get`] about lifetime binding
    pub fn get_iter_mut<'a, 'b: 'a, T>(&'a self, slice: &'b mut Slice<'a, T>) -> impl Iterator<Item = &'b mut T> {
        SliceIter::<T, false>{ arena: self, current: slice.data.get(), remaining: slice.len, phantom: PhantomData }
    }
}

// amazingly this works
struct SliceIter<'a, 'b, T, const C: bool> {
    arena: &'a Arena,
    current: u32,   // index of next return value
    remaining: u32, // remaining item count
    phantom: PhantomData<&'b T>,
}

impl<'a, 'b, T> Iterator for SliceIter<'a, 'b, T, true> where 'b: 'a {
    type Item = &'b T;

    fn next(&mut self) -> Option<&'b T> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            // SAFETY: I have written really longer safety explanations, according to them this is safe
            let result = unsafe { self.arena.map_index(self.current).cast::<T>().as_ref() };
            // + sizeof(T) is enough because sizeof is multiple of alignof 
            // so align is always correct and no align padding between items
            self.current += size_of::<T>() as u32;
            Some(result)
        }
    }
}

impl<'a, 'b, T> Iterator for SliceIter<'a, 'b, T, false> where 'b: 'a {
    type Item = &'b mut T;

    fn next(&mut self) -> Option<&'b mut T> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            // SAFETY: I have written really longer safety explanations, according to them this is safe
            let result = unsafe { self.arena.map_index(self.current).cast::<T>().as_mut() };
            // + sizeof(T) is enough because sizeof is multiple of alignof 
            // so align is always correct and no align padding between items
            self.current += size_of::<T>() as u32;
            Some(result)
        }
    }
}

// --------------------------
// test/debug related methods
// --------------------------

impl Arena {

    #[allow(dead_code)] // for debugging
    pub fn status(&self, each_byte: bool) -> ArenaStatus {
        ArenaStatus(self, each_byte)
    }
}

#[allow(dead_code)] // for debugging
pub struct ArenaStatus<'a>(&'a Arena, bool); // bool: each byte

impl<'a> fmt::Display for ArenaStatus<'a> {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // SAFETY: same as before, or is not important for test purpose
        unsafe {
            for (chunk_index, chunk) in self.0.chunks.borrow().iter().enumerate() {
                let used = chunk.next.as_ptr().offset_from(chunk.head.as_ptr()) as usize;
                writeln!(f, "chunk#{} @ 0x{:X} ({}/4096 bytes)", chunk_index, chunk.head.as_ptr() as usize, used)?;
                if self.1 {
                    let slice = std::slice::from_raw_parts(chunk.head.as_ptr(), used);
                    for (chunk_index, chunk) in slice.chunks(16).enumerate() {
                        write!(f, "{:08X} |", chunk_index * 0x10)?;
                        for byte in chunk {
                            write!(f, " {byte:02X}")?;
                        }
                        writeln!(f)?;
                    }
                }
            }
        }
        Ok(())
    }
}
