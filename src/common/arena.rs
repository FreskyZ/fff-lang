// see docs/internal/tree-memory.md

// NOTE this implmementation is completely single threaded,
// I'm not familiar with both multi thread programing and unsafe programming,
// so there may a lot of traditional multi thread issue if the compiler become multi threaded

// this is currently the reason this project needs nightly,
// there is std::alloc::alloc method, but they said they will deprecate when Allocator trait stablized
use std::alloc::{Global, Allocator, Layout};
use std::cell::{Cell, RefCell};
use std::fmt;
use std::marker::PhantomData;
use std::mem::{size_of, align_of};
use std::num::{NonZeroU32, NonZeroUsize};
use std::ptr::NonNull;

// ATTENTION: this is ?Copy even ?Clone,
// because or else you can clone this and claim 2 mutable references from arena
pub struct Index<'a, T> {
    v: NonZeroU32,
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Index<'a, T> {
    // non zero but aligned index
    fn dangling() -> Self {
        // SAFETY: align of will not return 0
        Self{ v: unsafe { NonZeroU32::new_unchecked(align_of::<T>() as u32) }, phantom: PhantomData }
    }
}

// this is also ?Copy and ?Clone
pub struct Slice<'a, T> {
    len: u32, // use head+len instead of begin+end to allow fast .len()
    data: NonZeroU32, // this is index
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Slice<'a, T> {

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

// chunk size is 4k
const CHUNK_SIZE_WIDTH: usize = 12;
const CHUNK_SIZE: usize = 1 << CHUNK_SIZE_WIDTH;
// 0x00000FFF: lower 12bits of virtual address is offset inside chunk
const MASK_OFFSET: u32 = (1 << CHUNK_SIZE_WIDTH) - 1;
// 0xFFFFF000: upper 20bits of virtual address is chunk index
const MASK_CHUNK_INDEX: u32 = ((1 << (32 - CHUNK_SIZE_WIDTH)) - 1) << CHUNK_SIZE_WIDTH;
// SAFETY: constant input
// AMAZING: this is allowed
const CHUNK_LAYOUT: Layout = unsafe {
    Layout::from_size_align_unchecked(CHUNK_SIZE, CHUNK_SIZE)
};

struct Chunk {
    next: NonNull<u8>,
    head: NonNull<u8>,
}

impl Chunk {

    fn new() -> Chunk {
        let head = match Global.allocate(CHUNK_LAYOUT) {
            Ok(head) => {
                // cast: allocate returns NonNull<[u8]>, 
                // that does not allow `offset` like methods which rely on T: Sized
                let head = head.cast();
                // ptr::offset_from seems indicating this should be guaranteed by current implementation,
                // but I did not found it in implementaion or document, so assert here (this is not debug assert)
                // // this will not happen in major linux or windows platforms, because this high virtual address is used by os
                assert!(head.addr().get() < usize::MAX - CHUNK_SIZE, "invalid alloc result");
                head
            },
            // AllocError does not provide any information and actually nothing more can do here
            Err(_) => panic!("oom"),
        };
        Chunk{ head, next: head }
    }

    // return none for not enough space
    fn allocate(&mut self, chunk_index: usize, layout: Layout) -> Option<(NonNull<u8>, NonZeroU32)> {
        let align = if layout.align() < 4 { 4 } else { layout.align() }; // min align 4
        let align_mask = align - 1;
        debug_assert!(layout.size() <= CHUNK_SIZE, "too large object");

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

        if offset + align_padding + layout.size() <= CHUNK_SIZE {
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
                NonZeroU32::new_unchecked(((chunk_index << CHUNK_SIZE_WIDTH) + offset + align_padding) as u32)
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
    //   if a memory manager requires mutable reference to return some reference like (via PhantonData),
    //   it will be regarded as continuously mutable borrows self and cannot do anything more
    chunks: RefCell<Vec<Chunk>>,
    // cannot exist 2 slice builders at same time
    building_slice: Cell<bool>,
}

impl Arena {

    pub fn new() -> Self {

        // include first chunk in new, because you never want an empty arena
        let mut first_chunk = Chunk::new();
        // index 0 is not used, SAFETY: usize add 4 is safe, no overlapping explained
        first_chunk.next = first_chunk.next.map_addr(|a| unsafe { NonZeroUsize::new_unchecked(a.get() + 4) });

        Self{ chunks: RefCell::new(vec![first_chunk]), building_slice: Cell::new(false) }
    }

    fn allocate_impl(&self, layout: Layout) -> (NonNull<u8>, NonZeroU32) {
        debug_assert!(layout.size() <= CHUNK_SIZE, "too large object");

        let mut chunks = self.chunks.borrow_mut();
        let chunk_index = chunks.len() - 1;

        // SAFETY: self.chunks is not empty
        let last_chunk = unsafe { chunks.last_mut().unwrap_unchecked() };
        if let Some(addr) = last_chunk.allocate(chunk_index, layout) {
            return addr;
        }

        // you cannot put this in match { None } or .or_else,
        // because borrow mut of self.chunks may not be dropped and that's runtime error
        chunks.push(Chunk::new());
        // SAFETY: self.chunks is not empty
        let last_chunk = unsafe { chunks.last_mut().unwrap_unchecked() };
        // SAFETY: layout size is less than CHUNK_SIZE and must success for a new chunk
        unsafe { last_chunk.allocate(chunk_index + 1, layout).unwrap_unchecked() }
    }

    // allocate object in place
    pub fn emplace<'a, T, F: FnOnce(&'a mut T)>(&'a self, f: F) -> Index<'a, T> where T: Sized {

        let (real, index) = self.allocate_impl(Layout::new::<T>());
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
            f(real.cast::<T>().as_mut());
        }
        Index{ v: index, phantom: PhantomData }
    }

    // arena slice is very similar to normal slice: items are inlined in the buffer that pointer point to
    // it in theory can be simply emplace items and use begin index and end index to construct a alice with head + len
    // **BUT** that violates reference aliasing rule: you can use mutable item index and mutable slice at the same time
    // to claim 2 mutable reference to an item object, so the slice builder actually simply eat the returned indexes to prevent that.
    // the simply-forward-to-emplace design also allows slice to cross chunk
    #[must_use = "must finish building slice"]
    pub fn slice_builder<T>(&self) -> SliceBuilder<T> {
        assert!(!self.building_slice.get(), "multiple slice builder");
        self.building_slice.set(true);
        SliceBuilder{ arena: self, len: 0, head: Index::<T>::dangling().v, phantom: PhantomData }
    }
}

pub struct SliceBuilder<'a, T> {
    arena: &'a Arena,
    len: u32,
    head: NonZeroU32,
    phantom: PhantomData<T>,
}

impl<'a, T> SliceBuilder<'a, T> {

    pub fn emplace<'b, F: FnOnce(&'b mut T)>(&'b mut self, f: F) where T: Sized {
        let index = self.arena.emplace(f);
        if self.len == 0 {
            self.head = index.v;
        }
        self.len += 1;
    }

    // no, this is not FromIterator but this makes slice builder more like a vector
    pub fn collect(self) -> Slice<'a, T> {
        self.arena.building_slice.set(false);
        Slice{ len: self.len, data: self.head, phantom: PhantomData }
    }
}

impl Drop for Arena {

    fn drop(&mut self) {
        for chunk in self.chunks.borrow().iter() {
            // SAFETY: chunk.head is allocated as CHUNK_LAYOUT and does not change
            unsafe { Global.deallocate(chunk.head, CHUNK_LAYOUT); }
        }
    }
}

// --------------------
// read related methods
// --------------------

impl Arena {

    // map index to real address
    fn map_index(&self, index: u32) -> NonNull<u8> {
        let chunks = self.chunks.borrow();
        let chunk_index = ((index & MASK_CHUNK_INDEX) >> CHUNK_SIZE_WIDTH) as usize;
        let chunk = chunks.get(chunk_index).expect(&format!("invalid chunk index {chunk_index}"));

        let offset = (index & MASK_OFFSET) as usize;
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

    // ATTENTION: return reference bound to lifetime of index, so that it follows normal reference alias rule
    pub fn get<'a, 'b: 'a, T>(&'a self, index: &'b Index<'a, T>) -> &'b T {
        let ptr = self.map_index(index.v.get());
        // SAFETY: the safety requirements are really long, but I have written really longer safety explanations, according to them this is safe
        unsafe { ptr.cast::<T>().as_ref() }
    }

    // ATTENTION: return reference bound to lifetime of index, so that it follows normal reference alias rule
    pub fn get_mut<'a, 'b: 'a, T>(&'a self, index: &'b mut Index<'a, T>) -> &'b mut T {
        let ptr = self.map_index(index.v.get());
        // SAFETY: same as before
        unsafe { ptr.cast::<T>().as_mut() }
    }
}

impl<'a, T> Slice<'a, T> {

    // ATTENTION: return iter bound to lifetime of slice, it seems to be required for reference alias rule
    pub fn iter<'b: 'a>(&'b self, arena: &'a Arena) -> SliceIter<'a, 'b, T> {
        SliceIter{ arena, current: self.data.get(), remaining: self.len, phantom: PhantomData }
    }

    // ATTENTION: return iter bound to lifetime of slice, it seems to be required for reference alias rule
    pub fn iter_mut<'b: 'a>(&'b mut self, arena: &'a Arena) -> SliceIterMut<'a, 'b, T> {
        SliceIterMut{ arena, current: self.data.get(), remaining: self.len, phantom: PhantomData }
    }
}

pub struct SliceIter<'a, 'b, T> {
    arena: &'a Arena,
    current: u32,   // index of next return value
    remaining: u32, // remaining item count
    phantom: PhantomData<&'b T>,
}

impl<'a, 'b, T> Iterator for SliceIter<'a, 'b, T> where 'b: 'a {
    type Item = &'b T;

    fn next(&mut self) -> Option<&'b T> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            // SAFETY: the safety requirements are really long, but I have written really longer safety explanations, according to them this is safe
            let result = unsafe { self.arena.map_index(self.current).cast::<T>().as_ref() };
            // + sizeof(T) is enough because sizeof is multiple of alignof so align is always correct and no align padding between items
            self.current += size_of::<T>() as u32;
            Some(result)
        }
    }
}

pub struct SliceIterMut<'a, 'b, T> {
    arena: &'a Arena,
    current: u32,   // index of next return value
    remaining: u32, // remaining item count
    phantom: PhantomData<&'b T>,
}

impl<'a, 'b, T> Iterator for SliceIterMut<'a, 'b, T> where 'b: 'a {
    type Item = &'b mut T;

    fn next(&mut self) -> Option<&'b mut T> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            // SAFETY: same as before
            let result = unsafe { self.arena.map_index(self.current).cast::<T>().as_mut() };
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

#[cfg(test)]
#[test]
fn basic() {
    struct Node1 { span: u32, isid: u32 }
    struct Node2 { span: u32, isid: u32, span2: u128, keyword: u8, separator: u8 }
    struct Node3<'a> { span: u32, node1: Index<'a, Node1>, node4: Option<Index<'a, Node4<'a>>> }
    struct Node4<'a> { span: u32, node3s: Slice<'a, Node3<'a>> }

    let arena = Arena::new();
    let index1 = arena.emplace(|n: &mut Node1| { n.span = 1; n.isid = 2; });
    let index2 = arena.emplace(|n: &mut Node2| { n.span = 3; n.isid = 4; n.span2 = 5; n.keyword = 6; n.separator = 7 });
    let index3 = arena.emplace(|n: &mut Node1| { n.span = 8; n.isid = 9; });
    let index8 = arena.emplace(|n: &mut Node1| { n.span = 14; n.isid = 15; });

    let mut node3s = arena.slice_builder();
    node3s.emplace(|n: &mut Node3| { n.span = 10; n.node1 = index1; n.node4 = None; });
    node3s.emplace(|n: &mut Node3| { n.span = 11; n.node1 = index3; n.node4 = None; });
    node3s.emplace(|n: &mut Node3| { n.span = 12; n.node1 = index8; n.node4 = None; });
    let node3s = node3s.collect();
    let index7 = arena.emplace(|n: &mut Node4| { n.span = 13; n.node3s = node3s; });
    
    // println!("{}", arena.status(true));

    let node2 = arena.get(&index2);
    assert_eq!((node2.span, node2.isid, node2.span2, node2.keyword, node2.separator), (3, 4, 5, 6, 7));
    let node7 = arena.get(&index7);
    assert_eq!(node7.span, 13);
    assert_eq!(node7.node3s.len(), 3);
    let node7_node3s = node7.node3s.iter(&arena).collect::<Vec<_>>();
    assert_eq!(node7_node3s.len(), 3);
    assert_eq!(node7_node3s[0].span, 10);
    assert!(node7_node3s[0].node4.is_none());
    let node1 = arena.get(&node7_node3s[0].node1);
    assert_eq!((node1.span, node1.isid), (1, 2));
    assert_eq!(node7_node3s[1].span, 11);
    assert!(node7_node3s[1].node4.is_none());
    let node3 = arena.get(&node7_node3s[1].node1);
    assert_eq!((node3.span, node3.isid), (8, 9));
    assert_eq!(node7_node3s[2].span, 12);
    assert!(node7_node3s[2].node4.is_none());
    let node8 = arena.get(&node7_node3s[2].node1);
    assert_eq!((node8.span, node8.isid), (14, 15));
}

/// returned reference lifetime bounds to index, not arena
/// # Example
/// ```compile_fail
/// let arena = Arena::new();
/// let mut object = None;
/// {
///     let index = arena.emplace(|n: &mut i32| { *n = 1; });
///     object = Some(arena.get(&index));
/// }
/// assert_eq!(object.unwrap(), 1);
/// ```
#[allow(dead_code)]
fn get_lifetime() {
}

#[cfg(test)]
#[test]
fn very_large_object() {
    let arena = Arena::new();
    arena.emplace(|_: &mut [u128; 256]| {});
}

#[cfg(test)]
#[test]
#[should_panic(expected = "too large object")]
fn too_large_object() {
    let arena = Arena::new();
    arena.emplace(|_: &mut [u128; 257]| {});
}

#[cfg(test)]
#[test]
#[should_panic(expected = "multiple slice builder")]
fn multiple_slice_builder() {
    let arena = Arena::new();

    let _ = arena.slice_builder::<i32>();
    let _ = arena.slice_builder::<i32>();
}

#[cfg(test)]
#[test]
fn large_array() {
    let arena = Arena::new();

    let mut values = arena.slice_builder();
    for i in 0..2400 {
        values.emplace(|n: &mut i32| { *n = i; });
    }
    let slice = values.collect();

    // println!("{}", arena.status(true));
    
    let vec = slice.iter(&arena).collect::<Vec<_>>();
    for i in 0..2400 {
        assert_eq!(i as i32, *vec[i]);
    }
}
