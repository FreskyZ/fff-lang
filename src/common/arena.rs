// see docs/internal/tree-memory.md

// this is currently the reason this project needs nightly,
// there is std::alloc::alloc method, but they said they will deprecate when Allocator trait stablized
use std::alloc::{Global, Allocator, Layout};
use std::cell::RefCell;
use std::fmt;
use std::marker::PhantomData;
use std::num::{NonZeroU32, NonZeroUsize};
use std::ptr::{NonNull, copy_nonoverlapping};

struct Address {
    // real address
    r: *mut u8,
    // virtual address, byte index start from 0, from beginning of
    // first chunk regarding all chunks as one block of continuous memory,
    // the first u32 in first chunk is not used, making this non null
    // // the operating system level virtual address is not
    // // involved here (although that's exactly the pointer above) so I can call this virtual address
    v: NonZeroU32,
}

impl Address {

    fn new(real: *mut u8, r#virtual: u32) -> Self {
        debug_assert!(r#virtual != 0, "invalid virtual address");
        debug_assert!(r#virtual & 0x3 == 0, "invalid align");
        // SAFETY: asserted above
        // // this unsafe should remove some of the boring safety claims following
        Address{ r: real, v: unsafe { NonZeroU32::new_unchecked(r#virtual) } }
    }
}

pub struct Index<'a, T> {
    v: NonZeroU32,
    phantom: PhantomData<&'a T>,
}

// with phantom data, auto derive will not 
// implement if T is not Clone/Copy, and T is actually really not Clone/Copy
impl<'a, T> Clone for Index<'a, T> {

    fn clone(&self) -> Self {
        Self{ v: self.v, phantom: PhantomData }
    }
    fn clone_from(&mut self, source: &Self) {
        self.v = source.v;
    }
}

impl<'a, T> Copy for Index<'a, T> {
}

impl<'a, T> Index<'a, T> {

    pub fn into(self) -> u32 {
        self.v.get()
    }
}

pub struct Slice<'a, T> {
    head: NonZeroU32,
    size: NonZeroU32,
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Slice<'a, T> {

    pub fn len(&self) -> usize {
        self.size.get() as usize
    }

    pub fn iter(&self, arena: &'a Arena) -> SliceIter<'a, T> {
        SliceIter{
            arena,
            current: arena.map_addr(self.head.get()).as_const().cast(),
            remaining: self.size.get() as usize,
            phantom: PhantomData,
        }
    }

    pub fn iter_mut(&self, arena: &'a Arena) -> SliceIterMut<'a, T> {
        SliceIterMut{
            arena,
            current: arena.map_addr(self.head.get()).cast(),
            remaining: self.size.get() as usize,
            phantom: PhantomData,
        }
    }
}

pub struct SliceIter<'a, T> {
    arena: &'a Arena,
    current: *const u32, // index for next return value
    remaining: usize,  // remaining item count
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Iterator for SliceIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            // SAFETY: self.current is valid pointer from arena.map_addr
            let result = unsafe {
                self.arena.map_addr(self.current.read()).cast::<T>().as_ref()
            };
            // SAFETY: after restricted by remaining, 
            //         increasing self.current is still valid pointer from arena.map_addr
            unsafe {
                self.current = self.current.add(1);
            }
            result
        }
    }
}

pub struct SliceIterMut<'a, T> {
    arena: &'a Arena,
    current: *const u32,  // next return value
    remaining: usize, // remaining item count
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Iterator for SliceIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<&'a mut T> {
        if self.remaining == 0 {
            None
        } else {
            self.remaining -= 1;
            // SAFETY: same as before
            let result = unsafe {
                self.arena.map_addr(self.current.read()).cast::<T>().as_mut()
            };
            // SAFETY: same as before
            unsafe {
                self.current = self.current.add(1);
            }
            result
        }
    }
}

// chunk size is 4k
const CHUNK_SIZE_WIDTH: usize = 12;
const CHUNK_SIZE: usize = 1 << CHUNK_SIZE_WIDTH;
// 0x00000FFF: lower 12bits of virtual address is offset inside chunk
const MASK_OFFSET: u32 = (1 << (CHUNK_SIZE_WIDTH + 1)) - 1;
// 0xFFFFF000: upper 20bits of virtual address is chunk index
const MASK_CHUNK_INDEX: u32 = ((1 << (33 - CHUNK_SIZE_WIDTH)) - 1) << CHUNK_SIZE_WIDTH;
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
            // AllocError does not provide any information
            // and I actually have nothing more to do here
            Err(_) => panic!("oom"),
        };
        Chunk{ head, next: head }
    }

    // return none for not enough space
    fn allocate(&mut self, chunk_index: usize, layout: Layout) -> Option<Address> {
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
            // as usize: previous proof prove that self.next is only same or large than self.head
            self.next.as_ptr().offset_from(self.head.as_ptr()) as usize
        };
        let align_padding = if offset & align_mask == 0 { 0 } else { align - (offset & align_mask) };

        if offset + align_padding + layout.size() <= CHUNK_SIZE {
            // SAFETY: this if guarantees that add align padding still keep result inside self.head object
            let real = unsafe { self.next.as_ptr().add(align_padding) };
            // SAFETY: usize + usize keep non-null-ness, not overflow explained in previous comment
            self.next = self.next.map_addr(|a| unsafe {
                NonZeroUsize::new_unchecked(a.get() + align_padding + layout.size())
            });
            Some(Address::new(real, ((chunk_index << CHUNK_SIZE_WIDTH) + offset + align_padding) as u32))
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
}

impl Arena {

    pub fn new() -> Self {

        // include first chunk in new, because you never want an empty arena
        let mut first_chunk = Chunk::new();
        // index 0 is not used, SAFETY: usize add 4 is safe, no overlapping explained
        first_chunk.next = first_chunk.next.map_addr(|a| unsafe { NonZeroUsize::new_unchecked(a.get() + 4) });

        Self{ chunks: RefCell::new(vec![first_chunk]) }
    }

    fn allocate(&self, layout: Layout) -> Address {
        macro_rules! last_chunk { () => (
            // SAFETY: self.chunks is not empty
            self.chunks.borrow_mut().last_mut().unwrap_unchecked()
        )}
        let chunk_index = self.chunks.borrow().len() - 1;
        match unsafe { last_chunk!() }.allocate(chunk_index, layout) {
            Some(addr) => addr,
            None => {
                self.chunks.borrow_mut().push(Chunk::new());
                // SAFETY: new_size is less than CHUNK_SIZE and must allocate success for a new chunk
                unsafe {
                    last_chunk!().allocate(chunk_index + 1, layout).unwrap_unchecked()
                }
            },
        }
    }

    pub fn emplace<'a, T, F: FnOnce(&'a mut T)>(&'a self, f: F) -> Index<'a, T> where T: Sized {
        let addr = self.allocate(Layout::new::<T>());
        // SAFETY for as_mut:
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
        // 3. no, this is not initialized, the id explainers will panic for invalid values
        //    *but*, the correct implementation of scripts/ast.py (and scripts/mast.py?) will guarantee
        //    that all fields is required in the constructor and written to this object without any reading
        // 4. the result lifetime is &self, which is by design, although this allow 
        //    multiple mutable reference of same object or coexist of mutable reference
        //    and immutable reference, there is actually nothing can be done here (for arena in rust)
        // SAFETY for unwrap_unchecked: real address are derived from Chunk::head which is not null
        unsafe {
            f(addr.r.cast::<T>().as_mut().unwrap_unchecked());
        }
        Index{ v: addr.v, phantom: PhantomData }
    }

    pub fn extend<'a, T>(&'a self, vec: Vec<Index<'a, T>>) -> Slice<'a, T> {

        debug_assert!(vec.len() > 0, "empty slice");
        // TODO: 1000 is possible array size, need to think up of an solution
        debug_assert!(vec.len() < CHUNK_SIZE >> 2, "too large array");
        // SAFETY: after that assert, CHUNK_SIZE is well below usize to overflow
        let addr = self.allocate(unsafe { Layout::array::<u32>(vec.len()).unwrap_unchecked() });

        // SAFETY
        // // from doc
        // // 1. src must be valid for reads of count * size_of::<T>() bytes.
        // // 2. dst must be valid for writes of count * size_of::<T>() bytes.
        // // 3. Both src and dst must be properly aligned.
        // // 4. The region of memory beginning at src with a size of count * size_of::<T>() bytes must not overlap with the region of memory beginning at dst with the same size.
        // 0. the definition for valid is not stabled, so the following explain is not stabled
        // 1. vec.as_ptr() within vec.len() is valid for reads
        // 2. correct allocation implementation guarantees dest is valid for write
        // 3. correct vec implementation and allocation implementation guarantee this
        // 4. they must in different allocated object and cannot overlapping
        unsafe {
            copy_nonoverlapping(vec.as_ptr().cast::<u32>(), addr.r.cast::<u32>(), vec.len());
        }

        // SAFETY: checked in debug_assert
        Slice{ head: addr.v, size: unsafe { NonZeroU32::new_unchecked(vec.len() as u32) }, phantom: PhantomData }
    }

    // map virtual address to real address
    fn map_addr(&self, index: u32) -> *mut u8 {
        let chunk_index = (index & MASK_CHUNK_INDEX) as usize;
        let chunks = self.chunks.borrow();

        debug_assert!(chunk_index < chunks.len(), "invalid chunk index");
        // SAFETY: asserted before
        let chunk = unsafe { chunks.get_unchecked(chunk_index) };

        // SAFETY for add
        // // from docs
        // // 1. Both the starting and resulting pointer must be either in bounds or one
        // //    byte past the end of the same [allocated object].
        // // 2. The computed offset, **in bytes**, cannot overflow an `isize`.
        // // 3. The offset being in bounds cannot rely on "wrapping around" the address
        // //    space. That is, the infinite-precision sum must fit in a `usize`.
        // 1. index & MASK_OFFSET is less than CHUNK_SIZE and make self.head and self.head + offset in side chunk.head object
        // 2. index & MASK_OFFSET is less than CHUNK_SIZE and less than isize for 16/32/64bit platforms
        // 3. chunk.head object does not overflap
        let ptr = unsafe {
            chunk.head.as_ptr().add((index & MASK_OFFSET) as usize)
        };
        debug_assert!(ptr < chunk.next.as_ptr(), "invalid offset");
        ptr
    }

    pub fn get<'a, T>(&self, index: Index<'a, T>) -> &T where T: Sized {
        let ptr = self.map_addr(index.v.get());
        // SAFETY: it is safe if index is not arbitray created
        unsafe {
            ptr.cast::<T>().as_ref().unwrap_unchecked()
        }
    }

    // ATTENTION:
    // this immutably borrows self, because when the indexes are alive you simply cannot mutably borrow self,
    // so you can easily holding an immutable object and a mutable object or 2 mutable objects at same time,
    // it is neither compile time nor runtime checked, this function seems needs unsafe because of that, but
    // mark this unsafe does not actually help but only make calling this inconvenient, this may actually need
    // branded type for that: https://plv.mpi-sws.org/rustbelt/ghostcell/paper.pdf
    pub fn get_mut<'a, T>(&self, index: Index<'a, T>) -> &mut T where T: Sized {
        let ptr = self.map_addr(index.v.get());
        // SAFETY: same as before
        unsafe {
            ptr.cast::<T>().as_mut().unwrap_unchecked()
        }
    }

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
        Ok(())
    }
}

#[cfg(test)]
#[test]
fn basic() {
    struct Node1 { span: u32, isid: u32 }
    struct Node2 { span: u32, isid: u32, span2: u32, keyword: u8, separator: u8 }
    struct Node3<'a> { span: u32, node1: Index<'a, Node1>, node4: Option<Index<'a, Node4<'a>>> }
    struct Node4<'a> { span: u32, node3s: Slice<'a, Node3<'a>> }

    let arena = Arena::new();
    let index1 = arena.emplace(|n: &mut Node1| { n.span = 1; n.isid = 2; });
    let index2 = arena.emplace(|n: &mut Node2| { n.span = 3; n.isid = 4; n.span2 = 5; n.keyword = 6; n.separator = 7 });
    let index3 = arena.emplace(|n: &mut Node1| { n.span = 8; n.isid = 9; });
    let index4 = arena.emplace(|n: &mut Node3| { n.span = 10; n.node1 = index1; n.node4 = None; });
    let index5 = arena.emplace(|n: &mut Node3| { n.span = 11; n.node1 = index3; n.node4 = None; });
    let index6 = arena.emplace(|n: &mut Node3| { n.span = 12; n.node1 = index3; n.node4 = None; });

    let slice = arena.extend(vec![index4, index5, index6]);
    let index7 = arena.emplace(|n: &mut Node4| { n.span = 13; n.node3s = slice; });
    arena.get_mut(index6).node4 = Some(index7);

    // println!("{}", arena.status(true));

    let node1 = arena.get(index1);
    assert_eq!((node1.span, node1.isid), (1, 2));
    let node2 = arena.get(index2);
    assert_eq!((node2.span, node2.isid, node2.span2, node2.keyword, node2.separator), (3, 4, 5, 6, 7));
    let node3 = arena.get(index3);
    assert_eq!((node3.span, node3.isid), (8, 9));
    let node4 = arena.get(index4);
    assert_eq!(node4.span, 10);
    assert!(node4.node4.is_none());
    let node4_node1 = arena.get(node4.node1);
    assert_eq!((node4_node1.span, node4_node1.isid), (1, 2));
    let node5 = arena.get(index5);
    assert_eq!(node5.span, 11);
    assert!(node5.node4.is_none());
    let node5_node1 = arena.get(node5.node1);
    assert_eq!((node5_node1.span, node5_node1.isid), (8, 9));
    let node6 = arena.get(index6);
    assert_eq!(node6.span, 12);
    assert!(node6.node4.is_some());
    let node6_node1 = arena.get(node6.node1);
    assert_eq!((node6_node1.span, node6_node1.isid), (8, 9));
    let node6_node4 = arena.get(node6.node4.unwrap());
    assert_eq!(node6_node4.span, 13);
    assert_eq!(node6_node4.node3s.len(), 3);
    let node6_node4_node3s = node6_node4.node3s.iter(&arena).collect::<Vec<_>>();
    assert_eq!(node6_node4_node3s.len(), 3);
    assert_eq!(node6_node4_node3s[0].span, 10);
    assert_eq!(node6_node4_node3s[1].span, 11);
    assert_eq!(node6_node4_node3s[2].span, 12);
    let node7 = arena.get(index7);
    assert_eq!(node7.span, 13);
    assert_eq!(node7.node3s.len(), 3);
    let node7_node3s = node7.node3s.iter(&arena).collect::<Vec<_>>();
    assert_eq!(node7_node3s.len(), 3);
    assert_eq!(node7_node3s[0].span, 10);
    assert_eq!(node7_node3s[1].span, 11);
    assert_eq!(node7_node3s[2].span, 12);
    
    let mut node7_node3s = node7.node3s.iter_mut(&arena).collect::<Vec<_>>();
    assert_eq!(node7_node3s.len(), 3);
    assert_eq!(node7_node3s[0].span, 10);
    assert_eq!(node7_node3s[1].span, 11);
    assert_eq!(node7_node3s[2].span, 12);
    
    node7_node3s[0].span = 14;
    // this is very unsafe
    assert_eq!(node4.span, 14);
}

#[cfg(test)]
#[test]
fn more_align() {

    struct Node1 { span: u32, isid: u32 }
    struct Node2 { span: u32, value: u64 }

    assert_eq!(std::mem::align_of::<Node1>(), 4);
    assert_eq!(std::mem::align_of::<Node2>(), 8);

    let arena = Arena::new();
    let index1 = arena.emplace(|n: &mut Node1| { n.span = 1; n.isid = 2; });
    let index2 = arena.emplace(|n: &mut Node2| { n.span = 3; n.value = u64::MAX; });
    let index3 = arena.emplace(|n: &mut Node1| { n.span = 4; n.isid = 5; });
    let index4 = arena.emplace(|n: &mut Node2| { n.span = 6; n.value = 8; });

    let node1 = arena.get(index1);
    assert_eq!((node1.span, node1.isid), (1, 2));
    let node2 = arena.get(index2);
    assert_eq!((node2.span, node2.value), (3, u64::MAX));
    let node3 = arena.get(index3);
    assert_eq!((node3.span, node3.isid), (4, 5));
    let node4 = arena.get(index4);
    assert_eq!((node4.span, node4.value), (6, 8));
}
