// see docs/internal/tree-memory.md

// this is currently the reason this project needs nightly,
// there is std::alloc::alloc method, but they said they will deprecate when Allocator trait stablized
use std::alloc::{Global, Allocator, Layout};
use std::cell::{Cell, RefCell, Ref, RefMut};
use std::marker::PhantomData;
use std::mem::{size_of, align_of};
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

// chunk size is 4k
const CHUNK_SIZE_WIDTH: usize = 12;
const CHUNK_SIZE: usize = 1 << CHUNK_SIZE_WIDTH;
// 0x00000FFF: lower 12bits of virtual address is offset inside chunk
const MASK_OFFSET: u32 = (1 << (CHUNK_SIZE_WIDTH + 1)) - 1;
// 0xFFFFF000: upper 20bits of virtual address is chunk index
const MASK_CHUNK_INDEX: u32 = ((1 << (33 - CHUNK_SIZE_WIDTH)) - 1) << CHUNK_SIZE_WIDTH;

// SAFETY: see constant definitions above
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
    fn allocate(&mut self, chunk_index: usize, size: usize) -> Option<Address> {
        debug_assert!(size & 0x3 == 0, "invalid size"); // size is expected to align to u32
        debug_assert!(size < CHUNK_SIZE, "too large object");

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
        if offset + size <= CHUNK_SIZE {
            let real = self.next.as_ptr();
            // SAFETY: usize + usize keep non-null-ness, not overflow explained in previous comment
            self.next = self.next.map_addr(|a| unsafe {
                NonZeroUsize::new_unchecked(a.get() + size) 
            });
            Some(Address::new(real, ((chunk_index << CHUNK_SIZE_WIDTH) + offset) as u32))
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
    // another slice_place before previous slice_place finish is error
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

    // when getting arbitrary chunk, caller does not need mutable chunk to get mutable pointer
    fn get_chunk(&self, chunk_index: usize) -> Ref<Chunk> {
        let chunks = self.chunks.borrow();
        debug_assert!(chunk_index < chunks.len(), "invalid chunk index");
        // SAFETY: asserted before
        Ref::map(chunks, |v| unsafe { v.get_unchecked(chunk_index) })
    }

    // when getting last chunk, caller defnitely want to allocate something, so it is mut only
    fn get_last_chunk(&self) -> RefMut<Chunk> {
        let chunks = self.chunks.borrow_mut();
        // SAFETY: self.chunks is not empty
        RefMut::map(chunks, |v| unsafe { v.last_mut().unwrap_unchecked() })
    }

    fn allocate(&self, size: usize) -> Address {
        debug_assert!(size & 0x3 == 0, "invalid size");
        debug_assert!(size < CHUNK_SIZE, "too large object");
        
        let chunk_index = self.chunks.borrow().len() - 1;
        match self.get_last_chunk().allocate(chunk_index, size) {
            Some(addr) => addr,
            None => {
                self.chunks.borrow_mut().push(Chunk::new());
                // SAFETY: new_size is less than CHUNK_SIZE and must allocate success for a new chunk
                unsafe {
                    self.get_last_chunk().allocate(chunk_index + 1, size).unwrap_unchecked()
                }
            },
        }
    }

    // old_size, new_size: size in bytes
    // try append last allocated object from old_size to new_size, copy if not enough space in original chunk, return new address if copy
    fn reallocate(&self, old_addr: &Address, old_size: usize, new_size: usize) -> Option<Address> {
        debug_assert!(new_size & 0x3 == 0, "invalid size");
        debug_assert!(new_size < CHUNK_SIZE, "too large object");

        let old_chunk_index = (old_addr.v.get() & MASK_CHUNK_INDEX) as usize;
        debug_assert!(old_chunk_index == self.chunks.borrow().len() - 1, "invalid chunk index");

        if self.get_last_chunk().allocate(old_chunk_index, new_size - old_size).is_some() {
            return None; // successfully append in place
        }

        // you cannot put these in match allocate result None arm because that keeps cell::RefMut lifetime
        self.chunks.borrow_mut().push(Chunk::new());
        // SAFETY: new_size is less than CHUNK_SIZE and must allocate success for a new chunk
        let new_addr = unsafe {
            self.get_last_chunk().allocate(old_chunk_index + 1, new_size).unwrap_unchecked()
        };

        // SAFETY
        // // from doc
        // // 1. src must be valid for reads of count * size_of::<T>() bytes.
        // // 2. dst must be valid for writes of count * size_of::<T>() bytes.
        // // 3. Both src and dst must be properly aligned.
        // // 4. The region of memory beginning at src with a size of count * size_of::<T>() bytes must not overlap with the region of memory beginning at dst with the same size.
        // 0. the definition for valid is not stabled, so the following explain is not stabled
        // 1. [old_addr, old_addr + old_size) is inside old_chunk and valid to read
        // 2. [new_addr, new_add + new_size) is inside new_chunk and valid to write
        // 3. they are u8 pointers and must be aligned
        // 4. they are in different allocated object and cannot overlapping
        unsafe {
            copy_nonoverlapping(old_addr.r, new_addr.r, old_size)
        }
        Some(new_addr)
    }

    pub fn place<T>(&self) -> Place<T> where T: Sized {
        // objects are expected to u32 index, no less, no more
        debug_assert!(align_of::<T>() == 4, "invalid align");
        Place{ addr: self.allocate(size_of::<T>()), phantom: PhantomData }
    }

    pub fn slice_place<T>(&self) -> SlicePlace<T> {
        debug_assert!(!self.building_slice.get(), "duplicate call of slice_place");
        self.building_slice.set(true);
        // objects are expected to u32 index, no less, no more
        debug_assert!(align_of::<T>() == 4, "invalid align");
        SlicePlace::new(self)
    }

    // map virtual address to real address
    fn map_addr(&self, index: u32) -> *mut u8 {
        let chunk = self.get_chunk((index & MASK_CHUNK_INDEX) as usize);

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
}

pub struct Place<'a, T> {
    addr: Address,
    phantom: PhantomData<&'a mut T>, // place is logically mutable when constructing (the this pointer inside constructor in traditional OOP language)
}

impl<'a, T> Place<'a, T> where T: Sized {

    /// # Safety
    /// The function `f` must write valid values into all fields and not read from them
    pub unsafe fn new_with(self, f: impl FnOnce(&'a mut T)) -> Index<'a, T> {

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
        // 1. align is 4 and size is align at 4 is checked in many place
        // 2. the memory range of the given starting at the pointer is within the single allocated object Chunk::head
        // 3. no, this is not initialized, the id explainers will panic for invalid values
        //    *but*, the correct implementation of scripts/ast.py (and scripts/mast.py?) will guarantee
        //    that all fields is required in the constructor and written to this object without any reading
        // 4. the result lifetime is inferred as same as &Arena, which is correct according to definition
        //
        // SAFETY for unwrap_unchecked
        // real address are derived from Chunk::head which is not null
        f(self.addr.r.cast::<T>().as_mut().unwrap_unchecked());

        Index{ v: self.addr.v, phantom: PhantomData }
    }
}

// place does not change size after allocate, and object type is kind of arbitrary
// slice place allows continuous push and may reallocate, item must be index, T is for index, not item
pub struct SlicePlace<'a, T> {
    m: &'a Arena,
    head: Address,
    len: usize, // in bytes
    cap: usize, // in bytes
    phantom: PhantomData<&'a mut T>, // place is logically mutable when constructing
}

impl<'a, T> SlicePlace<'a, T> {

    fn new(m: &'a Arena) -> Self {
        // allocate one item when creating,
        // or else self.head does not have valid init value
        let head = m.allocate(4);
        Self{ m, head, len: 0, cap: 4, phantom: PhantomData }
    }

    pub fn push(&mut self, item: Index<'a, T>) {

        // grow
        if self.len + 4 > self.cap {
            // use x1.5 multiplier
            let new_cap = if self.cap == 4 { 8 } else { self.cap + (self.cap >> 1) };
            let new_cap = if new_cap > CHUNK_SIZE { CHUNK_SIZE } else { new_cap };
            // TODO: 1000 item is possible array size, may need to split it
            debug_assert!(new_cap > self.cap, "too large slice");
            if let Some(new_head) = self.m.reallocate(&self.head, self.cap, new_cap) {
                self.head = new_head;
            }
            self.cap = new_cap; 
        }

        // SAFETY for add
        // // from docs
        // // 1. Both the starting and resulting pointer must be either in bounds or one
        // //    byte past the end of the same [allocated object].
        // // 2. The computed offset, **in bytes**, cannot overflow an `isize`.
        // // 3. The offset being in bounds cannot rely on "wrapping around" the address
        // //    space. That is, the infinite-precision sum must fit in a `usize`.
        // 1. [self.head, self.head + self.len) is inside the same allocated object Chunk::head
        // 2. self.len is less than or equal to self.cap, self.cap is less than or equal to CHUNK_SIZE
        // 3. Chunk::head is not overlapping
        //
        // SAFETY for write:
        // // from docs
        // // 1. `dst` must be [valid] for writes.
        // // 2. `dst` must be properly aligned. Use [`write_unaligned`] if this is not the case.
        // 1. yes
        // 2. align 4 is checked at many places
        unsafe {
            self.head.r.add(self.len).cast::<u32>().write(item.v.get());
        }
        self.len += 4;
    }

    pub fn finish(self) -> Slice<'a, T> {
        self.m.building_slice.set(false);
        debug_assert!(self.len != 0, "empty slice");

        // SAFETY: checked in debug_assert
        Slice{ head: self.head.v, size: unsafe { NonZeroU32::new_unchecked((self.len >> 2) as u32) }, phantom: PhantomData }
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

impl<'a, T> Copy for Index<'a, T> {}

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

#[cfg(test)]
#[allow(dead_code)] // used when debugging
fn dump(arena: &Arena) {
    unsafe {
        for (chunk_index, chunk) in arena.chunks.borrow().iter().enumerate() {
            let used = chunk.next.as_ptr().offset_from(chunk.head.as_ptr()) as usize;
            println!("chunk#{} 0x{:X} ({} bytes)", chunk_index, chunk.head.as_ptr() as usize, used);
    
            let slice = std::slice::from_raw_parts(chunk.head.as_ptr(), used);
            for (chunk_index, chunk) in slice.chunks(16).enumerate() {
                print!("{:08X} |", chunk_index * 0x10);
                for byte in chunk {
                    print!(" {byte:02X}");
                }
                print!(" |");
                let ptr = chunk.as_ptr();
                let slice = std::slice::from_raw_parts(ptr.cast::<u32>(), 4);
                for id in slice {
                    // the second part is regard them as u32, not ascii
                    // 4 should be enough for test purpose
                    print!(" {id:<4}");
                }
                println!();
            }
        }
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
    let (index1, index2, index3, index4, index5, index6, index7) = unsafe {
        let index1 = arena.place::<Node1>().new_with(|n| { n.span = 1; n.isid = 2; });
        let index2 = arena.place::<Node2>().new_with(|n| { n.span = 3; n.isid = 4; n.span2 = 5; n.keyword = 6; n.separator = 7 });
        let index3 = arena.place::<Node1>().new_with(|n| { n.span = 8; n.isid = 9; });
        let index4 = arena.place::<Node3>().new_with(|n| { n.span = 10; n.node1 = index1; n.node4 = None; });
        let index5 = arena.place::<Node3>().new_with(|n| { n.span = 11; n.node1 = index3; n.node4 = None; });
        let index6 = arena.place::<Node3>().new_with(|n| { n.span = 12; n.node1 = index3; n.node4 = None; });

        let mut vec = arena.slice_place::<Node3>();
        vec.push(index4);
        vec.push(index5);
        vec.push(index6);
        let slice = vec.finish();
        let index7 = arena.place::<Node4>().new_with(|n| { n.span = 13; n.node3s = slice; });
        arena.get_mut(index6).node4 = Some(index7);

        (index1, index2, index3, index4, index5, index6, index7)
    };

    // dump(&arena);

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
