
use super::*;

#[test]
fn basic() {
    struct Node1 { span: u32, isid: u32 }
    struct Node2 { span: u32, isid: u32, span2: u128, keyword: u8, separator: u8 }
    struct Node3<'a> { span: u32, node1: Index<'a, Node1>, node4: Option<Index<'a, Node4<'a>>> }
    struct Node4<'a> { span: u32, node3s: Slice<'a, Index<'a, Node3<'a>>> }

    let arena = Arena::new();
    let index1 = arena.emplace(|n: &mut Node1| { n.span = 1; n.isid = 2; });
    let index2 = arena.emplace(|n: &mut Node2| { n.span = 3; n.isid = 4; n.span2 = 5; n.keyword = 6; n.separator = 7 });
    let index3 = arena.emplace(|n: &mut Node1| { n.span = 8; n.isid = 9; });
    let index8 = arena.emplace(|n: &mut Node1| { n.span = 14; n.isid = 15; });

    let mut node3s = Vec::new();
    node3s.push(arena.emplace(|n: &mut Node3| { n.span = 10; n.node1 = index1; n.node4 = None; }));
    node3s.push(arena.emplace(|n: &mut Node3| { n.span = 11; n.node1 = index3; n.node4 = None; }));
    node3s.push(arena.emplace(|n: &mut Node3| { n.span = 12; n.node1 = index8; n.node4 = None; }));
    let node3s = arena.build_slice(node3s);
    let index7 = arena.emplace(|n: &mut Node4| { n.span = 13; n.node3s = node3s; });
    
    // println!("{}", arena.status(true));

    let node2 = arena.get(&index2);
    assert_eq!((node2.span, node2.isid, node2.span2, node2.keyword, node2.separator), (3, 4, 5, 6, 7));
    let node7 = arena.get(&index7);
    assert_eq!(node7.span, 13);
    assert_eq!(node7.node3s.len(), 3);
    let node7_node3s = arena.get_iter(&node7.node3s).map(|i| arena.get(i)).collect::<Vec<_>>();
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

#[test]
#[cfg(not(miri))] // 2000 size vector is too large for miri
fn chunk_size_operations() {

    let sizes = (0..2056).map(|i| get_chunk_size(i)).collect::<Vec<_>>();
    assert_eq!(sizes[0], 1 << 12);
    assert_eq!(sizes[1], 1 << 12);
    assert_eq!(sizes[2], 1 << 13);
    assert_eq!(sizes[9], 1 << 20);
    assert_eq!(sizes[10], 1 << 21);
    assert_eq!(sizes[50], 1 << 21);

    // compare to naive method
    let mut start_indexes = Vec::with_capacity(2057);
    start_indexes.push(0);
    for &size in &sizes {
        let new_start_index = *start_indexes.last().unwrap() + size;
        start_indexes.push(new_start_index);
    }
    assert_eq!(start_indexes.len(), 2057);
    assert_eq!(start_indexes[0], 0);
    assert_eq!(start_indexes[1], 1 << 12);
    assert_eq!(start_indexes[2], 1 << 13);
    assert_eq!(start_indexes[9], 1 << 20);
    assert_eq!(start_indexes[10], 1 << 21);
    assert_eq!(start_indexes[11], 2 << 21);
    assert_eq!(start_indexes[12], 3 << 21);
    assert_eq!(start_indexes[13], 4 << 21);
    assert_eq!(start_indexes[14], 5 << 21);
    assert_eq!(start_indexes[15], 6 << 21);
    for i in 10..=2056 {
        assert_eq!(start_indexes[i], (i - 9) << 21);
    }
    for i in 0..=2056 {
        assert_eq!(get_chunk_base_index(i), start_indexes[i]);
    }

    macro_rules! case {
        ($i:expr) => (
            assert_eq!(get_chunk_index_and_offset($i), {
                let mut chunk_index = 0;
                loop {
                    if ($i as usize) < start_indexes[chunk_index + 1] {
                        break (chunk_index, ($i - start_indexes[chunk_index]));
                    }
                    chunk_index += 1;
                }
            });
        );
    }
    case!(0);
    case!(1);
    case!(4000);
    case!(8000);
    case!(10000);
    case!(100000);
    case!(1000000);
    case!(10000000);
    case!(100000000);

    macro_rules! cases {
        ($($i:expr),+) => {{
            $(case!($i);)+
        }}
    }
    // from random import randint
    // from textwrap import wrap
    // print('\n'.join(wrap(', '.join([str(randint(0, 1000000) if randint(0, 100) > 10 else randint(0, 0xFFFFFFFF)) for _ in range(0, 100)]), width=120)))
    cases!{
        454603, 436048, 795409, 2281690808, 633818, 703200, 412184, 3030640151, 351551, 229871, 721038, 3280059628, 42351,
        538350, 500639, 623567, 971871, 630982, 152641, 534634, 203521, 821225, 637018539, 99095, 2684916193, 988599, 183001,
        790087, 631094, 987952, 216783, 603426, 334300, 213410, 71344, 664852, 184458, 905925, 749255, 757427, 542735, 750410,
        983898, 2671555705, 532565, 2888062604, 694565, 765835, 663730, 125219, 313677, 407941, 477639, 400632, 144275,
        1884706155, 676943, 435813, 1107472433, 542688, 676498, 75842, 655258, 714652, 475418, 3397429939, 552209, 80713,
        366352, 538997, 583272, 791465, 917358, 507872, 790258657, 205375, 100045, 452772, 706163, 531050, 584011, 246191,
        356364, 168979, 203555, 86832, 437606, 412513, 259866, 808418, 519853, 604293, 243356, 871119, 290524, 305770, 322255,
        156722, 757885, 176444
    }
}

#[test]
fn very_large_object() {
    let arena = Arena::new();
    arena.emplace(|_: &mut [u128; 256]| {});
}

#[test]
#[should_panic(expected = "too large object")]
fn too_large_object() {
    let arena = Arena::new();
    arena.emplace(|_: &mut [u128; 257]| {});
}

#[test]
#[cfg(not(miri))] // 2000 size vector is too large for miri, too
fn large_array() {
    let arena = Arena::new();

    let mut values = Vec::new();
    for i in 0..2400 {
        values.push(arena.emplace(|n: &mut i32| { *n = i; }));
    }
    let slice = arena.build_slice(values);

    // println!("{}", arena.status(true));
    
    let vec = arena.get_iter(&slice).map(|i| arena.get(i)).collect::<Vec<_>>();
    for i in 0..2400 {
        assert_eq!(i as i32, *vec[i]);
    }
}
