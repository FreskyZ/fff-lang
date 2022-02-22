
// Builtin methods implementation

use std::str::FromStr;
use std::fmt;

use crate::lexical::SeperatorKind;

use crate::codegen::FnName;
use crate::codegen::ItemID;
use crate::codegen::Type;
use crate::codegen::Operand;
use crate::codegen::TypeCollection;

use super::runtime::Runtime;
use super::runtime::RuntimeValue;

pub fn read_int_from_stdin<T>() -> T where T: FromStr, <T as FromStr>::Err: fmt::Debug {
    use std::io;

    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("Failed to read line");
    line.trim().parse().expect("Wanted a number")
}

pub fn dispatch_builtin(signature: (FnName, Vec<ItemID>), parameters: &[Operand], types: &TypeCollection, rt: &mut Runtime) -> RuntimeValue {
    use RuntimeValue::*;

    // +, -, *, /, %, &&, ||
    macro_rules! impl_bin_op { (($v1pat:pat, $v2pat:pat) => $result:expr; $($typeid:pat => $variant:path),+) => (
        // old code says binary operator then 2 arguments and 2 parameters must exist, so directly unwrap to simplify match pattern
        match (signature.1[0].0.unwrap(), signature.1[1].0.unwrap(), rt.index(&parameters[0]), rt.index(&parameters[1])) {
            $(($typeid, $typeid, $variant($v1pat), $variant($v2pat)) => return $variant($result),)+
            _ => {},
        }
    ) }
    // >>, <<
    macro_rules! impl_bin_op_with_i32 { (($v1pat:pat, $v2pat:pat) => $result:expr; $($typeid:pat => $variant:path),+) => (
        match (signature.1[0].0.unwrap(), signature.1[1].0.unwrap(), rt.index(&parameters[0]), rt.index(&parameters[1])) {
            $(($typeid, $typeid, $variant($v1pat), I32($v2pat)) => return $variant($result),)+
            _ => {},
        }
    ) }
    // ~, !
    macro_rules! impl_unary_op { ($v:pat => $result:expr; $($typeid:pat => $variant:path),+) => (
        match (signature.1[0].0.unwrap(), rt.index(&parameters[0])) {
            $(($typeid, $variant($v)) => return $variant($result),)+
            _ => {},
        }
    ) }
    // to_string
    macro_rules! impl_to_string { ($v:pat => $result:expr; $($typeid:pat => $variant:path),+) => (
        match (signature.1[0].0.unwrap(), rt.index(&parameters[0])) {
            $(($typeid, $variant($v)) => return Str($result),)+
            _ => {},
        }
    ) }
    // ++, --
    macro_rules! impl_unary_op_mut { ($v:pat => $result:expr; $($typeid:pat => $variant:path),+) => (
        match (signature.1[0].0.unwrap(), rt.index_mut(&parameters[0])) {
            $(($typeid, $variant($v)) => { $result; return Unit; },)+
            _ => {},
        }
    ) }
    // ==, !=, >=, <=, >, <
    macro_rules! impl_cmp_op { (($v1pat:pat, $v2pat:pat) => $result:expr; $($typeid:pat => $variant:path),+) => (
        match (signature.1[0].0.unwrap(), signature.1[1].0.unwrap(), rt.index(&parameters[0]), rt.index(&parameters[1])) {
            $(($typeid, $typeid, $variant($v1pat), $variant($v2pat)) => return Bool($result),)+
            _ => {},
        }
    ) }
    
    // 8 int to 8 int
    macro_rules! impl_cast { ($result_variant:path, $result_type:ty; $($typeid:pat => $variant:path),+) => (
        match (signature.1[0].0.unwrap(), rt.index(&parameters[0])) {
            $(($typeid, $variant(v)) => return $result_variant(v as $result_type),)+
            _ => {},
        }
    ) }

    match signature.0 {
        FnName::Operator(ref op) => match op {
            SeperatorKind::Add => impl_bin_op!((v1, v2) => v1 + v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::Sub => impl_bin_op!((v1, v2) => v1 - v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::Mul => impl_bin_op!((v1, v2) => v1 * v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::Div => impl_bin_op!((v1, v2) => v1 / v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::Rem => impl_bin_op!((v1, v2) => v1 % v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::ShiftLeft => impl_bin_op_with_i32!((v1, v2) => v1 << v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::ShiftRight => impl_bin_op_with_i32!((v1, v2) => v1 >> v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::Increase => impl_unary_op_mut!(v => *v += 1; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::Decrease => impl_unary_op_mut!(v => *v -= 1; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::BitAnd => impl_bin_op!((v1, v2) => v1 & v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::BitOr => impl_bin_op!((v1, v2) => v1 | v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::BitXor => impl_bin_op!((v1, v2) => v1 ^ v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::Equal => impl_cmp_op!((v1, v2) => v1 == v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64, 11 => Char, 12 => Bool, 13 => Str),
            SeperatorKind::NotEqual => impl_cmp_op!((v1, v2) => v1 != v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64, 11 => Char, 12 => Bool, 13 => Str),
            SeperatorKind::GreatEqual => impl_cmp_op!((v1, v2) => v1 >= v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::LessEqual => impl_cmp_op!((v1, v2) => v1 <= v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::Great => impl_cmp_op!((v1, v2) => v1 > v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::Less => impl_cmp_op!((v1, v2) => v1 < v2; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            SeperatorKind::BitNot => impl_unary_op!(v => !v; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            SeperatorKind::LogicalAnd => impl_bin_op!((v1, v2) => v1 && v2; 12 => Bool),
            SeperatorKind::LogicalOr => impl_bin_op!((v1, v2) => v1 || v2; 12 => Bool),
            SeperatorKind::LogicalNot => impl_unary_op!(v => !v; 12 => Bool),
            _ => {},
        },
        FnName::Cast(target_type) => match target_type {
            1 => impl_cast!(I8, i8; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            2 => impl_cast!(U8, u8; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            3 => impl_cast!(I16, i16; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            4 => impl_cast!(U16, u16; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            5 => impl_cast!(I32, i32; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            6 => impl_cast!(U32, u32; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 11 => Char), // TODO: missing u32 as char
            7 => impl_cast!(I64, i64; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64),
            8 => impl_cast!(U64, u64; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            9 => impl_cast!(R32, f32; 9 => R32, 10 => R64),
            10 => impl_cast!(R64, f64; 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64),
            _ => {},
        },
        FnName::Ident(ref name) if name == "to_string" 
            => impl_to_string!(v => v.to_string(); 1 => I8, 2 => U8, 3 => I16, 4 => U16, 5 => I32, 6 => U32, 7 => I64, 8 => U64, 9 => R32, 10 => R64, 11 => Char), // TODO missing bool to string, string to string
        FnName::Ident(ref name) => match (name.as_str(), signature.1.get(0), signature.1.get(1), parameters.get(0).map(|p| rt.index(p)), parameters.get(1).map(|p| rt.index(p))) {
            ("write", Some(ItemID(Some(13))), None, Some(Str(v)), None) => { print!("{}", v); return Unit; },
            ("writeln", Some(ItemID(Some(13))), None, Some(Str(v)), None) => { println!("{}", v); return Unit; },
            ("read_i32", None, None, None, None) => return I32(read_int_from_stdin::<i32>()),
            ("read_u64", None, None, None, None) => return U64(read_int_from_stdin::<u64>()),
            ("sqrt", Some(ItemID(Some(10))), None, Some(R64(v)), None) => return R64(v.sqrt()),
            ("length", Some(ItemID(Some(13))), None, Some(Str(v)), None) => return U64(v.len() as u64),
            ("get_index", Some(ItemID(Some(13))), Some(ItemID(Some(5))), Some(Str(v1)), Some(I32(v2))) => return Char(v1.chars().nth(v2 as usize).unwrap()),
            ("get_index", Some(ItemID(Some(13))), Some(ItemID(Some(8))), Some(Str(v1)), Some(U64(v2))) => return Char(v1.chars().nth(v2 as usize).unwrap()),
            _ => {},
        },
    }

    if let (FnName::Operator(SeperatorKind::Add), Some(ItemID(Some(13))), Some(ItemID(Some(13))), Some(Str(v1)), Some(Str(v2))) = (&signature.0, signature.1.get(0), signature.1.get(1), parameters.get(0).map(|p| rt.index(p)), parameters.get(1).map(|p| rt.index(p))) {
        return Str(v1 + &v2);
    }

    // Builtin template type global
    if let FnName::Ident(ref fn_name) = signature.0 {
        if fn_name == "?new_tuple" {                        // ?new_tuple(aaa, bbb, ccc) -> (aaa, bbb, ccc)
            let mut rt_values = Vec::new();
            for param in parameters {
                rt_values.push(rt.index(param));
            }
            return RuntimeValue::Tuple(rt_values); 

        } else if fn_name == "?new_array" {                 // ?new_array(value, size) -> [value]
            if let RuntimeValue::I32(item_init_size) = rt.index(&parameters[1]) {
                let item_init_value = rt.index(&parameters[0]);

                let mut array_items = Vec::new();
                for _ in 0..item_init_size {
                    array_items.push(item_init_value.clone());
                }
                return RuntimeValue::ArrayRef(rt.allocate_heap(RuntimeValue::Array(array_items)));
            } else if let RuntimeValue::U64(item_init_size) = rt.index(&parameters[1]) {
                let item_init_value = rt.index(&parameters[0]);

                let mut array_items = Vec::new();
                for _ in 0..item_init_size {
                    array_items.push(item_init_value.clone());
                }
                return RuntimeValue::ArrayRef(rt.allocate_heap(RuntimeValue::Array(array_items)));
            }

        } else if fn_name.starts_with("?new_array_") {  // ?new_array_xxx() -> [xxx]
            return RuntimeValue::ArrayRef(rt.allocate_heap(RuntimeValue::Array(Vec::new())));

        } else if fn_name.starts_with("set_item") {
            let mut temp_id_str = String::new();
            for i in 8..fn_name.len() {
                temp_id_str.push(fn_name.chars().nth(i).unwrap());
            }
            let item_id = usize::from_str(&temp_id_str).unwrap();
            if parameters.len() == 2 {
                let new_value = rt.index(&parameters[1]);
                match rt.index_mut(&parameters[0]) {
                    &mut RuntimeValue::Tuple(ref mut items) => { items[item_id] = new_value; return RuntimeValue::Unit; }
                    _ => unreachable!()
                }
            } else {
                unreachable!()
            }
        }
    }

    // Builtin template type instance
    if let Some(ItemID(Some(real_typeid))) = signature.1.get(0) {
        match types.get_by_idx(*real_typeid) {
            &Type::Base(_) => unreachable!("fnname {:?} realtypeid {}", signature.0, real_typeid),
            &Type::Array(_item_real_typeid) => {
                match (&signature.0, signature.1.get(0), signature.1.get(1), signature.1.get(2), parameters.get(0), parameters.get(1), parameters.get(2)) {
                    (&FnName::Operator(SeperatorKind::Equal), _, _, _, Some(ref param0), Some(ref param1), None) => {
                        match (rt.index(param0), rt.index(param1)) {
                            (RuntimeValue::ArrayRef(heap_index1), RuntimeValue::ArrayRef(heap_index2)) => {
                                let (array1_clone, array2_clone) = match (&rt.heap[heap_index1], &rt.heap[heap_index2]) {
                                    (&RuntimeValue::Array(ref array1), &RuntimeValue::Array(ref array2)) => (array1.clone(), array2.clone()),
                                    _ => unreachable!(),
                                };

                                if array1_clone.len() != array2_clone.len() {
                                    return RuntimeValue::Bool(false);
                                } else {
                                    let mut ret_val = true;
                                    for i in 0..array1_clone.len() {
                                        ret_val = ret_val && (array1_clone[i] == array2_clone[i]); // because cannot decide part of heap's operand, that's all
                                    }
                                    return RuntimeValue::Bool(ret_val);
                                }
                            }
                            _ => unreachable!()
                        }
                    }
                    (&FnName::Operator(SeperatorKind::NotEqual), _, _, _, Some(ref param0), Some(ref param1), None) => {
                        match (rt.index(param0), rt.index(param1)) {
                            (RuntimeValue::ArrayRef(heap_index1), RuntimeValue::ArrayRef(heap_index2)) => {
                                let (array1_clone, array2_clone) = match (&rt.heap[heap_index1], &rt.heap[heap_index2]) {
                                    (&RuntimeValue::Array(ref array1), &RuntimeValue::Array(ref array2)) => (array1.clone(), array2.clone()),
                                    _ => unreachable!(),
                                };

                                if array1_clone.len() != array2_clone.len() {
                                    return RuntimeValue::Bool(true);
                                } else {
                                    let mut ret_val = false;
                                    for i in 0..array1_clone.len() {
                                        ret_val = ret_val || (array1_clone[i] != array2_clone[i]);
                                    }
                                    return RuntimeValue::Bool(ret_val);
                                }
                            }
                            _ => unreachable!()
                        }
                    }

                    (&FnName::Ident(ref ident_name), param_type0, param_type1, param_type2, param0, param1, param2) => match (ident_name.as_ref(), param_type0, param_type1, param_type2, param0, param1, param2) {
                        ("set_index", _, Some(ItemID(Some(5))), _, Some(ref param0), Some(ref param1), Some(ref param2)) => {
                            match (rt.index(param0), rt.index(param1), rt.index(param2)) {
                                (RuntimeValue::ArrayRef(heap_index), RuntimeValue::I32(array_index), new_value) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => { array[array_index as usize] = new_value.clone(); return RuntimeValue::Unit; }
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("set_index", _, Some(ItemID(Some(8))), _, Some(ref param0), Some(ref param1), Some(ref param2)) => {
                            match (rt.index(param0), rt.index(param1), rt.index(param2)) {
                                (RuntimeValue::ArrayRef(heap_index), RuntimeValue::U64(array_index), new_value) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => { array[array_index as usize] = new_value.clone(); return RuntimeValue::Unit; }
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("get_index", _, Some(ItemID(Some(8))), None, Some(ref param0), Some(ref param1), None) => {
                            match (rt.index(param0), rt.index(param1)) {
                                (RuntimeValue::ArrayRef(heap_index), RuntimeValue::U64(array_index)) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => return array[array_index as usize].clone(),
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("get_index", _, Some(ItemID(Some(5))), None, Some(ref param0), Some(ref param1), None) => {
                            match (rt.index(param0), rt.index(param1)) {
                                (RuntimeValue::ArrayRef(heap_index), RuntimeValue::I32(array_index)) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => return array[array_index as usize].clone(),
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("push", _, _, None, Some(ref param0), Some(ref param1), None) => {
                            match (rt.index(param0), rt.index(param1)) {
                                (RuntimeValue::ArrayRef(heap_index), new_value) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => { array.push(new_value.clone()); return RuntimeValue::Unit; }
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("pop", _, None, None, Some(ref param0), None, None) => {
                            match rt.index(param0) {
                                RuntimeValue::ArrayRef(heap_index) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => return array.pop().unwrap(),
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("length", _, None, None, Some(ref param0), None, None) => {
                            match rt.index(param0) {
                                RuntimeValue::ArrayRef(heap_index) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => return RuntimeValue::U64(array.len() as u64),
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            &Type::Tuple(ref _item_real_typeids) => {
                match (&signature.0, parameters.get(0), parameters.get(1)) {
                    (&FnName::Operator(SeperatorKind::Equal), Some(ref param0), Some(ref param1)) => {
                        match (rt.index(param0), rt.index(param1)) {
                            (RuntimeValue::Tuple(values1), RuntimeValue::Tuple(values2)) => {
                                let mut ret_val = true;
                                for i in 0..values1.len() {
                                    ret_val = ret_val && values1[i] == values2[i];
                                }
                                return RuntimeValue::Bool(ret_val);
                            }
                            _ => unreachable!()
                        }
                    }
                    (&FnName::Operator(SeperatorKind::NotEqual), Some(ref param0), Some(ref param1)) => {
                        match (rt.index(param0), rt.index(param1)) {
                            (RuntimeValue::Tuple(values1), RuntimeValue::Tuple(values2)) => {
                                let mut ret_val = false;
                                for i in 0..values1.len() {
                                    ret_val = ret_val || values1[i] != values2[i];
                                }
                                return RuntimeValue::Bool(ret_val);
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                }
            }
        }
    }

    unreachable!()
}