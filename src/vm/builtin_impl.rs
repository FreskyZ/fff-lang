
// Builtin methods implementation

use std::str::FromStr;
use std::fmt;

use lexical::SeperatorKind;

use codegen::FnName;
use codegen::ItemID;
use codegen::Type;
use codegen::TypeCollection;

use super::runtime::Runtime;
use super::runtime::RuntimeValue;

// Based on the assumption that runtime will not meet invalid, return ItemID::Invalid for index out of bound
fn prep_param_type(typeids: &Vec<ItemID>, index: usize) -> Option<usize> {
    if index >= typeids.len() {
        None
    } else {
        Some(typeids[index].as_option().unwrap()) // assume unwrap able
    }
}
fn prep_param(params: &Vec<RuntimeValue>, index: usize) -> Option<&RuntimeValue> {
    if index >= params.len() {
        None
    } else {
        Some(&params[index])
    }
}
fn prep_param_mut(params: &mut Vec<RuntimeValue>, index: usize) -> Option<&mut RuntimeValue> {
    if index >= params.len() {
        None
    } else {
        Some(&mut params[index])
    }
}


fn read_int_from_stdin<T>() -> T where T: FromStr, <T as FromStr>::Err: fmt::Debug {
    use std::io;

    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("Failed to read line");
    line.trim().parse().expect("Wanted a number")
}

fn str_begin_with(source: &str, pattern: &str) -> bool {

    let mut source_chars = source.chars();
    let mut pattern_chars = pattern.chars();

    loop {
        match (source_chars.next(), pattern_chars.next()) {
            (Some(ch1), Some(ch2)) => if ch1 != ch2 { return false; }, // else continue
            (_, _) => return true,
        }
    }
}

pub fn dispatch_builtin(fn_sign: (FnName, Vec<ItemID>), mut params: Vec<RuntimeValue>, types: &TypeCollection, rt: &mut Runtime) -> RuntimeValue {
    
    let fn_name = fn_sign.0;
    let param_types = fn_sign.1;

    match (&fn_name, prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param(&params, 0), prep_param(&params, 1)) {
        (&FnName::Ident(ref ident_name), param_type0, param_type1, param0, param1) => match (ident_name.as_ref(), param_type0, param_type1, param0, param1) {
            ("write", Some(13), None, Some(&RuntimeValue::Str(ref value)), None) => { print!("{}", value); return RuntimeValue::Unit; }
            ("writeln", Some(13), None, Some(&RuntimeValue::Str(ref value)), None) => { println!("{}", value); return RuntimeValue::Unit; }
            ("read_i32", None, None, None, None) => return RuntimeValue::Int(read_int_from_stdin::<i32>() as u64),
            ("read_u64", None, None, None, None) => return RuntimeValue::Int(read_int_from_stdin::<u64>()), 
            _ => (),
        },
        _ => (),
    }

    // Builtin type
    match (&fn_name, prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param(&params, 0), prep_param(&params, 1)) {

        // Integral Add
        (&FnName::Operator(SeperatorKind::Add), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Add), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 + *value2),
                _ => unreachable!(),
            }
        }
        // Integral Sub
        (&FnName::Operator(SeperatorKind::Sub), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Sub), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 - *value2),
                _ => unreachable!(),
            }
        }
        // Integral Mul
        (&FnName::Operator(SeperatorKind::Mul), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Mul), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 * *value2),
                _ => unreachable!(),
            }
        }
        // Integral Div
        (&FnName::Operator(SeperatorKind::Div), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Div), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 / *value2),
                _ => unreachable!(),
            }
        }
        // Integral Rem
        (&FnName::Operator(SeperatorKind::Rem), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Rem), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Rem), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Rem), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Rem), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Rem), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Rem), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Rem), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 % *value2),
                _ => unreachable!(),
            }
        }
        // Integral ShiftLeft
        (&FnName::Operator(SeperatorKind::ShiftLeft), Some(1), Some(5), param0, param1) 
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(2), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(3), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(4), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(6), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(7), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(8), Some(5), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 << *value2),
                _ => unreachable!(),
            }
        }
        // Integral ShiftRight
        (&FnName::Operator(SeperatorKind::ShiftRight), Some(1), Some(5), param0, param1) 
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(2), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(3), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(4), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(6), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(7), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(8), Some(5), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 >> *value2),
                _ => unreachable!(),
            }
        }
        // Integral BitAnd
        (&FnName::Operator(SeperatorKind::BitAnd), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 & *value2),
                _ => unreachable!(),
            }
        }
        // Integral BitOr
        (&FnName::Operator(SeperatorKind::BitOr), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::BitOr), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitOr), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitOr), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitOr), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitOr), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitOr), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitOr), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 | *value2),
                _ => unreachable!(),
            }
        }
        // Integral BitXor
        (&FnName::Operator(SeperatorKind::BitXor), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::BitXor), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitXor), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitXor), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitXor), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitXor), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitXor), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::BitXor), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Int(*value1 ^ *value2),
                _ => unreachable!(),
            }
        }

        // Integral Equal
        (&FnName::Operator(SeperatorKind::Equal), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Equal), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Bool(*value1 == *value2),
                _ => unreachable!(),
            }
        }
        // Integral NotEqual
        (&FnName::Operator(SeperatorKind::NotEqual), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Bool(*value1 != *value2),
                _ => unreachable!(),
            }
        }
        // Integral GreatEqual
        (&FnName::Operator(SeperatorKind::GreatEqual), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Bool(*value1 >= *value2),
                _ => unreachable!(),
            }
        }
        // Integral LessEqual
        (&FnName::Operator(SeperatorKind::LessEqual), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Bool(*value1 <= *value2),
                _ => unreachable!(),
            }
        }
        // Integral Great
        (&FnName::Operator(SeperatorKind::Great), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Great), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Bool(*value1 > *value2),
                _ => unreachable!(),
            }
        }
        // Integral Less
        (&FnName::Operator(SeperatorKind::Less), Some(1), Some(1), param0, param1) 
        | (&FnName::Operator(SeperatorKind::Less), Some(2), Some(2), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(3), Some(3), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(4), Some(4), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(5), Some(5), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(6), Some(6), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(7), Some(7), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(8), Some(8), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Int(ref value1)), Some(&RuntimeValue::Int(ref value2))) => return RuntimeValue::Bool(*value1 < *value2),
                _ => unreachable!(),
            }
        }

        // Integral BitNot
        (&FnName::Operator(SeperatorKind::BitNot), Some(1), None, param0, _param1) 
        | (&FnName::Operator(SeperatorKind::BitNot), Some(2), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(3), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(4), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(5), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(6), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(7), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(8), None, param0, _param1) => {
            match param0 {
                Some(&RuntimeValue::Int(ref value1)) => return RuntimeValue::Int(u64::max_value() - *value1),
                _ => unreachable!(),
            }
        }
        // Integral Increase
        (&FnName::Operator(SeperatorKind::Increase), Some(1), None, param0, _param1) 
        | (&FnName::Operator(SeperatorKind::Increase), Some(2), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Increase), Some(3), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Increase), Some(4), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Increase), Some(5), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Increase), Some(6), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Increase), Some(7), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Increase), Some(8), None, param0, _param1) => {
            match param0 {
                Some(&RuntimeValue::Int(ref value1)) => return RuntimeValue::Int(*value1 + 1),  // assignment not here
                _ => unreachable!(),
            }
        }
        // Integral Decrease
        (&FnName::Operator(SeperatorKind::Decrease), Some(1), None, param0, _param1) 
        | (&FnName::Operator(SeperatorKind::Decrease), Some(2), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(3), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(4), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(5), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(6), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(7), None, param0, _param1)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(8), None, param0, _param1) => {
            match param0 {
                Some(&RuntimeValue::Int(ref value1)) => return RuntimeValue::Int(*value1 - 1), // assignment not here
                _ => unreachable!(),
            }
        }

        // Integral Cast to integral
        (&FnName::Cast(1), Some(1), None, param0, _param1) 
        | (&FnName::Cast(2), Some(1), None, param0, _param1) 
        | (&FnName::Cast(3), Some(1), None, param0, _param1) 
        | (&FnName::Cast(4), Some(1), None, param0, _param1) 
        | (&FnName::Cast(5), Some(1), None, param0, _param1) 
        | (&FnName::Cast(6), Some(1), None, param0, _param1) 
        | (&FnName::Cast(7), Some(1), None, param0, _param1) 
        | (&FnName::Cast(8), Some(1), None, param0, _param1) 
        | (&FnName::Cast(1), Some(2), None, param0, _param1) 
        | (&FnName::Cast(2), Some(2), None, param0, _param1) 
        | (&FnName::Cast(3), Some(2), None, param0, _param1) 
        | (&FnName::Cast(4), Some(2), None, param0, _param1) 
        | (&FnName::Cast(5), Some(2), None, param0, _param1) 
        | (&FnName::Cast(6), Some(2), None, param0, _param1) 
        | (&FnName::Cast(7), Some(2), None, param0, _param1) 
        | (&FnName::Cast(8), Some(2), None, param0, _param1) 
        | (&FnName::Cast(1), Some(3), None, param0, _param1) 
        | (&FnName::Cast(2), Some(3), None, param0, _param1) 
        | (&FnName::Cast(3), Some(3), None, param0, _param1) 
        | (&FnName::Cast(4), Some(3), None, param0, _param1) 
        | (&FnName::Cast(5), Some(3), None, param0, _param1) 
        | (&FnName::Cast(6), Some(3), None, param0, _param1) 
        | (&FnName::Cast(7), Some(3), None, param0, _param1) 
        | (&FnName::Cast(8), Some(3), None, param0, _param1) 
        | (&FnName::Cast(1), Some(4), None, param0, _param1) 
        | (&FnName::Cast(2), Some(4), None, param0, _param1) 
        | (&FnName::Cast(3), Some(4), None, param0, _param1) 
        | (&FnName::Cast(4), Some(4), None, param0, _param1) 
        | (&FnName::Cast(5), Some(4), None, param0, _param1) 
        | (&FnName::Cast(6), Some(4), None, param0, _param1) 
        | (&FnName::Cast(7), Some(4), None, param0, _param1) 
        | (&FnName::Cast(8), Some(4), None, param0, _param1) 
        | (&FnName::Cast(1), Some(5), None, param0, _param1) 
        | (&FnName::Cast(2), Some(5), None, param0, _param1) 
        | (&FnName::Cast(3), Some(5), None, param0, _param1) 
        | (&FnName::Cast(4), Some(5), None, param0, _param1) 
        | (&FnName::Cast(5), Some(5), None, param0, _param1) 
        | (&FnName::Cast(6), Some(5), None, param0, _param1) 
        | (&FnName::Cast(7), Some(5), None, param0, _param1) 
        | (&FnName::Cast(8), Some(5), None, param0, _param1) 
        | (&FnName::Cast(1), Some(6), None, param0, _param1) 
        | (&FnName::Cast(2), Some(6), None, param0, _param1) 
        | (&FnName::Cast(3), Some(6), None, param0, _param1) 
        | (&FnName::Cast(4), Some(6), None, param0, _param1) 
        | (&FnName::Cast(5), Some(6), None, param0, _param1) 
        | (&FnName::Cast(6), Some(6), None, param0, _param1) 
        | (&FnName::Cast(7), Some(6), None, param0, _param1) 
        | (&FnName::Cast(8), Some(6), None, param0, _param1) 
        | (&FnName::Cast(1), Some(7), None, param0, _param1) 
        | (&FnName::Cast(2), Some(7), None, param0, _param1) 
        | (&FnName::Cast(3), Some(7), None, param0, _param1) 
        | (&FnName::Cast(4), Some(7), None, param0, _param1) 
        | (&FnName::Cast(5), Some(7), None, param0, _param1) 
        | (&FnName::Cast(6), Some(7), None, param0, _param1) 
        | (&FnName::Cast(7), Some(7), None, param0, _param1) 
        | (&FnName::Cast(8), Some(7), None, param0, _param1) 
        | (&FnName::Cast(1), Some(8), None, param0, _param1) 
        | (&FnName::Cast(2), Some(8), None, param0, _param1) 
        | (&FnName::Cast(3), Some(8), None, param0, _param1) 
        | (&FnName::Cast(4), Some(8), None, param0, _param1) 
        | (&FnName::Cast(5), Some(8), None, param0, _param1) 
        | (&FnName::Cast(6), Some(8), None, param0, _param1) 
        | (&FnName::Cast(7), Some(8), None, param0, _param1) 
        | (&FnName::Cast(8), Some(8), None, param0, _param1) => {
            match param0 {
                Some(&RuntimeValue::Int(ref value0)) => return RuntimeValue::Int(*value0), // nothing happened, currently
                _ => unreachable!()
            }
        }

        // Integral cast to float
        (&FnName::Cast(10), Some(1), None, param0, _param1) 
        | (&FnName::Cast(10), Some(2), None, param0, _param1) 
        | (&FnName::Cast(10), Some(3), None, param0, _param1) 
        | (&FnName::Cast(10), Some(4), None, param0, _param1) 
        | (&FnName::Cast(10), Some(5), None, param0, _param1) 
        | (&FnName::Cast(10), Some(6), None, param0, _param1) 
        | (&FnName::Cast(10), Some(7), None, param0, _param1) 
        | (&FnName::Cast(10), Some(8), None, param0, _param1) => {
            match param0 {
                Some(&RuntimeValue::Int(ref value0)) => return RuntimeValue::Float(*value0 as f64), // nothing happened, currently
                _ => unreachable!()
            }
        }

        // Float Add
        (&FnName::Operator(SeperatorKind::Add), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Add), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Float(*value1 + *value2),
                _ => unreachable!()
            }
        }
        // Float Sub
        (&FnName::Operator(SeperatorKind::Sub), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Sub), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Float(*value1 - *value2),
                _ => unreachable!()
            }
        }
        // Float Mul
        (&FnName::Operator(SeperatorKind::Mul), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Mul), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Float(*value1 * *value2),
                _ => unreachable!()
            }
        }
        // Float Div
        (&FnName::Operator(SeperatorKind::Div), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Div), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Float(*value1 / *value2),
                _ => unreachable!()
            }
        }

        // Float Equal
        (&FnName::Operator(SeperatorKind::Equal), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Equal), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Bool(*value1 == *value2),
                _ => unreachable!(),
            }
        }
        // Float NotEqual
        (&FnName::Operator(SeperatorKind::NotEqual), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Bool(*value1 != *value2),
                _ => unreachable!(),
            }
        }
        // Float GreatEqual
        (&FnName::Operator(SeperatorKind::GreatEqual), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Bool(*value1 >= *value2),
                _ => unreachable!(),
            }
        }
        // Float LessEqual
        (&FnName::Operator(SeperatorKind::LessEqual), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Bool(*value1 <= *value2),
                _ => unreachable!(),
            }
        }
        // Float Great
        (&FnName::Operator(SeperatorKind::Great), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Great), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Bool(*value1 > *value2),
                _ => unreachable!(),
            }
        }
        // Float Less
        (&FnName::Operator(SeperatorKind::Less), Some(9), Some(9), param0, param1)
        | (&FnName::Operator(SeperatorKind::Less), Some(10), Some(10), param0, param1) => {
            match (param0, param1) {
                (Some(&RuntimeValue::Float(ref value1)), Some(&RuntimeValue::Float(ref value2))) => return RuntimeValue::Bool(*value1 < *value2),
                _ => unreachable!(),
            }
        }

        // Float cast to float
        (&FnName::Cast(9), Some(9), None, param0, _param1) 
        | (&FnName::Cast(10), Some(9), None, param0, _param1) 
        | (&FnName::Cast(9), Some(10), None, param0, _param1) 
        | (&FnName::Cast(10), Some(10), None, param0, _param1)  => {
            match param0 {
                Some(&RuntimeValue::Float(ref value0)) => return RuntimeValue::Float(*value0), // nothing happened, currently
                _ => unreachable!()
            }
        }

        // Float cast to integral
        (&FnName::Cast(8), Some(9), None, param0, _param1) 
        | (&FnName::Cast(8), Some(10), None, param0, _param1) => {
            match param0 {
                Some(&RuntimeValue::Float(ref value0)) => return RuntimeValue::Int(*value0 as u64), // nothing happened, currently
                _ => unreachable!()
            }
        }

        // Char Compare
        (&FnName::Operator(SeperatorKind::Equal), Some(11), Some(11), Some(&RuntimeValue::Char(ref value0)), Some(&RuntimeValue::Char(ref value1))) => return RuntimeValue::Bool(*value0 == *value1),
        (&FnName::Operator(SeperatorKind::NotEqual), Some(11), Some(11), Some(&RuntimeValue::Char(ref value0)), Some(&RuntimeValue::Char(ref value1))) => return RuntimeValue::Bool(*value0 != *value1),
        (&FnName::Operator(SeperatorKind::GreatEqual), Some(11), Some(11), Some(&RuntimeValue::Char(ref value0)), Some(&RuntimeValue::Char(ref value1))) => return RuntimeValue::Bool(*value0 >= *value1),
        (&FnName::Operator(SeperatorKind::LessEqual), Some(11), Some(11), Some(&RuntimeValue::Char(ref value0)), Some(&RuntimeValue::Char(ref value1))) => return RuntimeValue::Bool(*value0 <= *value1),
        (&FnName::Operator(SeperatorKind::Great), Some(11), Some(11), Some(&RuntimeValue::Char(ref value0)), Some(&RuntimeValue::Char(ref value1))) => return RuntimeValue::Bool(*value0 > *value1),
        (&FnName::Operator(SeperatorKind::Less), Some(11), Some(11), Some(&RuntimeValue::Char(ref value0)), Some(&RuntimeValue::Char(ref value1))) => return RuntimeValue::Bool(*value0 < *value1),

        // Char cast
        (&FnName::Cast(6), Some(11), None, Some(&RuntimeValue::Char(ref value)), None) => return RuntimeValue::Int(*value as u64),

        // Bool
        (&FnName::Operator(SeperatorKind::Equal), Some(12), Some(12), Some(&RuntimeValue::Bool(ref value0)), Some(&RuntimeValue::Bool(ref value1))) => return RuntimeValue::Bool(*value0 == *value1),
        (&FnName::Operator(SeperatorKind::NotEqual), Some(12), Some(12), Some(&RuntimeValue::Bool(ref value0)), Some(&RuntimeValue::Bool(ref value1))) => return RuntimeValue::Bool(*value0 != *value1),
        (&FnName::Operator(SeperatorKind::LogicalAnd), Some(12), Some(12), Some(&RuntimeValue::Bool(ref value0)), Some(&RuntimeValue::Bool(ref value1))) => return RuntimeValue::Bool(*value0 && *value1),
        (&FnName::Operator(SeperatorKind::LogicalOr), Some(12), Some(12), Some(&RuntimeValue::Bool(ref value0)), Some(&RuntimeValue::Bool(ref value1))) => return RuntimeValue::Bool(*value0 || *value1),
        (&FnName::Operator(SeperatorKind::LogicalNot), Some(12), None, Some(&RuntimeValue::Bool(ref value)), None) => return RuntimeValue::Bool(!*value),

        // Str
        (&FnName::Operator(SeperatorKind::Equal), Some(13), Some(13), Some(&RuntimeValue::Str(ref value0)), Some(&RuntimeValue::Str(ref value1))) => return RuntimeValue::Str(value0.clone() + &*value1),

        // Special ident dispatcher
        (&FnName::Ident(ref ident_name), param_type0, param_type1, param0, param1) => match (ident_name.as_ref(), param_type0, param_type1, param0, param1) {

            // Integral is_odd
            ("is_odd", Some(1), None, param0, _param1) 
            | ("is_odd", Some(2), None, param0, _param1) 
            | ("is_odd", Some(3), None, param0, _param1) 
            | ("is_odd", Some(4), None, param0, _param1) 
            | ("is_odd", Some(5), None, param0, _param1) 
            | ("is_odd", Some(6), None, param0, _param1) 
            | ("is_odd", Some(7), None, param0, _param1) 
            | ("is_odd", Some(8), None, param0, _param1) => {
                match param0 {
                    Some(&RuntimeValue::Int(ref value0)) => return RuntimeValue::Bool(*value0 & 1 == 0), // nothing happened, currently
                    _ => unreachable!()
                }
            }
            
            // Integral to_string
            ("to_string", Some(1), None, param0, _param1) 
            | ("to_string", Some(2), None, param0, _param1) 
            | ("to_string", Some(3), None, param0, _param1) 
            | ("to_string", Some(4), None, param0, _param1) 
            | ("to_string", Some(5), None, param0, _param1) 
            | ("to_string", Some(6), None, param0, _param1) 
            | ("to_string", Some(7), None, param0, _param1) 
            | ("to_string", Some(8), None, param0, _param1) => {
                match param0 {
                    Some(&RuntimeValue::Int(ref value0)) => return RuntimeValue::Str(format!("{}", value0)), // nothing happened, currently
                    _ => unreachable!()
                }
            }

            // Float to_string
            ("to_string", Some(9), None, param0, _param1) 
            | ("to_string", Some(10), None, param0, _param1) => {
                match param0 {
                    Some(&RuntimeValue::Float(ref value0)) => return RuntimeValue::Str(format!("{}", value0)), // nothing happened, currently
                    _ => unreachable!()
                }
            }

            // Char to_string
            ("to_string", Some(11), None, Some(&RuntimeValue::Char(ref value)), None) => return RuntimeValue::Str(format!("{}", value)),

            // String
            ("length", Some(13), None, Some(&RuntimeValue::Str(ref value)), None) => return RuntimeValue::Int(value.len() as u64),
            ("get_index", Some(13), Some(5), Some(&RuntimeValue::Str(ref value0)), Some(&RuntimeValue::Int(ref value1))) => return RuntimeValue::Char(value0.chars().nth(*value1 as usize).unwrap()),
            ("get_index", Some(13), Some(8), Some(&RuntimeValue::Str(ref value0)), Some(&RuntimeValue::Int(ref value1))) => return RuntimeValue::Char(value0.chars().nth(*value1 as usize).unwrap()),

            (_, _, _, _, _) => (),
        },

        (_, _, _, _, _) => (),
    }

    // Builtin template type global
    if let &FnName::Ident(ref fn_name) = &fn_name {
        if fn_name == "?new_tuple" {                        // ?new_tuple(aaa, bbb, ccc) -> (aaa, bbb, ccc)
            return RuntimeValue::Tuple(params);             // if no need to check type, this is all
        } else if fn_name == "?new_array" {                 // ?new_array(value, size) -> [value]
            // Currently 5 and 8 are all 8 at runtime, no more need to check
            if let RuntimeValue::Int(item_init_size) = params.pop().unwrap() {
                let item_init_value = params.pop().unwrap();

                let mut array_items = Vec::new();
                for _ in 0..item_init_size {
                    array_items.push(item_init_value.clone());
                }
                return RuntimeValue::ArrayRef(rt.allocate_heap(RuntimeValue::Array(array_items)));
            }
        } else if str_begin_with(fn_name, "?new_array_") {  // ?new_array_xxx() -> [xxx]
            return RuntimeValue::ArrayRef(rt.allocate_heap(RuntimeValue::Array(Vec::new())));
        } else if str_begin_with(fn_name, "set_item") {
            let mut temp_id_str = String::new();
            for i in 8..fn_name.len() {
                temp_id_str.push(fn_name.chars().nth(i).unwrap());
            }
            let item_id = usize::from_str(&temp_id_str).unwrap();
            if params.len() == 2 {
                let new_value = params[1].clone();
                match &mut params[0] {
                    &mut RuntimeValue::Tuple(ref mut items) => { items[item_id] = new_value; return RuntimeValue::Unit; }
                    _ => unreachable!()
                }
            } else {
                unreachable!()
            }
        }
    }

    // Builtin template type instance
    if let Some(real_typeid) = prep_param_type(&param_types, 0) {
        match types.get_by_idx(real_typeid) {
            &Type::Base(_) => unreachable!(),
            &Type::Array(item_real_typeid) => {
                match (&fn_name, 
                    prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param_type(&param_types, 2), 
                    prep_param(&params, 0), prep_param(&params, 1), prep_param(&params, 2)) {

                    (&FnName::Operator(SeperatorKind::Equal), _, _, _, Some(&RuntimeValue::ArrayRef(heap_index1)), Some(&RuntimeValue::ArrayRef(heap_index2)), None) => {
                        let (array1_clone, array2_clone) = match (&rt.heap[heap_index1], &rt.heap[heap_index2]) {
                            (&RuntimeValue::Array(ref array1), &RuntimeValue::Array(ref array2)) => (array1.clone(), array2.clone()),
                            _ => unreachable!(),
                        };

                        if array1_clone.len() != array2_clone.len() {
                            return RuntimeValue::Bool(false);
                        } else {
                            let mut ret_val = true;
                            for i in 0..array1_clone.len() {
                                ret_val = ret_val && (dispatch_builtin((FnName::Operator(SeperatorKind::Equal), vec![ItemID::new(item_real_typeid), ItemID::new(item_real_typeid)]), vec![array1_clone[i].clone(), array2_clone[i].clone()], types, rt) == true);
                            }
                            return RuntimeValue::Bool(ret_val);
                        }
                    }
                    (&FnName::Operator(SeperatorKind::NotEqual), _, _, _, Some(&RuntimeValue::ArrayRef(heap_index1)), Some(&RuntimeValue::ArrayRef(heap_index2)), None) => {
                        let (array1_clone, array2_clone) = match (&rt.heap[heap_index1], &rt.heap[heap_index2]) {
                            (&RuntimeValue::Array(ref array1), &RuntimeValue::Array(ref array2)) => (array1.clone(), array2.clone()),
                            _ => unreachable!(),
                        };

                        if array1_clone.len() != array2_clone.len() {
                            return RuntimeValue::Bool(true);
                        } else {
                            let mut ret_val = false;
                            for i in 0..array1_clone.len() {
                                ret_val = ret_val || (dispatch_builtin((FnName::Operator(SeperatorKind::Equal), vec![ItemID::new(item_real_typeid), ItemID::new(item_real_typeid)]), vec![array1_clone[i].clone(), array2_clone[i].clone()], types, rt) == true);
                            }
                            return RuntimeValue::Bool(ret_val);
                        }
                    }

                    (&FnName::Ident(ref ident_name), param_type0, param_type1, param_type2, param0, param1, param2) => match (ident_name.as_ref(), param_type0, param_type1, param_type2, param0, param1, param2) {
                        ("set_index", _, Some(5), _, Some(&RuntimeValue::ArrayRef(heap_index)), Some(&RuntimeValue::Int(array_index)), Some(value)) 
                        | ("set_index", _, Some(8), _, Some(&RuntimeValue::ArrayRef(heap_index)), Some(&RuntimeValue::Int(array_index)), Some(value)) => {
                            match &mut rt.heap[heap_index as usize] {
                                &mut RuntimeValue::Array(ref mut array) => { array[array_index as usize] = value.clone(); return RuntimeValue::Unit; }
                                _ => unreachable!()
                            }
                        }
                        ("get_index", _, Some(5), None, Some(&RuntimeValue::ArrayRef(heap_index)), Some(&RuntimeValue::Int(array_index)), None)
                        | ("get_index", _, Some(8), None, Some(&RuntimeValue::ArrayRef(heap_index)), Some(&RuntimeValue::Int(array_index)), None) => {
                            match &mut rt.heap[heap_index as usize] {
                                &mut RuntimeValue::Array(ref mut array) => return array[array_index as usize].clone(),
                                _ => unreachable!()
                            }
                        }
                        ("push", _, _, None, Some(&RuntimeValue::ArrayRef(heap_index)), Some(value), None) => {
                            match &mut rt.heap[heap_index as usize] {
                                &mut RuntimeValue::Array(ref mut array) => { array.push(value.clone()); return RuntimeValue::Unit; }
                                _ => unreachable!()
                            }
                        }
                        ("pop", _, None, None, Some(&RuntimeValue::ArrayRef(heap_index)), None, None) => {
                            match &mut rt.heap[heap_index as usize] {
                                &mut RuntimeValue::Array(ref mut array) => return array.pop().unwrap(),
                                _ => unreachable!()
                            }
                        }
                        ("length", _, None, None, Some(&RuntimeValue::ArrayRef(heap_index)), None, None) => {
                            match &mut rt.heap[heap_index as usize] {
                                &mut RuntimeValue::Array(ref mut array) => return RuntimeValue::Int(array.len() as u64),
                                _ => unreachable!()
                            }
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            &Type::Tuple(ref item_real_typeids) => {
                match (&fn_name, prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param(&params, 0), prep_param(&params, 1)) {
                    (&FnName::Operator(SeperatorKind::Equal), _, _, Some(&RuntimeValue::Tuple(ref values1)), Some(&RuntimeValue::Tuple(ref values2))) => {
                        let mut ret_val = true;
                        for i in 0..values1.len() {
                            ret_val = ret_val && (dispatch_builtin((FnName::Operator(SeperatorKind::Equal), vec![ItemID::new(item_real_typeids[i]), ItemID::new(item_real_typeids[i])]), vec![values1[i].clone(), values2[i].clone()], types, rt) == true);
                        }
                        return RuntimeValue::Bool(ret_val);
                    }
                    (&FnName::Operator(SeperatorKind::NotEqual), _, _, Some(&RuntimeValue::Tuple(ref values1)), Some(&RuntimeValue::Tuple(ref values2))) => {
                        let mut ret_val = false;
                        for i in 0..values1.len() {
                            ret_val = ret_val || (dispatch_builtin((FnName::Operator(SeperatorKind::Equal), vec![ItemID::new(item_real_typeids[i]), ItemID::new(item_real_typeids[i])]), vec![values1[i].clone(), values2[i].clone()], types, rt) == true);
                        }
                        return RuntimeValue::Bool(ret_val);
                    }
                    _ => unreachable!()
                }
            }
        }
    }

    unreachable!()
}