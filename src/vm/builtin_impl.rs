
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

// Based on the assumption that runtime will not meet invalid, return ItemID::Invalid for index out of bound
fn prep_param_type(typeids: &Vec<ItemID>, index: usize) -> Option<usize> {
    if index >= typeids.len() {
        None
    } else {
        Some(typeids[index].as_option().unwrap()) // assume unwrap able
    }
}
fn prep_param(params: &Vec<Operand>, index: usize) -> Option<&Operand> {
    if index >= params.len() {
        None
    } else {
        Some(&params[index])
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

pub fn dispatch_builtin(fn_sign: (FnName, Vec<ItemID>), params: &Vec<Operand>, types: &TypeCollection, rt: &mut Runtime) -> RuntimeValue {
    use super::runtime::RuntimeValue::*;

    let fn_name = fn_sign.0;
    let param_types = fn_sign.1;

    macro_rules! native_forward_2 {
        ($param0: expr, $param1: expr, $pat: pat => $stmt: block) => (match (rt.index($param0), rt.index($param1)) {
            $pat => $stmt,
            _ => unreachable!()
        })
    }
    macro_rules! native_forward_1 {
        ($param0: expr, $pat: pat => $stmt: block) => (match rt.index($param0) {
            $pat => $stmt
            _ => unreachable!()
        })
    }
    macro_rules! native_forward_mut_1 {
        ($param0: expr, $pat: pat => $stmt: block) => (match rt.index_mut($param0) {
            $pat => $stmt
            _ => unreachable!()
        })
    }

    // Global
    match (&fn_name, prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param(&params, 0), prep_param(&params, 1)) {
        (&FnName::Ident(ref ident_name), param_type0, param_type1, param0, param1) => match (ident_name.as_ref(), param_type0, param_type1, param0, param1) {
            ("write", Some(13), None, Some(ref operand), None) => { print!("{}", rt.index(operand).get_str_lit()); return RuntimeValue::Unit; }
            ("writeln", Some(13), None, Some(ref operand), None) => { println!("{}", rt.index(operand).get_str_lit()); return RuntimeValue::Unit; }
            ("read_i32", None, None, None, None) => return RuntimeValue::Int(read_int_from_stdin::<i32>() as u64),
            ("read_u64", None, None, None, None) => return RuntimeValue::Int(read_int_from_stdin::<u64>()), 
            _ => (),
        },
        _ => (),
    }

    // Builtin type
    match (&fn_name, prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param(&params, 0), prep_param(&params, 1)) {

        // Integral Add
        (&FnName::Operator(SeperatorKind::Add), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Add), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 + value1); } },

        // Integral Sub
        (&FnName::Operator(SeperatorKind::Sub), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Sub), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 - value1); } },

        // Integral Mul
        (&FnName::Operator(SeperatorKind::Mul), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Mul), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 * value1); } },

        // Integral Div
        (&FnName::Operator(SeperatorKind::Div), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Div), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(8), Some(8), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 / value1); } },

        // Integral Rem
        (&FnName::Operator(SeperatorKind::Rem), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Rem), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Rem), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Rem), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Rem), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Rem), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Rem), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Rem), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 % value1); } },

        // Integral ShiftLeft
        (&FnName::Operator(SeperatorKind::ShiftLeft), Some(1), Some(5), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(2), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(3), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(4), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(6), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(7), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftLeft), Some(8), Some(5), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 << value1); } },

        // Integral ShiftRight
        (&FnName::Operator(SeperatorKind::ShiftRight), Some(1), Some(5), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(2), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(3), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(4), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(6), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(7), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::ShiftRight), Some(8), Some(5), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 >> value1); } },

        // Integral BitAnd
        (&FnName::Operator(SeperatorKind::BitAnd), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitAnd), Some(8), Some(8), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 & value1); } },

        // Integral BitOr
        (&FnName::Operator(SeperatorKind::BitOr), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::BitOr), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitOr), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitOr), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitOr), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitOr), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitOr), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitOr), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 | value1); } },

        // Integral BitXor
        (&FnName::Operator(SeperatorKind::BitXor), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::BitXor), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitXor), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitXor), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitXor), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitXor), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitXor), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::BitXor), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Int(value0 ^ value1); } },


        // Integral Equal
        (&FnName::Operator(SeperatorKind::Equal), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Equal), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Bool(value0 == value1); } },

        // Integral NotEqual
        (&FnName::Operator(SeperatorKind::NotEqual), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(8), Some(8), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Bool(value0 != value1); } },

        // Integral GreatEqual
        (&FnName::Operator(SeperatorKind::GreatEqual), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(8), Some(8), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Bool(value0 >= value1); } },

        // Integral LessEqual
        (&FnName::Operator(SeperatorKind::LessEqual), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(8), Some(8), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Bool(value0 <= value1); } },

        // Integral Great
        (&FnName::Operator(SeperatorKind::Great), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Great), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(8), Some(8), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Bool(value0 > value1); } },

        // Integral Less
        (&FnName::Operator(SeperatorKind::Less), Some(1), Some(1), Some(ref param0), Some(ref param1)) 
        | (&FnName::Operator(SeperatorKind::Less), Some(2), Some(2), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(3), Some(3), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(4), Some(4), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(5), Some(5), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(6), Some(6), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(7), Some(7), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(8), Some(8), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Int(value0), Int(value1)) => { return Bool(value0 < value1); } },

        // Integral BitNot
        (&FnName::Operator(SeperatorKind::BitNot), Some(1), None, Some(ref param0), None) 
        | (&FnName::Operator(SeperatorKind::BitNot), Some(2), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(3), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(4), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(5), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(6), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(7), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::BitNot), Some(8), None, Some(ref param0), None) 
        => native_forward_1!{ param0, Int(value) => { return Int(!value); } },

        // Integral Increase
        (&FnName::Operator(SeperatorKind::Increase), Some(1), None, Some(ref param0), None) 
        | (&FnName::Operator(SeperatorKind::Increase), Some(2), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Increase), Some(3), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Increase), Some(4), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Increase), Some(5), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Increase), Some(6), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Increase), Some(7), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Increase), Some(8), None, Some(ref param0), None) 
        => native_forward_mut_1!{ param0, &mut Int(ref mut value) => { *value += 1; return Unit; } },

        // Integral Decrease
        (&FnName::Operator(SeperatorKind::Decrease), Some(1), None, Some(ref param0), None) 
        | (&FnName::Operator(SeperatorKind::Decrease), Some(2), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(3), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(4), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(5), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(6), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(7), None, Some(ref param0), None)
        | (&FnName::Operator(SeperatorKind::Decrease), Some(8), None, Some(ref param0), None)
        => native_forward_mut_1!{ param0, &mut Int(ref mut value) => { *value -= 1; return Unit; } },

        // Integral Cast to integral
        (&FnName::Cast(1), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(1), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(2), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(3), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(4), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(5), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(6), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(7), Some(8), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(8), None, Some(ref param0), None)
        => native_forward_1!{ param0, Int(value) => { return Int(value); } },

        // Integral cast to float
        (&FnName::Cast(10), Some(1), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(2), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(3), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(4), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(5), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(6), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(7), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(8), None, Some(ref param0), None)
        => native_forward_1!{ param0, Int(value) => { return Float(value as f64); } },
        
    
        // Float Add
        (&FnName::Operator(SeperatorKind::Add), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Add), Some(10), Some(10), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Float(value0 + value1); } },
        (&FnName::Operator(SeperatorKind::Sub), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Sub), Some(10), Some(10), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Float(value0 - value1); } },
        (&FnName::Operator(SeperatorKind::Mul), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Mul), Some(10), Some(10), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Float(value0 * value1); } },
        (&FnName::Operator(SeperatorKind::Div), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Div), Some(10), Some(10), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Float(value0 / value1); } },

        // Float Compare
        (&FnName::Operator(SeperatorKind::Equal), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Equal), Some(10), Some(10), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Bool(value0 == value1); } },
        (&FnName::Operator(SeperatorKind::NotEqual), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::NotEqual), Some(10), Some(10), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Bool(value0 != value1); } },
        (&FnName::Operator(SeperatorKind::GreatEqual), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::GreatEqual), Some(10), Some(10), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Bool(value0 >= value1); } },
        (&FnName::Operator(SeperatorKind::LessEqual), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::LessEqual), Some(10), Some(10), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Bool(value0 <= value1); } },
        (&FnName::Operator(SeperatorKind::Great), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Great), Some(10), Some(10), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Bool(value0 > value1); } },
        (&FnName::Operator(SeperatorKind::Less), Some(9), Some(9), Some(ref param0), Some(ref param1))
        | (&FnName::Operator(SeperatorKind::Less), Some(10), Some(10), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Float(value0), Float(value1)) => { return Bool(value0 < value1); } },

        // Float cast to float
        (&FnName::Cast(9), Some(9), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(9), None, Some(ref param0), None) 
        | (&FnName::Cast(9), Some(10), None, Some(ref param0), None) 
        | (&FnName::Cast(10), Some(10), None, Some(ref param0), None)
        => native_forward_1!{ param0, Float(value) => { return Float(value); } },
        // Float cast to integral
        (&FnName::Cast(8), Some(9), None, Some(ref param0), None) 
        | (&FnName::Cast(8), Some(10), None, Some(ref param0), None)
        => native_forward_1!{ param0, Float(value) => { return Int(value as u64); } },

        // Char Compare
        (&FnName::Operator(SeperatorKind::Equal), Some(11), Some(11), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Char(value0), Char(value1)) => { return Bool(value0 == value1); } },
        (&FnName::Operator(SeperatorKind::NotEqual), Some(11), Some(11), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Char(value0), Char(value1)) => { return Bool(value0 != value1); } },
        (&FnName::Operator(SeperatorKind::GreatEqual), Some(11), Some(11), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Char(value0), Char(value1)) => { return Bool(value0 >= value1); } },
        (&FnName::Operator(SeperatorKind::LessEqual), Some(11), Some(11), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Char(value0), Char(value1)) => { return Bool(value0 <= value1); } },
        (&FnName::Operator(SeperatorKind::Great), Some(11), Some(11), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Char(value0), Char(value1)) => { return Bool(value0 > value1); } },
        (&FnName::Operator(SeperatorKind::Less), Some(11), Some(11), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Char(value0), Char(value1)) => { return Bool(value0 < value1); } },

        // Char cast
        (&FnName::Cast(6), Some(11), None, Some(ref param0), None) 
        => native_forward_1!{ param0, Char(value) => { return RuntimeValue::Int(value as u64); } },

        // Bool
        (&FnName::Operator(SeperatorKind::Equal), Some(12), Some(12), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Bool(value0), Bool(value1)) => { return Bool(value0 == value1); } },
        (&FnName::Operator(SeperatorKind::NotEqual), Some(12), Some(12), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Bool(value0), Bool(value1)) => { return Bool(value0 != value1); } },
        (&FnName::Operator(SeperatorKind::LogicalAnd), Some(12), Some(12), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Bool(value0), Bool(value1)) => { return Bool(value0 && value1); } },
        (&FnName::Operator(SeperatorKind::LogicalOr), Some(12), Some(12), Some(ref param0), Some(ref param1)) 
        => native_forward_2!{ param0, param1, (Bool(value0), Bool(value1)) => { return Bool(value0 || value1); } },
        (&FnName::Operator(SeperatorKind::LogicalNot), Some(12), None, Some(ref param0), None) 
        => native_forward_1!{ param0, Bool(value) => { return Bool(!value); } },

        // Str
        (&FnName::Operator(SeperatorKind::Add), Some(13), Some(13), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Str(value0), Str(value1)) => { return Str(value0 + &value1); } },
        (&FnName::Operator(SeperatorKind::Equal), Some(13), Some(13), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Str(value0), Str(value1)) => { return Bool(value0 == value1); } },
        (&FnName::Operator(SeperatorKind::NotEqual), Some(13), Some(13), Some(ref param0), Some(ref param1))
        => native_forward_2!{ param0, param1, (Str(value0), Str(value1)) => { return Bool(value0 != value1); } },

        // Special ident dispatcher
        (&FnName::Ident(ref ident_name), param_type0, param_type1, param0, param1) => match (ident_name.as_ref(), param_type0, param_type1, param0, param1) {

            // Integral is_odd
            ("is_odd", Some(1), None, Some(ref param0), None) 
            | ("is_odd", Some(2), None, Some(ref param0), None) 
            | ("is_odd", Some(3), None, Some(ref param0), None) 
            | ("is_odd", Some(4), None, Some(ref param0), None) 
            | ("is_odd", Some(5), None, Some(ref param0), None) 
            | ("is_odd", Some(6), None, Some(ref param0), None) 
            | ("is_odd", Some(7), None, Some(ref param0), None) 
            | ("is_odd", Some(8), None, Some(ref param0), None) 
            => native_forward_1!{ param0, Int(value) => { return Bool((value & 1) == 0); } },
            
            // Integral to_string
            ("to_string", Some(1), None, Some(ref param0), None) 
            | ("to_string", Some(2), None, Some(ref param0), None) 
            | ("to_string", Some(3), None, Some(ref param0), None) 
            | ("to_string", Some(4), None, Some(ref param0), None) 
            | ("to_string", Some(5), None, Some(ref param0), None) 
            | ("to_string", Some(6), None, Some(ref param0), None) 
            | ("to_string", Some(7), None, Some(ref param0), None) 
            | ("to_string", Some(8), None, Some(ref param0), None)
            => native_forward_1!{ param0, Int(value) => { return Str(format!("{}", value)); } },

            // Float to_string
            ("to_string", Some(9), None, Some(ref param0), None) 
            | ("to_string", Some(10), None, Some(ref param0), None) 
            => native_forward_1!{ param0, Float(value) => { return Str(format!("{}", value)); } },

            // Char to_string
            ("to_string", Some(11), None, Some(ref param0), None)
            => native_forward_1!{ param0, Char(value) => { return Str(format!("{}", value)); } },

            // String
            ("length", Some(13), None, Some(ref param0), None)
            => native_forward_1!{ param0, Str(value) => { return Int(value.len() as u64); } },
            ("get_index", Some(13), Some(5), Some(ref param0), Some(ref param1))
            => native_forward_2!{ param0, param1, (Str(value0), Int(value1)) => { return Char(value0.chars().nth(value1 as usize).unwrap()); } },
            ("get_index", Some(13), Some(8), Some(ref param0), Some(ref param1))
            => native_forward_2!{ param0, param1, (Str(value0), Int(value1)) => { return Char(value0.chars().nth(value1 as usize).unwrap()); } },

            (_, _, _, _, _) => (),
        },

        (_, _, _, _, _) => (),
    }

    // Builtin template type global
    if let &FnName::Ident(ref fn_name) = &fn_name {

        if fn_name == "?new_tuple" {                        // ?new_tuple(aaa, bbb, ccc) -> (aaa, bbb, ccc)
            let mut rt_values = Vec::new();
            for param in params {
                rt_values.push(rt.index(param));
            }
            return RuntimeValue::Tuple(rt_values); 

        } else if fn_name == "?new_array" {                 // ?new_array(value, size) -> [value]
            // Currently 5 and 8 are all 8 at runtime, no more need to check
            if let RuntimeValue::Int(item_init_size) = rt.index(&params[1]) {
                let item_init_value = rt.index(&params[0]);

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
                let new_value = rt.index(&params[1]);
                match rt.index_mut(&params[0]) {
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
            &Type::Array(_item_real_typeid) => {
                match (&fn_name, 
                    prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param_type(&param_types, 2), 
                    prep_param(&params, 0), prep_param(&params, 1), prep_param(&params, 2)) {

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
                        ("set_index", _, Some(5), _, Some(ref param0), Some(ref param1), Some(ref param2)) 
                        | ("set_index", _, Some(8), _, Some(ref param0), Some(ref param1), Some(ref param2)) => {
                            match (rt.index(param0), rt.index(param1), rt.index(param2)) {
                                (RuntimeValue::ArrayRef(heap_index), RuntimeValue::Int(array_index), new_value) => {
                                    match &mut rt.heap[heap_index as usize] {
                                        &mut RuntimeValue::Array(ref mut array) => { array[array_index as usize] = new_value.clone(); return RuntimeValue::Unit; }
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        ("get_index", _, Some(5), None, Some(ref param0), Some(ref param1), None)
                        | ("get_index", _, Some(8), None, Some(ref param0), Some(ref param1), None) => {
                            match (rt.index(param0), rt.index(param1)) {
                                (RuntimeValue::ArrayRef(heap_index), RuntimeValue::Int(array_index)) => {
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
                                        &mut RuntimeValue::Array(ref mut array) => return RuntimeValue::Int(array.len() as u64),
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
                match (&fn_name, prep_param_type(&param_types, 0), prep_param_type(&param_types, 1), prep_param(&params, 0), prep_param(&params, 1)) {
                    (&FnName::Operator(SeperatorKind::Equal), _, _, Some(ref param0), Some(ref param1)) => {
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
                    (&FnName::Operator(SeperatorKind::NotEqual), _, _, Some(ref param0), Some(ref param1)) => {
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