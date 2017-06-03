//! fff-lang
//!
//! Every kind of messages

#[cfg_attr(test, macro_use)] extern crate codemap;
extern crate util;

use std::fmt;
use codemap::Span;
use util::format_vector_debug;

#[derive(Eq, PartialEq)]
pub enum CodegenMessage {
    FunctionHasSameName{ 
        name: String, 
        poss: Vec<Span>,
    },
    TypeNotExist{ 
        name: String, 
        pos: Span 
    },
    FunctionArgumentNameConfilict{ 
        func_pos: Span,  // position of the fn 
        func_name: String,        
        name: String,          
        pos1: Span,      // first def of the name
        pos2: Span,      // more def of the name
    },
    VariableDefined{
        name: String,
        predef: Span,    // Previous VarDeclStmt
        curdef: Span,    // Current VarDeclStmt, compiler generated will not collision
    },
    FunctionRedefinition{
        sign: String,
        fnposs: Vec<Span>,
    },

    ConstVariableDeclareMissingInitializationExpression{
        name: String,
        pos: Span,
    },

    FunctionCallOperatorNotAppliedToIdentifier{
        pos: Span,
    },

    InvalidExpressionStatementSingleSimpleExpression{
        pos: Span,
    },
    InvalidExpressionStatementLastOpNotValid{
        pos: Span,
    },
    LeftOfAssignmentStatementCannotBeComplex{
        pos: Span,
    },
    AssignToConstVar{
        name: String,
        pos: Span, // stmt pos
    },
    MemberAccessNotSupportedCurrently{
        pos: Span,
    },

    CannotFindLoopName{
        name: String, 
        pos: Span,
    },
    JumpStatementNotInLoop{
        pos: Span
    },

    ArrayInitializeElementNotSameType{
        expect: String,
        actual: String,
        pos: Span,
    },
    ArrayDupDefSecondParameterTypeNotExpected{
        actual: String,
        pos: Span,
    },

    MemberNotExist{
        prev_expr_pos: Span, // previous part position
        prev_type_desc: String,        // previous part type display name
        op_pos: Span,        // this op pos
        member_name: String,           // member name
    },

    IdentNotDeclared{
        name: String,
        pos: Span,
    },
    FunctionNotDefined{
        sign: String,
        pos: Span,
    },
    AssignmentTypeMismatch{
        left_desc: String,
        right_desc: String,
        pos: Span,
    },
    IfConditionNotBoolType{
        pos: Span,
        actual: String,
    },
    ReturnTypeMismatch{
        pos: Span,
        expect: String,
        actual: String,
    },
    ForIteraterTypeMismatch{
        pos: Span,
        actual: String,
    },
    ForRangeTypeMismatch{
        pos: Span,
        expect: String,
        actual: String,
    }
}
impl fmt::Debug for CodegenMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CodegenMessage::*;
        match *self {
            FunctionHasSameName{ ref name, ref poss } => {
                write!(f, "Function with name `{}` are defined mutliple time at {}", name, format_vector_debug(poss, ", "))
            },
            TypeNotExist{ ref name, ref pos } => {
                write!(f, "Type {} at {:?} not defined or not in scope", name, pos)
            },
            FunctionArgumentNameConfilict{ ref func_pos, ref func_name, ref name, ref pos1, ref pos2 } => {
                write!(f, "Parameter {} redifinition at {:?}, whose previous def at {:?} in function {} at {:?}",
                    name, pos2, pos1, func_name, func_pos
                )
            },
            VariableDefined{ ref name, ref predef, ref curdef } => {
                write!(f, "Variable {} already defined at {:?} but redefiened at {:?}", name, predef, curdef)
            },
            FunctionRedefinition{ ref sign, ref fnposs } => {
                write!(f, "Function with signature `{}` redefined at {}", sign, format_vector_debug(fnposs, ", "))
            },
            ConstVariableDeclareMissingInitializationExpression{ ref name, ref pos } => {
                write!(f, "Variable declaration of {} at {:?} is const but missing init expression", name, pos)
            },
            FunctionCallOperatorNotAppliedToIdentifier{ ref pos } => {
                write!(f, "Function call operator not applied to identifier at {:?}", pos)
            },
            InvalidExpressionStatementSingleSimpleExpression{ ref pos } => {
                write!(f, "Invalid expression statement at {:?}, simple expression cannot be statement", pos)
            },
            InvalidExpressionStatementLastOpNotValid{ ref pos } => {
                write!(f, "Invalid expression statement at {:?}, expression statement should be at last a function call statement or increase or decrease expression", pos)
            },
            LeftOfAssignmentStatementCannotBeComplex{ ref pos } => {
                write!(f, "Left expression at {:?} of assignment statement can only be an identifier", pos)
            },
            AssignToConstVar{ ref name, ref pos } => {
                write!(f, "Try to assign to const variable {:?} at {:?}", name, pos)
            },
            MemberAccessNotSupportedCurrently{ ref pos } => {
                write!(f, "Member access not supported **CURRENTLY** at {:?}", pos)
            },
            CannotFindLoopName{ ref name, ref pos } => {
                write!(f, "Cannot find loop name `{}` in jump statement at {:?}", name, pos)
            },
            JumpStatementNotInLoop{ ref pos } => {
                write!(f, "Jump statement at {:?} not in any loop", pos)
            },
            ArrayInitializeElementNotSameType{ ref expect, ref actual, ref pos } => {
                write!(f, "Unexpected type of initialize element in array at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
            ArrayDupDefSecondParameterTypeNotExpected{ ref actual, ref pos } => {
                write!(f, "Second parameter of array dup def at {:?} type miss match, expect i32 or u64, acutal is {}", pos, actual)
            },
            MemberNotExist{ ref prev_expr_pos, ref prev_type_desc, ref op_pos, ref member_name } => {
                write!(f, "Member {} at {:?} not exist for expression at {:?} with type {}", member_name, op_pos, prev_expr_pos, prev_type_desc)
            },
            IdentNotDeclared{ ref name, ref pos } => {
                write!(f, "Identifier `{}` not defined at {:?}", name, pos)
            },

            FunctionNotDefined{ ref sign, ref pos } => {
                write!(f, "Function with signature {} not defined at scope of the expression at {:?}", sign, pos)
            },
            AssignmentTypeMismatch{ ref left_desc, ref right_desc, ref pos } => {
                write!(f, "Assignment type mismatch at {:?}, left is `{}`, right is `{}`", pos, left_desc, right_desc)
            },
            IfConditionNotBoolType{ ref pos, ref actual } => {
                write!(f, "Expect if condition to be bool type, but actually is {} at {:?}", actual, pos)
            },
            ReturnTypeMismatch{ ref pos, ref actual, ref expect } => {
                write!(f, "Return type mismatch at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
            ForIteraterTypeMismatch{ ref pos, ref actual } => {
                write!(f, "**CURRENTLY** if iterater only supports integral type, meet {} at {:?}", actual, pos)
            },
            ForRangeTypeMismatch{ ref pos, ref actual, ref expect } => {
                write!(f, "For range type mismatch at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum LegacyMessage {
    Codegen(CodegenMessage),
    New(Message),
}
impl LegacyMessage {
    fn is_new(&self) -> bool {
        match self {
            &LegacyMessage::New(_) => true,
            _ => false,
        }
    }
}
impl From<CodegenMessage> for LegacyMessage {
    fn from(msg: CodegenMessage) -> LegacyMessage { LegacyMessage::Codegen(msg) }   
}

impl fmt::Debug for LegacyMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LegacyMessage::Codegen(ref msg) => write!(f, "{:?}", msg),
            LegacyMessage::New(ref msg) => write!(f, "{:?}", msg),
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct PosAndDesc {
    pub pos: Span, 
    pub desc: String,
}
impl From<(Span, String)> for PosAndDesc {
    fn from(pos_and_desc: (Span, String)) -> PosAndDesc {
        PosAndDesc{ pos: pos_and_desc.0, desc: pos_and_desc.1 }
    }
}
impl fmt::Debug for PosAndDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "At {:?}: {}", self.pos, self.desc)
    }
}

#[derive(Eq, PartialEq)]
pub struct Message {
    pub main_desc: String, 
    pub pos_and_descs: Vec<PosAndDesc>,
    pub helps: Vec<String>,
}
impl Message {    
    pub fn new(main_desc: String, pos_and_descs: Vec<(Span, String)>) -> Message {
        Message{ 
            main_desc: main_desc, 
            pos_and_descs: pos_and_descs.into_iter().map(|pos_and_desc| pos_and_desc.into()).collect(), 
            helps: Vec::new() 
        }
    }
    pub fn new_by_str(main_desc: &str, pos_and_descs: Vec<(Span, &str)>) -> Message {
        Message{ 
            main_desc: main_desc.to_owned(), 
            pos_and_descs: pos_and_descs.into_iter().map(|pos_and_desc| match pos_and_desc { (pos, desc) => (pos, desc.to_owned()).into() }).collect(),
            helps: Vec::new(),
        }
    }
    pub fn with_help(main_desc: String, pos_and_descs: Vec<(Span, String)>, helps: Vec<String>) -> Message {
        Message{ 
            main_desc: main_desc, 
            pos_and_descs: pos_and_descs.into_iter().map(|pos_and_desc| pos_and_desc.into()).collect(), 
            helps: helps 
        }
    }
    pub fn with_help_by_str(main_desc: &str, pos_and_descs: Vec<(Span, &str)>, helps: Vec<&str>) -> Message {
        Message{ 
            main_desc: main_desc.to_owned(), 
            pos_and_descs: pos_and_descs.into_iter().map(|pos_and_desc| match pos_and_desc { (pos, desc) => (pos, desc.to_owned()).into() }).collect(),
            helps: helps.into_iter().map(|help| help.to_owned()).collect(),
        }
    }
    pub fn new_simple(main_desc: &str) -> Message {
        Message{ main_desc: main_desc.to_owned(), pos_and_descs: Vec::new(), helps: Vec::new() }
    }
}
impl From<Message> for LegacyMessage {
    fn from(msg: Message) -> LegacyMessage { LegacyMessage::New(msg) }
}
impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use util;
        
        write!(f, "{}:\n", self.main_desc)?;
        if !self.pos_and_descs.is_empty() {
            write!(f, "    | {}", util::format_vector_debug(&self.pos_and_descs, "\n    | "))?;
        }
        if !self.helps.is_empty() {
            write!(f, "\n    = {}", util::format_vector_display(&self.helps, "\n    = "))?;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq)]
pub struct MessageCollection {
    messages: Vec<LegacyMessage>,
    m_uncontinuable: bool,
}
impl fmt::Debug for MessageCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for message in &self.messages {
            try!(writeln!(f, "{:?}\n", message));
        }
        Ok(())
    }
}
impl MessageCollection {

    pub fn new() -> MessageCollection {
        MessageCollection { 
            messages: Vec::new(),
            m_uncontinuable: false,
        }
    }

    pub fn is_empty(&self) -> bool { self.messages.is_empty() }

    pub fn push<T: Into<LegacyMessage>>(&mut self, message: T) {
        let legacy = message.into();
        if !legacy.is_new() {
            // panic!("Message is not new"); // magic to make lots of tests to fail
        }
        
        // Not repeat report error
        // Used in lexical/v2lexer/char::pass_non_ascii_char
        if self.messages.len() == 0 || (self.messages.len() > 0 && legacy != self.messages[self.messages.len() - 1]) {
            self.messages.push(legacy);
        }
    }

    pub fn pop(&mut self) { // Pop top
        let _ = self.messages.pop();
    }

    pub fn set_uncontinuable(&mut self) { self.m_uncontinuable = true; }
    pub fn is_uncontinuable(&self) -> bool { self.m_uncontinuable }
}

#[macro_export]
macro_rules! check_messages_continuable {
    ($msgs: expr) => (if $msgs.is_uncontinuable() { panic!("messages is uncontinuable: {:?}", $msgs) })
}

#[macro_export]
macro_rules! make_messages {
    ($($x:expr),*) => ({
        let mut retval = MessageCollection::new();
        {
            let _retval = &mut retval; // `&mut` for statisfy 'unused mut', `_` for statisfy unused var
            $(
                _retval.push($x);
            )*
        }
        retval
    });
    ($($x:expr,)*) => (make_messages![$($x),*])
}

#[cfg(test)] #[test]
fn message_complex_new() {

    assert_eq!(
        Message::new_by_str("123", vec![
            (Span::default(), "456"),
            (Span::default(), "789"),
        ]), 
        Message::new("123".to_owned(), vec![
            (Span::default(), "456".to_owned()),
            (Span::default(), "789".to_owned()),
        ])
    );
}

#[cfg(test)] #[test]
fn message_by_macro() {

    let mut messages = MessageCollection::new();
    assert_eq!(messages, make_messages![]);

    messages.push(Message::new_by_str("a", vec![(make_span!(1, 1), "b")]));
    assert_eq!(messages, make_messages![Message::new_by_str("a", vec![(make_span!(1, 1), "b")])]);
    assert_eq!(messages, make_messages![Message::new_by_str("a", vec![(make_span!(1, 1), "b")]), ]);

    messages.push(Message::new_by_str("c", vec![(make_span!(2, 3), "d")]));
    messages.push(Message::new_by_str("e", vec![(make_span!(2, 8), "f")]));
    assert_eq!(messages, make_messages![
        Message::new_by_str("a", vec![(make_span!(1, 1), "b")]), 
        Message::new_by_str("c", vec![(make_span!(2, 3), "d")]),
        Message::new_by_str("e", vec![(make_span!(2, 8), "f")])
    ]);
    assert_eq!(messages, make_messages![
        Message::new_by_str("a", vec![(make_span!(1, 1), "b")]), 
        Message::new_by_str("c", vec![(make_span!(2, 3), "d")]),
        Message::new_by_str("e", vec![(make_span!(2, 8), "f")]),
    ]);
}